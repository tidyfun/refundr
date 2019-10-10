#' Functional principal component analysis with fast covariance estimation
#'
#' A fast implementation of the sandwich smoother (Xiao et al., 2013)
#' for covariance matrix smoothing. Pooled generalized cross validation
#' at the data level is used for selecting the smoothing parameter.
#' @param data, the user must supply `data`, a matrix of functions
#' observed on a regular grid
#' @param Y.pred if desired, a matrix of functions to be approximated using
#' the FPC decomposition.
#' @param argvals numeric; function argument.
#' @param pve proportion of variance explained: used to choose the number of
#' principal components.
#' @param npc how many smooth SVs to try to extract, if `NA` (the
#' default) the hard thresholding rule of Gavish and Donoho (2014) is used (see
#' Details, References).
#' @param center logical; center `data` so that its column-means are 0? Defaults to
#' `TRUE`
#' @param p integer; the degree of B-splines functions to use
#' @param m integer; the order of difference penalty to use
#' @param knots number of knots to use or the vectors of knots; defaults to 35
#' @param lambda smoothing parameter; if not specified smoothing parameter is
#' chosen using [stats::optim()] or a grid search
#' @param alpha numeric; tuning parameter for GCV; see parameter `gamma`
#' in [mgcv::gam()]
## @param maxiter how many iterations of the power algorithm to perform at
## most (defaults to 15)
## @param tol convergence tolerance for power algorithm (defaults to 1e-4)
## @param diffpen difference penalty order controlling the desired smoothness
## of the right singular vectors, defaults to 3 (i.e., deviations from local
## quadratic polynomials).
## @param gridsearch use \code{\link[stats]{optimize}} or a grid search to
## find GCV-optimal smoothing parameters? defaults to \code{TRUE}.
## @param alphagrid grid of smoothing parameter values for grid search
## @param lower.alpha lower limit for for smoothing parameter if
## \code{!gridsearch}
## @param upper.alpha upper limit for smoothing parameter if
## \code{!gridsearch}
## @param verbose generate graphical summary of progress and diagnostic
## messages?  defaults to \code{FALSE}
## @param score.method character; method to use to estimate scores; one of
## \code{"blup"} or \code{"int"} (default)
#' @param search.grid logical; should a grid search be used to find `lambda`?
#'  Otherwise, [stats::optim()] is used
#' @param search.length integer; length of grid to use for grid search for
#' `lambda`; ignored if `search.grid` is `FALSE`
#' @param method method to use; see [stats::optim()]
#' @param lower see [stats::optim()]
#' @param upper see [stats::optim()]
#' @param control see [stats::optim()]
#' @return A list with components
#' \enumerate{
#' \item `Yhat` - If `Y.pred` is specified, the smooth version of
#' `Y.pred`.   Otherwise, if `Y.pred=NULL`, the smooth version of `data`.
#' \item `scores` - matrix of scores
#' \item `mu` - mean function
#' \item `npc` - number of principal components
#' \item `efunctions` - matrix of eigenvectors
#' \item `evalues` - vector of eigenvalues
#' }
#' @author Luo Xiao
#' @references Xiao, L., Li, Y., and Ruppert, D. (2013).
#' Fast bivariate *P*-splines: the sandwich smoother,
#' *Journal of the Royal Statistical Society: Series B*, 75(3), 577-599.
#'
#' Xiao, L., Ruppert, D., Zipunnikov, V., and Crainiceanu, C. (2016).
#' Fast covariance estimation for high-dimensional functional data.
#' *Statistics and Computing*, 26, 409-421.
#' DOI: 10.1007/s11222-014-9485-x.
#' @importFrom stats smooth.spline optim
#' @importFrom Matrix as.matrix
#' @importFrom MASS mvrnorm

fpca_face <-function(data=NULL,Y.pred = NULL,argvals=NULL,pve = 0.99, npc  = NULL,
         center=TRUE,knots=35,p=3,m=2,lambda=NULL,alpha = 1,
         search.grid=TRUE,search.length=100,
         method="L-BFGS-B", lower=-20,upper=20, control=NULL){

  data <- as.matrix(spread(as.data.frame(data), key = arg, value = value)[,-1])

  ## data: data, I by J data matrix, functions on rows
  ## argvals:  vector of J
  ## knots: to specify either the number of knots or the vectors of knots;
  ##        defaults to 35;
  ## p: the degree of B-splines;
  ## m: the order of difference penalty
  ## lambda: user-selected smoothing parameter
  ## method: see R function "optim"
  ## lower, upper, control: see R function "optim"
  #require(Matrix)
  #source("pspline.setting.R")

  ##### redo this so that it still accepts a dataframe (see ydata argument from original code)
  ##### but have this as an S3 class where it can accept either.
  stopifnot(!is.null(data))
  stopifnot(is.matrix(data))
  data_dim <- dim(data)
  I <- data_dim[1] ## number of subjects
  J <- data_dim[2] ## number of obs per function

  if(is.null(argvals))  argvals <- (1:J)/J-1/2/J ## if NULL, assume equally spaced

  meanX <- rep(0,J)
  if(center) {##center the functions
    meanX <- colMeans(data, na.rm=TRUE)
    meanX <- smooth.spline(argvals,meanX,all.knots =TRUE)$y
    data <- t(t(data)- meanX)
  }

  ## specify the B-spline basis: knots
  p.p <- p
  m.p <- m
  if(length(knots)==1){
    if(knots+p.p>=J) cat("Too many knots!\n")
    stopifnot(knots+p.p<J)

    K.p <- knots
    knots <- seq(-p.p,K.p+p.p,length=K.p+1+2*p.p)/K.p
    knots <- knots*(max(argvals)-min(argvals)) + min(argvals)
  }
  if(length(knots)>1) K.p <- length(knots)-2*p.p-1
  if(K.p>=J) cat("Too many knots!\n")
  stopifnot(K.p <J)
  c.p <- K.p + p.p

  ######### precalculation for smoothing #############
  List <- pspline.setting(argvals,knots,p.p,m.p)
  B <- List$B
  Bt <- Matrix(t(as.matrix(B)))
  s <- List$s
  Sigi.sqrt <- List$Sigi.sqrt
  U <- List$U
  A0 <- Sigi.sqrt%*%U
  MM <- function(A,s,option=1){
    if(option==2)
      return(A*(s%*%t(rep(1,dim(A)[2]))))
    if(option==1)
      return(A*(rep(1,dim(A)[1])%*%t(s)))
  }

  ######## precalculation for missing data ########
  imputation <- FALSE
  Niter.miss <- 1

  Index.miss <- is.na(data)
  if(sum(Index.miss)>0){
    num.miss <- rowSums(is.na(data))
    for(i in 1:I){
      if(num.miss[i]>0){
        y <- data[i,]
        seq <- (1:J)[!is.na(y)]
        seq2 <-(1:J)[is.na(y)]
        t1 <- argvals[seq]
        t2 <- argvals[seq2]
        fit <- smooth.spline(t1,y[seq])
        temp <- predict(fit,t2,all.knots=TRUE)$y
        if(max(t2)>max(t1)) temp[t2>max(t1)] <- mean(y[seq])#y[seq[length(seq)]]
        if(min(t2)<min(t1)) temp[t2<min(t1)] <- mean(y[seq])#y[seq[1]]
        data[i,seq2] <- temp
      }
    }
    Y0 <- matrix(NA,c.p,I)
    imputation <- TRUE
    Niter.miss <- 100
  }
  convergence.vector <- rep(0,Niter.miss);
  iter.miss <- 1
  totalmiss <- mean(Index.miss)

  while(iter.miss <= Niter.miss&&convergence.vector[iter.miss]==0) {
    ###################################################
    ######## Transform the Data           #############
    ###################################################
    Ytilde <- as.matrix(t(A0)%*%as.matrix(Bt%*%t(data)))
    C_diag <- rowSums(Ytilde^2)
    if(iter.miss==1) Y0 = Ytilde
    ###################################################
    ########  Select Smoothing Parameters #############
    ###################################################
    Y_square <- sum(data^2)
    Ytilde_square <- sum(Ytilde^2)
    face_gcv <- function(x) {
      lambda <- exp(x)
      lambda_s <- (lambda*s)^2/(1 + lambda*s)^2
      gcv <- sum(C_diag*lambda_s) - Ytilde_square + Y_square
      trace <- sum(1/(1+lambda*s))
      gcv <- gcv/(1-alpha*trace/J/(1-totalmiss))^2
      return(gcv)
    }

    if(is.null(lambda)) {
      if(!search.grid){
        fit <- optim(0,face_gcv,method=method,lower=lower,upper=upper,control=control)
        if(fit$convergence>0) {
          expression <- paste("Smoothing failed! The code is:",fit$convergence)
          print(expression)
        }

        lambda <- exp(fit$par)
      } else {
        Lambda <- seq(lower,upper,length=search.length)
        Length <- length(Lambda)
        Gcv <- rep(0,Length)
        for(i in 1:Length)
          Gcv[i] <- face_gcv(Lambda[i])
        i0 <- which.min(Gcv)
        lambda <- exp(Lambda[i0])
      }
    }
    YS <- MM(Ytilde,1/(1+lambda*s),2)

    ###################################################
    ####  Eigendecomposition of Smoothed Data #########
    ###################################################
    if(c.p <= I){
      temp <- YS%*%t(YS)/I
      Eigen <- eigen(temp,symmetric=TRUE)
      A <- Eigen$vectors
      Sigma <- Eigen$values/J
    } else {
      temp <- t(YS)%*%YS/I
      Eigen <- eigen(temp,symmetric=TRUE)
      Sigma <- Eigen$values/J
      #N <- sum(Sigma>0.0000001)
      A <- YS%*%(Eigen$vectors%*%diag(1/sqrt(Eigen$values)))/sqrt(I)
    }
    if(iter.miss>1&&iter.miss< Niter.miss) {
      diff <- norm(YS-YS.temp,"F")/norm(YS,"F")
      if(diff <= 0.02)
        convergence.vector[iter.miss+1] <- 1
    }

    YS.temp <- YS
    iter.miss <- iter.miss + 1
    N <- min(I,c.p)
    d <- Sigma[1:N]
    d <- d[d>0]
    per <- cumsum(d)/sum(d)

    N <- ifelse (is.null(npc), min(which(per>pve)), min(npc, length(d)))

    #print(c(iter.miss,convergence.vector[iter.miss+1],lambda,diff))
    #########################################
    #######     Principal  Scores   #########
    ########   data imputation      #########
    #########################################

    if(imputation) {
      A.N <- A[,1:N]
      d <- Sigma[1:N]
      sigmahat2  <-  max(mean(data[!Index.miss]^2) -sum(Sigma),0)
      Xi <- t(A.N)%*%Ytilde
      Xi <- t(as.matrix(B%*%(A0%*%((A.N%*%diag(d/(d+sigmahat2/J)))%*%Xi))))
      data <- data*(1-Index.miss) + Xi*Index.miss
      if(sum(is.na(data))>0)
        print("error")
    }
    #if(iter.miss%%10==0) print(iter.miss)
  } ## end of while loop

  ### now calculate scores
  if(is.null(Y.pred)) Y.pred = data
  else {Y.pred = t(t(as.matrix(Y.pred))-meanX)}

  N <- ifelse (is.null(npc), min(which(per>pve)), npc)
  if (N>ncol(A)) {
    warning(paste0("The requested npc of ", npc,
                   " is greater than the maximum allowable value of ",
                   ncol(A), ". Using ", ncol(A), "."))
    N <- ncol(A)
  }
  npc <- N

  Ytilde <- as.matrix(t(A0)%*%(Bt%*%t(Y.pred)))
  sigmahat2 <- max(mean(data[!Index.miss]^2) -sum(Sigma),0)
  Xi <- t(Ytilde)%*%(A[,1:N]/sqrt(J))
  Xi <- MM(Xi,Sigma[1:N]/(Sigma[1:N] + sigmahat2/J))

  eigenvectors = as.matrix(B%*%(A0%*%A[,1:N]))
  eigenvalues = Sigma[1:N] #- sigmahat2/J

  Yhat <- t(A[,1:N])%*%Ytilde
  Yhat <- as.matrix(B%*%(A0%*%A[,1:N]%*%diag(eigenvalues/(eigenvalues+sigmahat2/J))%*%Yhat))
  Yhat <- t(Yhat + meanX)


  scores <- sqrt(J)*Xi[,1:N]
  mu <- meanX
  efunctions <- eigenvectors[,1:N]
  evalues <- J*eigenvalues[1:N]

  ret.objects <- c("scores", "mu", "efunctions", "evalues", "npc")
  ret = lapply(1:length(ret.objects), function(u) get(ret.objects[u]))
  names(ret) = ret.objects
  class(ret) = "fpca"
  return(ret)

}
