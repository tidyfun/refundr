#' estimate_fpc_scores
#'
#' @param tfb_fpc_obj a fitted FPCA object produced by [rfr_fpca()]
#' @param Ypred Matrix containing curves for which estimated scores are needed
#' @param method BLUPs or Numeric integration. Only BLUPs currently implemented
#'
#' @return
#'
estimate_fpc_scores = function(tfb_fpc_obj, Ypred, method = "blup") {

  if (method != "blup") { stop("Only estimation using BLUPs are currently implemented.") }

  efunctions = tfb_fpc_obj[["efunctions"]]
  evalues = tfb_fpc_obj[["evalues"]]
  mu = tfb_fpc_obj[["mu"]]
  npc = tfb_fpc_obj[["npc"]]
  error_var = tfb_fpc_obj[["error_var"]]

  D = dim(efunctions)
  I.pred = dim(Ypred)[1]

  D.inv = diag(1/evalues, nrow = npc, ncol = npc)
  Z = efunctions
  data.tilde = Ypred - matrix(mu, I.pred, D, byrow = TRUE)
  Yhat = matrix(0, nrow = I.pred, ncol = D)
  rownames(Yhat) = rownames(Ypred)
  colnames(Yhat) = colnames(Ypred)
  scores = matrix(NA, nrow = I.pred, ncol = npc)
  for (i.subj in 1:I.pred) {
    obs.points = which(!is.na(Ypred[i.subj, ]))
    if (error_var == 0 & length(obs.points) < npc)
      stop("Measurement error estimated to be zero and there are fewer observed points than PCs; scores cannot be estimated.")
    Zcur = matrix(Z[obs.points, ], nrow = length(obs.points), ncol = dim(Z)[2])
    ZtZ_sD.inv = solve(crossprod(Zcur) + error_var * D.inv)
    scores[i.subj, ] = ZtZ_sD.inv %*% t(Zcur) %*% (data.tilde[i.subj, obs.points])
    Yhat[i.subj, ] = t(as.matrix(mu)) + scores[i.subj, ] %*% t(efunctions)
  }

  scores

}
