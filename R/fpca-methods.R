# methods for rfr_fpca-objects
#
#
# Author: Julia Wrobel
###############################################################################

##' Obtain residuals and fitted values for a rfr_fpca models
##'
##' @param object an \code{rf_fpca} object
##' @param ... optional arguments to be passed to methods
##'
##' @importFrom stats residuals
##' @method residuals rfr_fpca
##' @aliases fitted.rfr_fpca
##' @export
residuals.rfr_fpca <- function(object, ...){
  object$Y - tfd(object$Yhat_tfb, arg = tf_arg(object$Y))
}

##' @importFrom stats fitted fitted.values
##' @method fitted rfr_fpca
##' @export
##' @rdname residuals.rfr_fpca
fitted.rfr_fpca <- function(object, ...){
  tfd(object$Yhat_tfb, arg = tf_arg(object$Y))
}

#' Prediction for functional principal component analysis
#'
#' Takes a fitted \code{rfr_fpca}-object produced by \code{\link{rfr_fpca}()} and
#' produces predictions given a new set of values or the original values used for the
#' model fit.
#'
##' @param object an \code{rfr_fpca} object
##' @param newdata An optional `tf` data vector. If omitted, the fitted values are returned.
##' @param ... optional arguments to be passed to methods
##'
##' @importFrom stats fitted fitted.values
##' @method predict rfr_fpca
##' @export
predict.rfr_fpca <- function(object, newdata, ...){
  ## need different behavior for regular vs. irregular objects?
   # nah, just need to make sure it works for irregular objects.
    # default behavior (with no new data) is to return fitted values
  if (missing(newdata) || is.null(newdata)) {
    return(fitted(object))
  }

  ## include some data checks -- args for new data and fpc expansion, etc

  model_var = object$model_var
  new_tf = newdata[[model_var]]

  Ypred = as.matrix(spread(as.data.frame(new_tf), key = .data$arg, value = .data$value)[,-1])

  new_scores = estimate_fpc_scores(object, Ypred)

  coef_list <- split(cbind(1, new_scores), row(cbind(1, new_scores)))

  tfb_ob = object$Yhat_tfb

  structure(
    coef_list,
    domain = attr(tfb_ob, "domain"),
    basis = attr(tfb_ob, "basis"),
    basis_label = attr(tfb_ob, "basis_label"),
    basis_matrix = attr(tfb_ob, "basis_matrix"),
    arg = attr(tfb_ob, "arg"),
    resolution = attr(tfb_ob, "resolution"),
    score_variance = attr(tfb_ob, "score_variance"),
    error_variance = attr(tfb_ob, "error_variance"),
    class = c("tfb_fpc", "tfb", "tf")
  )

}
