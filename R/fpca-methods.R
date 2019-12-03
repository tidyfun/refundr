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
##' @param object an \code{rf_fpca} object
##' @param newdata An optional `tfd` data vector. If omitted, the fitted values are returned.
##' @param ... optional arguments to be passed to methods
##'
##' @importFrom stats fitted fitted.values
##' @method predict rfr_fpca
##' @export
predict.rfr_fpca <- function(object, newdata, ...){
  ## need different behavior for regular vs. irregular objects?
   # nah, just need to make sure it works for irregular objects.
    # default behavior (with no new data) is to return fitted values
  if(missing(newdata) || is.null(newdata)) {
    fitted(object)
  }


}
