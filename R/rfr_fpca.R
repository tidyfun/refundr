##' Tidy interface to `refund` FPCA functions
##'
##' Allows calls to (wrappers for) [refund::fpca.sc()], [refund::fpca.face()], and
##' [refund::fpca.ssvd()] as implemented in the `{refund}` package. Tidy functional data
##' objects are input and tidy objects are returned as well.
##'
##' @title rfr_fpca Generic function for functional principal component
##'   analyses.
##'
##' @param data a `tfd` data vector.
##' @param pve proportion of variance explained: used to choose the number of
##'   principal components.
##' @param npc prespecified value for the number of principal components (if
##'   given, this overrides `pve`).
##' @param method FPCA method of choice. Different options are available but
##'   default depends on regularity/irregularity of `data`, see **Methods**.
##' @param ... optional arguments to be passed to FPCA algorithms. Takes arguments from
##'   original `refund` functions.
##' @return an object of class `fpca`, internal structure not yet fixed. Currently
##' a list with a [tidyfun::tfb_fpc()] object and additional entries describingt the FPCA results.
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(dti_df)
##' fpca_irregular <- rfr_fpca(data = dti_df$cca)
##'
##' data(chf_df)
##' fpca_regular <- rfr_fpca(data = chf_df$activity)
##' }
##'
##' @export
##' @import tidyfun
rfr_fpca <- function(data, pve = 0.99, npc = NULL, method = NULL, ...){
  UseMethod("rfr_fpca", data)
}


#' @rdname rfr_fpca
#' @export
rfr_fpca.tfb <- function(data, pve = 0.99, npc = NULL, method = NULL, ...){
  data <- tfd(data)
  rfr_fpca(data = data, pve = pve, npc = npc, method = method, ...)
}
