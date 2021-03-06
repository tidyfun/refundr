##' Tidy interface to `refund` FPCA functions
##'
##' Allows calls to (wrappers for) [refund::fpca.sc()], [refund::fpca.face()], and
##' [refund::fpca.ssvd()] as implemented in the `{refund}` package. Tidy functional data
##' objects are input and tidy objects are returned as well.
##'
##' @title rfr_fpca Generic function for functional principal component
##'   analyses.
##'
##' @param Y character value indicating name for a `tfd` data vector. Class `tfb` also supported.
##' @param data a `data.frame` containing the `Y` vector as a column.
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
##' library(refundr)
##' data(dti_df)
##' fpca_irregular <- rfr_fpca(Y = "cca", data = dti_df)
##'
##' data(chf_df)
##' fpca_regular <- rfr_fpca(Y = "activity", data = chf_df)
##' }
##'
##' @export
##' @import tidyfun
rfr_fpca <- function(Y, data, pve = 0.99, npc = NULL, method = NULL, ...){
  UseMethod("rfr_fpca", data[[Y]])
}


#' @rdname rfr_fpca
#' @export
rfr_fpca.tfb <- function(Y, data, pve = 0.99, npc = NULL, ...){
  data[[Y]] <- tfd(data[[Y]])
  rfr_fpca(Y = Y, data = data, pve = .99, npc = npc, ...)
}
