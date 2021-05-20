##' Tidy interface to `refund` FPCA functions
##'
##' Allows calls to (wrappers for) [refund::fpca.sc()], [refund::fpca.face()], and
##' [refund::fpca.ssvd()] as implemented in the `{refund}` package. Tidy functional data
##' objects are input and tidy objects are returned as well.
##'
##' @title rfr_fpca Generic function for functional principal component
##'   analyses.
##'
##' @param Y Unquoted variable name for the `tf` data vector that will be decomposed
##' using FPCA.
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
##' fpca_irregular <- rfr_fpca(Y = cca, data = dti_df)
##'
##' data(chf_df)
##' fpca_regular <- rfr_fpca(Y = activity, data = chf_df)
##' }
##'
##' @export
##' @import tidyfun
##' @importFrom dplyr pull
##' @importFrom rlang enquo `!!`
rfr_fpca <- function(Y, data, pve = 0.99, npc = NULL, method = NULL, ...){
  UseMethod("rfr_fpca", pull(data, !!enquo(Y)))
}


#' @rdname rfr_fpca
#' @importFrom dplyr mutate
#' @importFrom rlang `:=`
#' @export
rfr_fpca.tfb <- function(Y, data, pve = 0.99, npc = NULL, ...){
  data = mutate(data, !!enquo(Y) := tfd(!!enquo(Y)))
  rfr_fpca(Y = !!enquo(Y), data = data, pve = .99, npc = npc, ...)
}
