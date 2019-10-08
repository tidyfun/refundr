##' Tidy interface to refund fpca functions
##'
##' Allows calls to \code{fpca.sc}, \code{fpca.face}, and \code{fpca.ssvd} as implemented in the \code{refund} package. Tidy functional data objects are input and tidy objects are returned as well. Generic function takes in a \code{tfd} vector and passes it to a fpca method.
##'
##' @title rfr_fpca Generic function for functional principal component analyses.
##'
##' @param data a \code{tfd} data vector.
##' @param pve proportion of variance explained: used to choose the number of
##' principal components.
##' @param npc prespecified value for the number of principal components (if
##' given, this overrides \code{pve}).
##' @param method fpca method of choice. Different options are available but defaults will be set depending on regularity/irregularity of the data
##' @param ... optional arguments to be passed to methods. Takes arguments from original \code{refund} functions.
##' @return
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##' @seealso \code{\link{rfr_fpca.tfd_irreg}}, \code{\link{rfr_fpca.tfd_reg}}
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(dti_df)
##' fpca_results <- rfr_fpca(data = dti_df$cca)
##'
##' data(chf_df)
##' fpca_results <- rfr_fpca(data = chf_df$activity)
##' }
##'
##' @export
rfr_fpca <- function(data, pve = 0.99, npc = NULL, method = NULL, ...){
  UseMethod("rfr_fpca", data)
}


#' @rdname rfr_fpca
#' @export
rfr_fpca.tfb <- function(data, pve = 0.99, npc = NULL, method = NULL, ...){
  data <- tfd(data)
  rfr_fpca(data = data, pve = pve, npc = npc, method = method, ...)
}
