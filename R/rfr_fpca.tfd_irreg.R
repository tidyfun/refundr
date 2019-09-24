##' Tidy interface for fpca with irregularly-spaced functional data
##'
##' Default call for irregular data is to \code{fpca_sc}, this function processes the data and provides a wrapper to \code{fpca_sc}.
##'
##'
##' @param Y a \code{tfd} data vector.
##' @param pve proportion of variance explained: used to choose the number of
##' principal components.
##' @param npc prespecified value for the number of principal components (if
##' given, this overrides \code{pve}).
##' @param fpca_method fpca method of choice. Different options are available but defaults will be set depending on regularity/irregularity of the data
##' @param ... optional arguments to be passed to methods. Takes arguments from original \code{refund} functions.
##' @return
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##' @seealso \code{\link{rfr_fpca}}, \code{\link{rfr_fpca.tfd_reg}}
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(dti_df)
##' fpca_results = rfr_fpca(Y = dti_df$cca)
##' }
##'
##' @export
rfr_fpca.tfd_irreg <- function(Y, pve = 0.99, npc = NULL, fpca_method = NULL, ...){
  print("irreg")
}
