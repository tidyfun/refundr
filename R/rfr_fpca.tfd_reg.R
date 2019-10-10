##' Tidy interface for fpca with regularly-spaced functional data
##'
##' Default call for regular data is to `fpca_face`, this function processes the data and provides a wrapper to `fpca_face`.
##'
##'
##' @param data a `tfd` data vector.
##' @param pve proportion of variance explained: used to choose the number of
##' principal components.
##' @param npc prespecified value for the number of principal components (if
##' given, this overrides `pve`).
##' @param method fpca method of choice. Different options are available but defaults will be set depending on regularity/irregularity of the data
##' @param ... optional arguments to be passed to methods. Takes arguments from original `refund` functions.
##' @return
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##' @seealso [rfr_fpca()], [rfr_fpca.tfd_irreg()]
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(chf_df)
##' fpca_results <- rfr_fpca(data = chf_df$activity)
##' }
##' @importFrom tidyr spread
##' @importFrom splines spline.des
##' @export
rfr_fpca.tfd_reg <- function(data, pve = 0.99, npc = NULL, method = fpca_face, ...){

  results <- tfb_fpc(data, method = method, pve = pve, npc = npc, ...)
  results_ls <- extract_fpca(results)
  return(results_ls)

}
