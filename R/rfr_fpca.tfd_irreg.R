##' Tidy interface for fpca with irregularly-spaced functional data
##'
##' Default call for irregular data is to `fpca_sc`, this function processes the data and provides a wrapper to `fpca_sc`.
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
##' @seealso [rfr_fpca()], [rfr_fpca.tfd_reg()]
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(dti_df)
##' fpca_results <- rfr_fpca(data = dti_df$cca)
##' }
##'
##' @importFrom tidyr spread
##' @import tidyfun
##' @export
rfr_fpca.tfd_irreg <- function(data, pve = 0.99, npc = NULL, method = fpca_sc, ...){

  ## eventually change to as.matrix.td() call from tidyfun package
  #Y_mat <- as.matrix(spread(as.data.frame(data), key = arg, value = value)[,-1])
  # argvals = attr(data, "args") # could feed this to fpca.sc, but this won't work if each subject has it's own grid, so instead use fpca.sc default
  #data <- tidyfun:::df_2_mat(data) ## calls complete.cases on the data, only use this once fixed regular function
  #data <- as.matrix(spread(as.data.frame(data), key = arg, value = value)[,-1])

  results <- tfb_fpc(data, method = method, pve = pve, npc = npc, ...)
  results_ls <- extract_fpca(results)
  return(results_ls)
}
