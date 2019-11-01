##' @describeIn rfr_fpca FPCA for data on irregular grids defaults to [refund::fpca.sc()]
##' @export
rfr_fpca.tfd_irreg <- function(Y, data, pve = 0.99, npc = NULL, method = fpca_sc, ...){

  Y <- data[[Y]]
  results <- tfb_fpc(Y, method = method, pve = pve, npc = npc, ...)
  results_ls <- extract_fpca(results)
  return(results_ls)
}
