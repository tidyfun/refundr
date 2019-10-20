##' @describeIn rfr_fpca FPCA for data on irregular grids defaults to [refund::fpca.sc()]
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
