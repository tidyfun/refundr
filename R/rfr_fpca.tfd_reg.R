##' @describeIn rfr_fpca FPCA for data on a regular grid defaults to [refund::fpca.face()]
##' @export
rfr_fpca.tfd_reg <- function(Y, data, pve = 0.99, npc = NULL, method = fpca_face, ...){

  Y <- data[[Y]]
  results <- tfb_fpc(Y, method = method, pve = pve, npc = npc, ...)
  results_ls <- extract_fpca(results)
  results_ls$Y <- Y
  return(results_ls)

}
