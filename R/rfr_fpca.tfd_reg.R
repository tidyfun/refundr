##' @describeIn rfr_fpca FPCA for data on a regular grid defaults to [refund::fpca.face()]
##' @export
rfr_fpca.tfd_reg <- function(data, pve = 0.99, npc = NULL, method = fpca_face, ...){

  results <- tfb_fpc(data, method = method, pve = pve, npc = npc, ...)
  results_ls <- extract_fpca(results)
  return(results_ls)

}
