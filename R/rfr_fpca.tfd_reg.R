##' @describeIn rfr_fpca FPCA for data on a regular grid defaults to [refund::fpca.face()]
##' @export
rfr_fpca.tfd_reg <- function(Y, data, pve = 0.99, npc = NULL, method = fpca_face, ...){

  method_name = rlang::as_name(enquo(method))

  tf_vec <- data[[Y]]
  results <- tfb_fpc(tf_vec, method = method, pve = pve, npc = npc, ...)

  results_ls <- extract_fpca(results)
  results_ls$Y <- tf_vec
  results_ls$model_var <- Y
  results_ls$method = method_name

  return(results_ls)
}
