##' @describeIn rfr_fpca FPCA for data on irregular grids defaults to [refund::fpca.sc()]
##' @importFrom rlang as_name enquo `!!`
##' @export
rfr_fpca.tfd_irreg <- function(Y, data, pve = 0.99, npc = NULL, method = fpca_sc, ...){

  method_name = as_name(enquo(method))
  model_var_name = as_name(enquo(Y))

  tf_vec <- pull(data, !!enquo(Y))
  results <- tfb_fpc(tf_vec, method = method, pve = pve, npc = npc, ...)

  results_ls <- extract_fpca(results)
  results_ls$Y <- tf_vec
  results_ls$model_var <- model_var_name
  results_ls$method = method_name

  return(results_ls)
}
