##' Convert tfb_fpc object to a list
##' @param tfb_fpc_obj object turned by `tfb_fpc`
##' @importFrom stats coefficients
extract_fpca <- function(tfb_fpc_obj){
  # may change this to not return the basis object
  N = length(tfb_fpc_obj)
  efunctions = attr(tfb_fpc_obj, "basis_matrix")
  evalues = attr(tfb_fpc_obj, "score_variance")
  npc = length(evalues)
  error_var = attr(tfb_fpc_obj, "error_variance")

  coef_list = coefficients(tfb_fpc_obj)
  score_list = lapply(coef_list, "[", -1)  #drop intercepts
  scores = do.call("rbind", score_list)

  fpca_obj <- list(
    Yhat_tfb = tfb_fpc_obj,
    scores = scores,
    mu = efunctions[, 1],
    efunctions = efunctions[, -1],
    evalues = evalues,
    npc = npc,
    error_var = error_var
  )
  class(fpca_obj) <- c("fpca", "rfr_fpca")
  fpca_obj
}


#' Extract scores from an FPC object
#'
#' This function will extract FPC scores from an object produced by `rfr_fpca`, and return these scores in a data frame.
#'
#' @param rfr_fpca_obj object returned by `rfr_fpca`
#'
#' @return Data frame containing FPCA scores
#'
#' @examples
#' library(refundr)
#' library(tidyverse)
#'
#' data(dti_df)
#' fpca_irregular <- rfr_fpca(Y = "cca", data = dti_df)
#' scores <- refundr:::extract_fpc_scores(fpca_irregular)
#'
#' # this gets you scores on a "new" df -- kinda messy though ...
#' predict(fpca_irregular, slice(dti_df, 1:10)) %>%
#'   refundr:::extract_fpca() %>%
#'   refundr:::extract_fpc_scores()
#'
extract_fpc_scores = function(rfr_fpca_obj){

  score_mat = rfr_fpca_obj$scores

  colnames(score_mat) = str_c(rfr_fpca_obj$model_var, 1:dim(score_mat)[2], sep = "_score_")

  as_tibble(score_mat)

}
