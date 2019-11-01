##' Convert tfb_fpc object to a list
##' @param tfb_fpc_obj object turned by `tfb_fpc`
##' @importFrom stats coefficients
extract_fpca <- function(tfb_fpc_obj){
  # may change this to not return the basis object
  N = length(tfb_fpc_obj)
  efunctions = attr(tfb_fpc_obj, "basis_matrix")
  evalues = attr(tfb_fpc_obj, "score_variance")
  npc = length(evalues)
  scores = coefficients(tfb_fpc_obj) %>%
    lapply("[", -1) %>% #drop intercepts
    do.call("rbind", .)

  fpca_obj <- list(
    Yhat_tfb = tfb_fpc_obj,
    scores = scores,
    mu = efunctions[, 1],
    efunctions = efunctions[, -1],
    evalues = evalues,
    npc = npc
  )
  class(fpca_obj) <- c("fpca", "rfr_fpca")
  fpca_obj
}

##' Convert tfb_fpc object to a list
##' @param object an rf_fpca object
##' @param ... optional arguments to be passed to methods
##'
##' @importFrom stats fitted fitted.values
##' @method fitted rfr_fpca
##' @export
fitted.rfr_fpca = function(object, ...){
  tfd(object$Yhat_tfb)
}
