##' Convert tfb_fpc object to a list
##'
extract_fpca <- function(rfr_tfb){
  # I want this list to only contain rfr_tfb unless that element is accessed
  # try different things then testing how much memory it takes up
  N = length(rfr_tfb)
  efunctions = attr(rfr_tfb, "basis_matrix")
  npc = dim(efunctions[, -1])[2]

  fpca_obj <- list(
    Yhat_tfb = rfr_tfb,
    scores = matrix(unlist(rfr_tfb), nrow = N, ncol = npc + 1, byrow = TRUE)[-1],
    mu = efunctions[, 1],
    efunctions = efunctions[, -1],
    evalues = NULL, ## update
    npc = npc
  )
  class(fpca_obj) <- c("fpca", "rfr_fpca")
  fpca_obj
}

##' Convert tfb_fpc object to a list
##' @param object an rf_fpca object
##' @importFrom stats fitted fitted.values
##' @export
fitted.rfr_fpca = function(object){
  tfd(object$Yhat_tfb)
}
