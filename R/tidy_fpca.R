##' Tidy interface to refund fpca functions
##'
##' Allows calls to \code{fpca.sc}, \code{fpca.face}, and \code{fpca.ssvd} as implemented in the \code{refund} package. Tidy functional data objects are input and tidy objects are returned as well.
##'
##'
##' @param Y a \code{tfd} data object that exists as a column in the dataframe \code{data} (regular vs. irregular may be assigned differnet defaults).
##' @param data a \code{tidyfun} dataframe containing a \code{tfd} vector of data.
##' @param pve proportion of variance explained: used to choose the number of
##' principal components.
##' @param npc prespecified value for the number of principal components (if
##' given, this overrides \code{pve}).
##' @param fpca_method fpca method of choice. Different options are available but defaults will be set depending on regularity/irregularity of the data
##' @param ... optional arguments to be passed to methods. Takes arguments from original \code{refund} functions.
##' @return
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##'
##' @references keep original references
##'
##' @examples
##' \dontrun{
##' library(refunder)
##' data(dti_df)
##' fpca_results = tidy_fpca(cca, dti_df)
##' }
##'
##' @importFrom dplyr "%>%" enquo pull select
##' @importFrom tidyr spread
##' @export

tidy_fpca <- function(Y, data, pve = 0.99, npc = NULL, fpca_method = NULL, ...){

  # do different default method based on the class of the tfd vector (regular or irregular)
  # potentially allow data = NULL, so just a tfd vector can be used in the Y argument?
  Y <- enquo(Y)


  Y_mat <- data %>%
    pull(!! Y) %>%
    as.data.frame() %>%
    spread(key = arg, value = value) %>%
    select(-id) %>%
    as.matrix()


  # eval(paste0("fpca_", fpca_method))
  # what is the best way to call whatever function you want? Im trying to avoid using if/else
  results <- fpca_sc(Y = Y_mat, pve = pve, npc = npc, ...)

  # eventually return a better formatted object that is consistent across choice of fpca method
  return(results)
}
