##' Tidy interface to refund fpca functions
##'
##' Allows calls to \code{fpca.sc}, \code{fpca.face}, and \code{fpca.ssvd} as implemented in the \code{refund} package. Tidy functional data objects are input and tidy objects are returned as well.
##'
##'
##' @param Y a \code{tfd} vector of data that specifies a column in the dataframe \code{data} (regular vs. irregular may be assigned differnet defaults).
##' @param data a \code{tidyfun} dataframe containing a \code{tfd} vector of data.
##' @param pve proportion of variance explained: used to choose the number of
##' principal components.
##' @param npc prespecified value for the number of principal components (if
##' given, this overrides \code{pve}).
##' @param method fpca method of choice. Different options are available but defaults will be set depending on regularity/irregularity of the data
##' @param ... optional arguments to be passed to methods. Takes arguments from original \code{refund} functions.

##' @return
##'
##' @author Julia, Jeff, Fabian (plus any authors of the original refund code)
##'
##' @references keep original references
##'
##' @examples
##' \dontrun{
##' library(tidyfun)
##' library(refunder)
##'
##' data(dti_df)
##' tidy_fpca(cca, dti_df)
##' }
##'
##' @importFrom dplyr "%>%" enquo pull select
##' @importFrom tidyr spread
##' @export

tidy_fpca <- function(Y, data, pve = 0.99, npc = NULL, method = NULL, ...){

  Y = enquo(Y)

  tfd = data %>%
    pull(!! Y)

  Y_mat = tfd %>%
    as.data.frame() %>%
    spread(key = arg, value = value) %>%
    select(-id) %>%
    as.matrix()


  results <- fpca_sc(Y = Y_mat, pve = pve, npc = npc, ...)

  return(results)
}
