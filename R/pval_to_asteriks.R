#' Transforms p-values into significance asteriks

#' @param pval vector of p-values
#'
#' @return vector of strings with asteriks
#' @export
#'
#'
pval_to_asteriks <- function(pval) {
  ret <- rep("", length = length(pval))
  for (ind in 1:length(pval)) {
    if (pval[ind] < 0.05) {
      ret[ind] <- "*"
    }
    if (pval[ind] < 0.01) {
      ret[ind] <- "**"
    }
    if (pval[ind] < 0.001) {
      ret[ind] <- "***"
    }
  }
  return(ret)
}
