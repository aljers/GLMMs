#' Summary of the bootstrap
#'
#' @param object a dataframe returned by bootstrap function
#' @param SignifLv numeric: significance level, defaulted to 0.05
#'
#' @return a dataframe with the mean, se, and upper/lower quantile of the estimators
#' @export
#'
#' @examples
BootSummary <- function(object, SignifLv = 0.05){
  collection <- object %>% group_by(term) %>% summarise(estimates = mean(estimate),
                                                      se = sd(estimate),
                                                      LowerCI = quantile(estimate,SignifLv/2),
                                                      UpperCI = quantile(estimate,1-SignifLv/2))
  return(collection)
}
