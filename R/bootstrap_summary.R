#' Analyse and Summaries bootstrap result
#' @description Point Estimate, Standard Error and Confidence Interval for bootstrap result
#' @param object table: The function return the bootstrap() function in this package. More detail in help(bootstrap)
#' @param SignifLv numeric: The significance level for conducting confidence interval. The default value is 0.05.
#'
#' @return table: A 5x5 table. In each row, there is term name, point estimates, bootstrap standard error, Lower Bound and Upper Bound of confidence interval.
#' @import dplyr
#' @export
#'
#' @examples
#' ## provide prior coefficients and sigmasq for generating random effect
#' full_fit <- run_model(epilepsy,example = "epilepsy")
#' true_beta <- full_fit$beta
#' true_sigmasq <- full_fit$sigmasq
#' ## create a bootstrap object, more detail in bootstrap()
#' boot <- bootstrap(dataset = epilepsy, true_beta, true_sigmasq)
#' ## summaries bootstrap estimates for each term
#' BS_summary <- BootSummary(boot)

BootSummary <- function(BS_object, SignifLv = 0.05){
  collection <- BS_object %>% group_by(term) %>% summarise(estimates = mean(estimate),
                                                      se = sd(estimate),
                                                      LowerCI = quantile(estimate,SignifLv/2),
                                                      UpperCI = quantile(estimate,1-SignifLv/2))
  return(collection)
}
