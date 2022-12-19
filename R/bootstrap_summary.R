BootSummary <- function(object, SignifLv = 0.05){
  collection <- object %>% group_by(term) %>% summarise(estimates = mean(estimate),
                                                      se = sd(estimate),
                                                      LowerCI = quantile(estimate,SignifLv/2),
                                                      UpperCI = quantile(estimate,1-SignifLv/2))
  return(collection)
}
