source("poisson_glmm.R")
epilepsy <- read_csv("epilepsy.csv")
epilepsy <- epilepsy %>%
  group_by(id, treat, expind, age) %>%
  summarize(seizures = sum(seizures),
            .groups = "drop")

full_fit <- run_model(epilepsy,example = "epilepsy")
true_beta <- full_fit$beta
true_sigmasq <- full_fit$sigmasq

boot <- bootstrap(epilepsy, subject_index = 1, B = 100, true_beta, true_sigmasq)
BS_summary <- boot %>% group_by(term) %>% summarise(estimates = mean(estimate),
                                      se = sd(estimate),
                                      LowerCI = quantile(estimate,0.025),
                                      UpperCI = quantile(estimate,0.975))
## hypothesis test
if(0>BS_summary[4,]$LowerCI & 0<BS_summary[4,]$UpperCI){
  print('0 is inside the 95% Confidence Interval, so we do not reject H0, and beta is not significantly different from 0')
}
else{
  print('0 is outside the 95% Confidence Interval, so we reject H0, and beta is significantly different from 0')
}
