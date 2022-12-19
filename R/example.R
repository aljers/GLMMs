source("poisson_glmm.R")
epilepsy <- read_csv("epilepsy.csv")
epilepsy <- epilepsy %>%
  group_by(id, treat, expind, age) %>%
  summarize(seizures = sum(seizures),
            .groups = "drop")

full_fit <- run_model(epilepsy,example = "epilepsy")
true_beta <- full_fit$beta
true_sigmasq <- full_fit$sigmasq
h0_beta <- true_beta
h0_beta[4] <- 0

boot <- bootstrap(dataset = epilepsy, true_beta, true_sigmasq)
BS_summary <- BootSummary(boot)
## hypothesis test
boot_h0 <- bootstrap(dataset = epilepsy, h0_beta, true_sigmasq)
h0_summary <- BootSummary(boot_h0)
if(0>BS_summary[4,]$LowerCI & 0<BS_summary[4,]$UpperCI){
  print('0 is inside the 95% Confidence Interval, so we do not reject H0, and beta is not significantly different from 0')
} else{
  print('0 is outside the 95% Confidence Interval, so we reject H0, and beta is significantly different from 0')
  }
