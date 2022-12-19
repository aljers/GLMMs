source("poisson_glmm.R")
epilepsy <- read_csv("epilepsy.csv")
epilepsy <- epilepsy %>%
  group_by(id, treat, expind, age) %>%
  summarize(seizures = sum(seizures),
            .groups = "drop")

full_fit <- run_model(epilepsy,example = "epilepsy")
true_beta <- full_fit$beta
true_sigmasq <- full_fit$sigmasq


boot <- bootstrap(dataset = epilepsy, true_beta, true_sigmasq)
BS_summary <- BootSummary(boot)
## hypothesis test
h0_beta <- true_beta
h0_beta[4] <- 0
boot_h0 <- bootstrap(dataset = epilepsy, h0_beta, true_sigmasq)
## Compute the proportion of time that we obtain test statistic larger(more
## extreme) then the t value obtained from original model fit, full_fit. We use
## this proportion as the p-value of the significance test.
t_value <- full_fit$test_stat[4]
p_value <- (boot_h0 %>%
  filter(term == "expind:treat") %>%
  summarize(p = mean(abs(statistic) > abs(t_value))))$p

if(p_value < 0.05){
  print('Given 5% significance level, p value < 0.05 shows enough evidence against the null hypothesis.
        We reject H0, and beta is significantly different from 0')
} else{
  print('Given 5% significance level, p value > 0.05 does not show enough evidence against the null hypothesis.
        We do not reject H0, and beta is not significantly different from 0')
  }
