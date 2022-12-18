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
