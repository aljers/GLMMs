library(tidyverse)
library(dplyr)
library(lme4)
source("estimation_functions.R")

## Import and mutate the dataset:


## Conduct bootstrap:
## Resample 29 id's from `treat == 0` and 30 id's from `treat == 1`, choose all the
## attributes within each id.
## Repeat this for N times.


bootstrap <- function(dataset, subject_index = 1, B = 100, true_beta, true_sigmasq){

  n_sub <- nrow(unique(dataset %>% select(subject_index)))
  n_rep <- nrow(dataset)/n_sub
  z_i <- rep(rnorm(B*n_sub,0,sqrt(true_sigmasq)),each=2)
  mu_sample <- exp(rep(as.matrix(dataset  %>%
                                   mutate(intercept = rep(1,n()),
                                          interact = expind*treat) %>%
                                   select(intercept, age, expind, interact) ) %*%
                         true_beta, B)+z_i)
  data_sample <- dataset %>%
    select(-seizures) %>%
    slice(rep(1:n(), B)) %>%
    mutate(seizures = rpois(length(mu_sample),mu_sample),
           Boot = rep(1:B,each = nrow(dataset)))
  data_sample %>%
    group_by(Boot) %>%
    summarise(tidy(glmer(seizures ~ age + expind + expind:treat + (1|id), family = poisson)),
              .groups = 'drop')
}

