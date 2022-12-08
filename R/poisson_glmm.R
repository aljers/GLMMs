library(tidyverse)
library(dplyr)
library(here)
library(lme4)

## Import and mutate the dataset:
epilepsy <- read_csv("epilepsy.csv")
epilepsy <- epilepsy %>%
  group_by(id, treat, expind, age) %>%
  summarize(seizures = sum(seizures),
            .groups = "drop")


## Conduct bootstrap:
## Resample 29 id's from `treat == 0` and 30 id's from `treat == 1`, choose all the
## attributes within each id.
## Repeat this for N times.

bootstrap <- function(N = 100){
  source(here("estimation_functions.R"))
  beta <- list()
  sigmasq <- c()
  for(i in 1:N){
    ## Resample 30 persons with `treat == 1`:
    boot.treat <- epilepsy %>%
      filter(treat == 1)
    boot.index1 <- sample(unique(boot.treat$id), 30, replace = TRUE)
    boot.treat <- do.call(rbind, lapply(boot.index1, function(x) epilepsy[epilepsy$id == x,]))

    ## Resample 29 persons with `treat == 0`:
    boot.placebo <- epilepsy %>%
      filter(treat == 0)
    boot.index0 <- sample(unique(boot.placebo$id), 29, replace = TRUE)
    boot.placebo <- do.call(rbind, lapply(boot.index0, function(x) epilepsy[epilepsy$id == x,]))

    boot <- rbind(boot.treat, boot.placebo)
    ## Fit the model:
    mod <- run_model(boot, "epilepsy")
    betai <- mod$beta
    sigmasqi <- mod$sigmasq
    sigmasq <- c(sigmasq, sigmasqi)
    beta[[i]] <- betai
  }
  return(list(beta = beta,
              sigmasq = sigmasq))
}


## Test the function:
boot <- bootstrap()
plot(density(boot$sigmasq))


