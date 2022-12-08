library(tidyverse)
library(dplyr)
library(here)

epilepsy <- read_csv("epilepsy.csv")


## Conduct bootstrap:
## Resample from each id for the after treatment period (n=4), combined with the before
## treatment record of this id, to be our bootstrapped dataset. Repeat this for N times.

bootstrap <- function(N = 500){
  source(here("estimation_functions.r"))
  for(i in 1:N){
    ## Sum separate observations for each patient in the after period:
    epilepsy.boot <- sample(epilepsy, 4, replace = TRUE)
    epilepsy.boot <- epilepsy %>%
      group_by(id, treat, expind, age) %>%
      summarize(seizures = sum(seizures),
                .groups = "drop")

    ## Fit the model:
    mod <- run_model(epilepsy.boot, "epilepsy")
    betai <- mod$beta
    sigmasqi <- mod$sigmasq
  }
}





