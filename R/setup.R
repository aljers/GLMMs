set.seed(9864)
library(tidyverse)
library(usethis)
library(lme4)
library(broom.mixed)

epilepsy <-  read.csv('epilepsy.csv')
epilepsy <- epilepsy %>%
  group_by(id, treat, expind, age) %>%
  summarize(seizures = sum(seizures),
            .groups = "drop")
