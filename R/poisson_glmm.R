#' Parametric Bootstrap for poisson GLMM
#'
#' @description Parametric Bootstrap for poisson GLMM
#'
#' @param dataset table: In this dataset, we have one response and one variable indicates groups/subjects the input dataset.
#' @param true_beta vector: the prior value of coefficients used to generate linear predictor, eta.
#' @param true_sigmasq numeric: non-negative. The standard deviation of the random effect. In this project, we assume all the random effect has equal standard deviation.
#' @param subject_index integer: a index states which column of dataset is used the first as the group/subject indicator. The default value is subject_index = 1 means column(row) is used as group/subject index.
#' @param B integer:positive. The resample size of Bootstrap method. The default size is 100.
#'
#' @details
#' To be convince, let i be the number of groups/subjects, j be the number of
#' repeated measurement for each subject, B be the number of bootstrap loop (
#' dafult value is 100)
#' Conduct bootstrap:\n
#' 1.Resample k random effects, where k = i * B = (# of subjects/groups * # of
#' bootstrap iteration).\n
#' 2.Repeat every generated random effect j times, where j is the number of
#' repeated measurement.\n
#' 3.Bind these random effects with intercept and fixed effects provided in
#' argument 'dataset' to construct design matrix.\n
#' 4.Calculate linear predictor eta, and hence compute corresponding expected
#' value of the response variable.\n
#'
#' @return table: a summary table with 8 columns.
#' @import tidyverse,lme4,broom.mixed
#' @export
#'
#' @examples
#' ## provide prior coefficients and sigmasq for generating random effect
#' full_fit <- run_model(epilepsy,example = "epilepsy")
#' true_beta <- full_fit$beta
#' true_sigmasq <- full_fit$sigmasq
#' #################################################################################
#' ## run bootstrap with epilepsy data. We use true_beta for conducting linear    ##
#' ## predictors, true_sigmasq for generating random effects. We identify the     ##
#' ## first column as the subject index and set the bootstrap resample size be 100##
#' #################################################################################
#' boot <- bootstrap(dataset = epilepsy, true_beta, true_sigmasq, subject_index = 1, B = 100)

bootstrap <- function(dataset, beta, sigmasq, subject_index = 1, B = 100){
  # obtain number of subjects and number of repeat measurement for each subject
  # according to subject_index
  n_sub <- nrow(unique(dataset %>% select(subject_index)))
  n_rep <- nrow(dataset)/n_sub
  # generate random effect with given sigmasq
  z_i <- rep(rnorm(B*n_sub,0,sqrt(sigmasq)),each=2)
  mu_sample <- exp(rep(as.matrix(dataset  %>%
                                   mutate(intercept = rep(1,n()),
                                          interact = expind*treat) %>%
                                   select(intercept, age, expind, interact) ) %*%
                         beta, B) +
                     z_i)
  data_sample <- dataset %>%
    select(-seizures) %>%
    slice(rep(1:n(), B)) %>%
    mutate(seizures = rpois(length(mu_sample),
                            mu_sample),
           Boot = rep(1:B,each = nrow(dataset)))
  return(data_sample %>%
           group_by(Boot) %>%
           summarise(tidy(glmer(seizures ~ age + expind + expind:treat + (1|id),
                                family = poisson)),
                     .groups = 'drop') %>%
           arrange(term))
}
