#' Parametric Bootstrap for poisson GLMM
#'
#' @description Parametric Bootstrap for poisson GLMM
#'
#' @param dataset table: In this dataset, we have one response and one variable indicates groups/subjects the input dataset.
#' @param beta vector: the prior value of coefficients used to generate linear predictor, eta.
#' @param sigmasq numeric: non-negative. The standard deviation of the random effect. In this project, we assume all the random effect has equal standard deviation.
#' @param subject_index integer: a index states which column of dataset is used the first as the group/subject indicator. The default value is subject_index = 1 means column(row) is used as group/subject index.
#' @param B integer:positive. The resample size of Bootstrap method. The default size is 100.
#'
#' @details
#' To be convince, let i be the number of groups/subjects, j be the number of
#' repeated measurement for each subject, B be the number of bootstrap loop (
#' dafult value is 100)
#' Conduct bootstrap:
#' 1.Resample k random effects, where k = i * B = (# of subjects/groups * # of
#' bootstrap iteration).
#' 2.Repeat every generated random effect j times, where j is the number of
#' repeated measurement.
#' 3.Bind these random effects with intercept and fixed effects provided in
#' argument 'dataset' to construct design matrix.
#' 4.Calculate linear predictor eta, and hence compute corresponding expected
#' value of the response variable.
#'
#' @return table: a summary table with 8 columns.
#'
#' @examples
#' ## provide prior coefficients and sigmasq for generating random effect
#' full_fit <- run_model(epilepsy,example = "epilepsy")
#' true_beta <- full_fit$beta
#' true_sigmasq <- full_fit$sigmasq
#' ## run bootstrap with epilepsy data. We use true_beta for conducting linear
#' ## Run bootstrap with epilepsy data. We use true_beta for conducting linear
#' ## predictors, true_sigmasq for generating random effects. We identify the
#' ## first column as the subject index and set the bootstrap resample size be 100
#' boot <- bootstrap(dataset = epilepsy, true_beta, true_sigmasq, subject_index = 1, B = 100)
#' @import tidyverse
#' @importFrom  readr read_csv
#' @import dplyr
#' @importFrom lme4 glmer
#' @import broom.mixed
#' @export
bootstrap <- function(dataset, beta, sigmasq, subject_index = 1, B = 100){
  # obtain number of subjects and number of repeat measurement for each subject
  # according to subject_index
  n_sub <- nrow(unique(dataset %>% select(subject_index)))
  n_rep <- nrow(dataset)/n_sub
  # generate random effect with given sigmasq
  z_i <- rep(rnorm(B*n_sub,0,sqrt(sigmasq)),each=2)
  # Obtain number of subjects and number of repeat measurement for each subject
  # According to subject_index
  n_sub <- nrow(unique(dataset %>% select(subject_index)))
  n_rep <- nrow(dataset)/n_sub
  # Generate random effect with given sigmasq. To increase computing efficiency,
  # we generate all the random effects at one shot, instead of looping over resample size.
  # There are n_sub random effects in each bootstrap run, and there are B runs in
  # total.
  # For each subject, there are n_rep repeated measurements, so we repeat each of
  # its element by n_rep.
  z_i <- rep(rnorm(B*n_sub,0,sqrt(sigmasq)),each=n_rep)
  # The following code combines two steps.
  # 1. Use design matrix and beta vector to calculate linear predictors.
  # 2. Transform the linear predictors via link function.
  mu_sample <- exp(rep(as.matrix(dataset  %>%
                                   mutate(intercept = rep(1,n()), # Create a new column for intercept term
                                                                  # and a new column for the interaction
                                                                  # between expind and treat.
                                          interact = expind*treat) %>%
                                   select(intercept, age, expind, interact) ) %*%
                         beta, B) + z_i) #add random effect vector

  # Then, we replace the original response value by the random generation for
  # poisson distributed response.
  data_sample <- dataset %>%
    select(-seizures) %>% # remove original response value
    slice(rep(1:n(), B)) %>% # repeat whole dataset B times
    mutate(seizures = rpois(length(mu_sample),
                            mu_sample),
           Boot = rep(1:B,each = nrow(dataset))# index for bootstrap runs
           )
  # Finally, we fit GLMM with generated data and store all of them together as function return
  return(data_sample %>%
           group_by(Boot) %>%
           summarise(tidy(glmer(seizures ~ age + expind + expind:treat + (1|id),
                                family = poisson)),
                     .groups = 'drop') %>%
           arrange(term))
}
