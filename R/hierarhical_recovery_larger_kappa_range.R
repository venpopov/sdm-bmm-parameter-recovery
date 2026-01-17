library(bmm)
library(purrr)

simulate_hierarhical_sdm_data <- function(subj_params, n_trials) {
  y_list <- pmap(subj_params, \(c, kappa) rsdm(n = n_trials, mu = 0, c = c, kappa = kappa))
  data.frame(
    id = rep(seq_len(nrow(subj_params)), each = n_trials),
    y = unlist(y_list)
  )
}

generate_subj_parameters <- function(n_subj) {
  # same distributions for C and kappa
  mean_c <- 4
  var_c <- 1.5^2
  data.frame(
    c = rgamma(n_subj, mean_c^2 / var_c, mean_c / var_c),
    kappa = rgamma(n_subj, mean_c^2 / var_c, mean_c / var_c)
  )
}


set.seed(123)
subj_params <- generate_subj_parameters(200)
sim_data <- simulate_hierarhical_sdm_data(subj_params, n_trials = 100)

fit <- bmm(
  formula = bmf(c ~ 1 + (1 | id), kappa ~ 1 + (1 | id)),
  data = sim_data,
  model = sdm(resp_error = "y"),
  backend = "cmdstanr",
  cores = 4,
  iter = 4000
)

saveRDS(list(fit = fit, subj_params = subj_params), "output/sdm_hierarhical_recovery_larger_range.rds")
