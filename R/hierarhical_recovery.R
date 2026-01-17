library(bmm)
library(purrr)

simulate_hierarhical_sdm_data <- function(subj_params, n_trials) {
  y_list <- pmap(subj_params, \(c, kappa) rsdm(n = n_trials, mu = 0, c = c, kappa = kappa))
  data.frame(
    id = rep(seq_len(nrow(subj_params)), each = n_trials),
    y = unlist(y_list)
  )
}

# load Klaus's generating parameters
set.seed(123)
subj_params <- read.csv("output/ko_params.csv")
sim_data <- simulate_hierarhical_sdm_data(subj_params, n_trials = 100)
fit <- bmm(
  formula = bmf(c ~ 1 + (1 | id), kappa ~ 1 + (1 | id)),
  data = sim_data,
  model = sdm(resp_error = "y"),
  backend = "cmdstanr",
  cores = 4,
  iter = 4000
)

saveRDS(list(fit = fit, subj_params = subj_params), "output/sdm_hierarhical_recovery.rds")
