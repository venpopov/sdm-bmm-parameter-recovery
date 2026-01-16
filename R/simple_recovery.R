library(bmm)
library(purrr)

estimate_sdm <- function(n_trials, mu = 0, c, kappa, ...) {
  fit <- bmm(
    formula = bmf(c ~ 1, kappa ~ 1),
    data = data.frame(y = rsdm(n = n_trials, mu = mu, c = c, kappa = kappa)),
    model = sdm(resp_error = "y"),
    backend = "cmdstanr",
    cores = 4,
    silent = 2,
    iter = 3000,
    ...
  )
  list(est = summary(fit)$fixed[-1, ], divergences = rstan::get_num_divergent(fit$fit))
}

par_grid <- expand.grid(
  n_trials = c(50, 100, 300, 1000),
  c = seq(1, 10, 0.2),
  kappa = seq(1, 10, 0.2)
)

results <- pmap(par_grid, estimate_sdm, .progress = TRUE)
saveRDS(list(results = results, par_grid = par_grid), "output/sdm_simple_recovery_results.rds")
