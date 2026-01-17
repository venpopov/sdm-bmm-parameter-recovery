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

# load Klaus's generating parameters
set.seed(123)
subj_params <- read.csv("output/ko_params.csv")
subj_params$n_trials <- 100

results <- pmap(subj_params, estimate_sdm, .progress = TRUE)
saveRDS(list(results = results, par_grid = subj_params), "output/ko_bysubject_unpooled.rds")
