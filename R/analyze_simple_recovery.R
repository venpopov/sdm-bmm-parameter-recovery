library(tidyverse)

extract_var_df <- function(est_list, var) {
  est <- lapply(estimates_list, \(x) x[paste0(var, "_Intercept"), ])
  est <- do.call(rbind, est)
  col_suffixes <- c("mean", "stder", "l95", "u95", "rhat", "bulk_ess", "tail_ess")
  colnames(est) <- paste0(var, "_", col_suffixes)
  rownames(est) <- NULL
  est
}

preprocess_posterior_estimates <- function(results) {
  estimates_list <- lapply(results, \(x) x[[1]])
  log_c <- extract_var_df(estimates_lis, "c")
  log_kappa <- extract_var_df(estimates_lis, "kappa")
  divergences <- sapply(results, \(x) x[[2]])
  out <- cbind(
    data.frame(id = seq.int(nrow(log_c))),
    log_c,
    log_kappa,
    data.frame(divergences = divergences)
  )
  out$issues <- out$c_rhat > 1.02 | out$kappa_rhat > 1.02 | out$divergences > 0
  out
}

# load the results
input <- readRDS("output/sdm_simple_recovery_results.rds")
par_grid <- input$par_grid
results <- preprocess_posterior_estimates(input$results)

# combine with generating parameters grid. Note that the estimates are on the log scale
results <- par_grid |>
  mutate(
    log_c = log(c),
    log_kappa = log(kappa)
  ) |>
  cbind(results)

# diagnostics
# 106 out of 8464 posteriors had some convergence issues
sum(results$divergences > 0)
sum(results$c_rhat > 1.02)
sum(results$kappa_rhat > 1.02)
sum(results$issues) # at least on of the three

# similar number of convergence issues regardless of number of trials
table(results$n_trials, results$issues)
table(results$c, results$issues)
table(results$kappa, results$issues)

# most convergence issues are for really low values of c (less than 2), or really high values
issues_counts <- table(results$c, results$kappa, results$issues)[, , 2]
image(x = unique(par_grid$c), y = unique(par_grid$kappa), z = issues_counts, xlab = "c", ylab = "kappa")

# correlation in recovered parameters
cors_by_ntrials <- results |>
  filter(!issues) |>
  group_by(n_trials) |>
  summarize(
    c_cor = cor(log_c, c_mean),
    k_cor = cor(log_kappa, kappa_mean)
  )
cors_by_ntrials

results |>
  filter(!issues) |>
  ggplot(aes(c, exp(c_mean))) +
  geom_point(aes(color = kappa), alpha = 0.5) +
  geom_abline(color = "red") +
  # geom_errorbar(aes(ymin = exp(c_l95), ymax = exp(c_u95))) +
  facet_wrap(~n_trials) +
  theme_bw() +
  scale_color_viridis_c() +
  ggtitle("Overall c recovery across all kappa values, split by number of trials") +
  geom_text(data = cors_by_ntrials, x = 2, y = 20, aes(label = paste0("r = ", round(c_cor, 3)))) +
  ylab("Posterior mean estimate")

results |>
  filter(!issues) |>
  ggplot(aes(kappa, exp(kappa_mean))) +
  geom_point(aes(color = c), alpha = 0.5) +
  geom_abline(color = "red") +
  # geom_errorbar(aes(ymin = exp(c_l95), ymax = exp(c_u95))) +
  facet_wrap(~n_trials) +
  theme_bw() +
  scale_color_viridis_c() +
  ggtitle("Overall kappa recovery across all c values, split by number of trials") +
  geom_text(data = cors_by_ntrials, x = 2, y = 18, aes(label = paste0("r = ", round(k_cor, 3)))) +
  ylab("Posterior mean estimate")


# correlation in a limited kappa range (between 1 and 3)
cors_limited_kappa <- results |>
  filter(!issues, kappa <= 3) |>
  group_by(n_trials) |>
  summarize(
    c_cor = cor(log_c, c_mean),
    k_cor = cor(log_kappa, kappa_mean)
  )
cors_limited_kappa

results |>
  filter(!issues, kappa <= 3) |>
  ggplot(aes(kappa, exp(kappa_mean))) +
  geom_point(aes(color = c), alpha = 0.5) +
  geom_abline(color = "red") +
  # geom_errorbar(aes(ymin = exp(c_l95), ymax = exp(c_u95))) +
  facet_wrap(~n_trials) +
  theme_bw() +
  scale_color_viridis_c() +
  ggtitle("Overall kappa recovery across all c values, split by number of trials") +
  geom_text(data = cors_limited_kappa, x = 1.5, y = 9, aes(label = paste0("r = ", round(k_cor, 3)))) +
  ylab("Posterior mean estimate")
