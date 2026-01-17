library(brms)
library(posterior)

input <- readRDS("output/sdm_hierarhical_recovery_larger_range.rds")
fit <- input$fit
subj_params <- input$subj_params

summary(fit)
plot(fit)

ranefs <- ranef(fit)$id[, "Estimate", ]
fixefs <- t(as.matrix(fixef(fit)[-1, "Estimate"]))

posterior_means <- as.data.frame(ranefs + fixefs[rep(1, nrow(ranefs)), ])
colnames(posterior_means) <- c("c_est", "kappa_est")


par(mfrow = c(1, 2))

plot(subj_params$kappa, exp(posterior_means$kappa), xlab = "True kappa", ylab = "Posterior estimate")
abline(0, 1)
k_corr <- round(cor(subj_params$k, exp(posterior_means$kappa_est)), 3)
text(2.5, 7, paste0("r = ", k_corr))

plot(subj_params$c, exp(posterior_means$c_est), xlab = "True c", ylab = "Posterior estimate")
abline(0, 1)
c_corr <- round(cor(subj_params$c, exp(posterior_means$c_est)), 3)
text(2.5, 6, paste0("r = ", c_corr))
