# Bayesian Piecewise Linear Model for Aluminium Yield Point
# Exactly following the specified model and priors

library(rstan)
library(ggplot2)
library(dplyr)
library(bayesplot)

# Load data
data <- read.csv("aluminium.csv")
data <- data %>% filter(stress > 0) %>% arrange(strain)

x <- data$strain
y <- data$stress
n <- length(x)

# Candidate breakpoints (unique strain values)
tau_candidates <- sort(unique(x))
n_tau <- length(tau_candidates)

# Stan model matching exact specification
stan_model_code <- "
data {
  int<lower=1> n;                    // Number of observations
  vector[n] x;                       // Strain
  vector[n] y;                       // Stress
  int<lower=1> n_tau;                // Number of candidate breakpoints
  vector[n_tau] tau_candidates;      // Candidate breakpoint values
}

parameters {
  real beta1;                        // Slope before breakpoint
  real beta2;                        // Slope after breakpoint
  real<lower=0> sigma2;              // Variance
  real<lower=0, upper=n_tau> tau_idx_raw; // Index for discrete uniform prior
}

transformed parameters {
  real<lower=0> sigma = sqrt(sigma2);
  int tau_idx = ceil(tau_idx_raw);   // Convert to integer index
  real tau = tau_candidates[tau_idx]; // Actual breakpoint value
  vector[n] mu;
  
  // Piecewise linear mean function with continuity at tau
  for (i in 1:n) {
    if (x[i] <= tau) {
      mu[i] = beta1 * x[i];
    } else {
      mu[i] = beta1 * tau + beta2 * (x[i] - tau);
    }
  }
}

model {
  // Priors exactly as specified
  // beta_j | sigma2, tau ~ N(30000, 20000^2 * sigma2)
  beta1 ~ normal(30000, 20000 * sigma);
  beta2 ~ normal(30000, 20000 * sigma);
  
  // Discrete uniform prior on tau over candidate breakpoints
  // (Implemented via uniform on index)
  target += uniform_lpdf(tau_idx_raw | 0, n_tau);
  
  // sigma2 | tau ~ Inverse-Gamma(4, 13.12)
  sigma2 ~ inv_gamma(4, 13.12);
  
  // Likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[n] log_lik;
  vector[n] y_rep;
  for (i in 1:n) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
    y_rep[i] = normal_rng(mu[i], sigma);
  }
}
"

# Write model to file
writeLines(stan_model_code, "aluminium_breakpoint.stan")

# Prepare data for Stan
stan_data <- list(
  n = n,
  x = x,
  y = y,
  n_tau = n_tau,
  tau_candidates = tau_candidates
)

# Compile and fit
cat("========================================\n")
cat("Fitting Bayesian piecewise linear model\n")
cat("========================================\n")
cat(sprintf("Data: %d observations\n", n))
cat(sprintf("Candidate breakpoints: %d\n", n_tau))
cat("\nSampling...\n")

fit <- stan(
  file = "aluminium_breakpoint.stan",
  data = stan_data,
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 123
)

# Extract posterior samples
posterior <- extract(fit)

# Posterior summaries for breakpoint
tau_samples <- posterior$tau
beta1_samples <- posterior$beta1
beta2_samples <- posterior$beta2
sigma_samples <- posterior$sigma

# Summary function
post_summary <- function(samples, name) {
  data.frame(
    Parameter = name,
    Mean = mean(samples),
    SD = sd(samples),
    `2.5%` = quantile(samples, 0.025),
    `25%` = quantile(samples, 0.25),
    Median = quantile(samples, 0.50),
    `75%` = quantile(samples, 0.75),
    `97.5%` = quantile(samples, 0.975)
  )
}

# Create summary table
summary_table <- rbind(
  post_summary(tau_samples, "Breakpoint (τ) - Yield Point"),
  post_summary(beta1_samples, "β1 (Elastic Modulus)"),
  post_summary(beta2_samples, "β2 (Plastic Modulus)"),
  post_summary(sigma_samples, "σ (Residual SD)")
)

cat("\n========================================\n")
cat("POSTERIOR SUMMARIES\n")
cat("========================================\n")
print(summary_table, digits = 6)

# Posterior probability of breakpoint at each candidate
tau_probs <- table(tau_samples) / length(tau_samples)
tau_probs_df <- data.frame(
  tau = as.numeric(names(tau_probs)),
  prob = as.numeric(tau_probs)
)

# Find most probable breakpoint
most_probable_tau <- tau_probs_df$tau[which.max(tau_probs_df$prob)]
cat(sprintf("\nMost probable breakpoint: %.6f (strain)", most_probable_tau))
cat(sprintf("\nPosterior median breakpoint: %.6f (strain)", median(tau_samples)))
cat(sprintf("\n95%% credible interval: [%.6f, %.6f]\n", 
    quantile(tau_samples, 0.025), quantile(tau_samples, 0.975)))

# Generate fitted posterior mean curve
x_pred <- seq(min(x), max(x), length.out = 500)
n_pred <- length(x_pred)
n_samples <- length(posterior$lp__)

# Compute posterior mean curve
y_pred_matrix <- matrix(NA, nrow = n_samples, ncol = n_pred)

for(i in 1:n_samples) {
  tau_i <- tau_samples[i]
  beta1_i <- beta1_samples[i]
  beta2_i <- beta2_samples[i]
  
  for(j in 1:n_pred) {
    if(x_pred[j] <= tau_i) {
      y_pred_matrix[i, j] <- beta1_i * x_pred[j]
    } else {
      y_pred_matrix[i, j] <- beta1_i * tau_i + beta2_i * (x_pred[j] - tau_i)
    }
  }
}

y_pred_mean <- colMeans(y_pred_matrix)
y_pred_lower <- apply(y_pred_matrix, 2, quantile, probs = 0.025)
y_pred_upper <- apply(y_pred_matrix, 2, quantile, probs = 0.975)

# Create plot
plot_data <- data.frame(
  strain = x_pred,
  mean = y_pred_mean,
  lower = y_pred_lower,
  upper = y_pred_upper
)

# Add breakpoint uncertainty ribbon for x-axis
tau_credible <- data.frame(
  tau_median = median(tau_samples),
  tau_lower = quantile(tau_samples, 0.025),
  tau_upper = quantile(tau_samples, 0.975)
)

p <- ggplot() +
  # 95% credible interval for mean curve
  geom_ribbon(data = plot_data, 
              aes(x = strain, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.5) +
  # Posterior mean curve
  geom_line(data = plot_data, 
            aes(x = strain, y = mean), 
            color = "blue", size = 1.2) +
  # Observed data
  geom_point(data = data.frame(x = x, y = y), 
             aes(x = x, y = y), alpha = 0.4, size = 0.8) +
  # Breakpoint median
  geom_vline(xintercept = tau_credible$tau_median, 
             color = "red", linetype = "dashed", size = 1) +
  # Breakpoint credible interval
  geom_rect(aes(xmin = tau_credible$tau_lower, 
                xmax = tau_credible$tau_upper,
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.05) +
  labs(title = "Aluminium Stress-Strain with Bayesian Breakpoint",
       subtitle = paste0("Estimated yield point (τ) = ", 
                         round(tau_credible$tau_median, 6),
                         " [95% CI: ", round(tau_credible$tau_lower, 6),
                         " - ", round(tau_credible$tau_upper, 6), "]"),
       x = "Strain",
       y = "Stress (MPa)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Save plot
ggsave("aluminium_breakpoint_plot.png", p, width = 10, height = 6)
cat("\nPlot saved: aluminium_breakpoint_plot.png\n")

# Save posterior summaries
write.csv(summary_table, "aluminium_posterior_summaries.csv", row.names = FALSE)

# Save breakpoint probability distribution
write.csv(tau_probs_df, "breakpoint_probabilities.csv", row.names = FALSE)

# Plot breakpoint posterior probability
tau_prob_plot <- ggplot(tau_probs_df, aes(x = tau, y = prob)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median(tau_samples), color = "red", linetype = "dashed") +
  labs(title = "Posterior Probability of Breakpoint Location",
       x = "Strain at Breakpoint (τ)", y = "Posterior Probability") +
  theme_minimal()
ggsave("breakpoint_posterior_probability.png", tau_prob_plot, width = 8, height = 5)

# Convergence diagnostics
cat("\n========================================\n")
cat("CONVERGENCE DIAGNOSTICS\n")
cat("========================================\n")
fit_summary <- summary(fit)$summary
rhat_values <- fit_summary[c("beta1", "beta2", "sigma2", "tau_idx_raw"), "Rhat"]
print(rhat_values)
cat("\nAll R-hat < 1.1 indicates convergence\n")

# Save fitted model
saveRDS(fit, "aluminium_fit.rds")

cat("\n========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
