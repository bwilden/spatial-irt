
irt2_f <- bf(response ~ exp(loggamma) * (theta - beta),
             theta ~ (1 | county_fips) + gender + (1 | race) +
               (1 | age) + (1 | educ),
             beta ~ 1,
             loggamma ~ 1,
             nl = TRUE,
             family = bernoulli(link = "probit"))
irt2_priors <-
  prior(normal(0, 2), class = "b", nlpar = "theta") +
  prior(normal(0, 2), class = "b", nlpar = "beta") +
  prior(normal(0, 1), class = "b", nlpar = "loggamma")
  # prior(constant(1), class = "sd", group = "rid", nlpar = "theta") +
  # prior(normal(0, 2), class = "sd", group = "policy", nlpar = "beta") +
  # prior(normal(0, 1), class = "sd", group = "policy", nlpar = "loggamma")

fit_irt2 <- brm(
  irt2_f,
  data = ces %>% slice_sample(n = 1e3),
  prior = irt2_priors,
  cores = 8,
  threads = threading(2),
  backend = "cmdstanr",
  silent = 0
)
summary(fit_irt2)

ranef(fit_irt2)$rid %>%
  as_tibble() %>%
  arrange(Estimate.theta_Intercept) %>%
  mutate(rid = row_number()) %>%
  ggplot(aes(x = Estimate.theta_Intercept, y = rid)) +
  geom_pointrange(aes(xmin = Q2.5.theta_Intercept,
                      xmax = Q97.5.theta_Intercept),
                  alpha = .05) +
  theme_minimal()



epred_mat <- posterior_epred(fit_irt2, newdata = poststrat_table, draws = 1000,
                             allow_new_levels = TRUE)
mrp_estimates_vector <- epred_mat %*% poststrat_table$n / sum(poststrat_table$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))
mrp_estimate
hist(mrp_estimates_vector)
