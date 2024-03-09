
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

tar_load(ideal_mrp_fit_summary_ideal_mrp)
tar_load(ideal_mrp_fit_draws_ideal_mrp)
tar_load(postrat_df)

postrat_df <- postrat_df %>% 
  mutate(cell = row_number())


s <- ideal_mrp_fit_summary_ideal_mrp
s %>% filter(str_detect(variable, "theta\\[")) %>% 
  # ggplot(aes(x = sd)) +
  # geom_histogram()
  arrange(mean) %>% 
  mutate(variable = forcats::fct_inorder(variable)) %>% 
  ggplot(aes(x = mean, y = variable)) +
  geom_pointrange(aes(xmin = q5,
                      xmax = q95), alpha = .5)

df_fit <- ideal_mrp_fit_draws_ideal_mrp

epred_mat = df_fit %>% 
  select(mu_theta, starts_with("theta_pred_raw")) %>% 
  mutate(across(starts_with("theta_pred_raw"), ~.x + mu_theta),
         draw = row_number()) %>% 
  select(-mu_theta) %>% 
  pivot_longer(cols = starts_with("theta_pred_raw"),
               names_to = "cell",
               names_pattern = "theta_pred_raw\\[(\\d+)\\]",
               names_transform = list(cell = as.integer),
               values_to = "theta")


c_draws = left_join(epred_mat, postrat_df, by = "cell") %>% 
  group_by(county_fips, draw) %>% 
  reframe(theta_c = theta * n / sum(n)) %>% 
  mutate(theta_c_mean = mean(theta_c),
         theta_c_se = sd(theta_c),
         .by = county_fips)


c_draws %>% 
  select(theta_c_mean, theta_c_se, county_fips) %>% 
  distinct() %>% 
  arrange(theta_c_mean) %>% 
  mutate(county_fips = forcats::fct_inorder(county_fips)) %>% 
  ggplot(aes(x = theta_c_mean, y = county_fips)) +
  geom_pointrange(aes(xmin = theta_c_mean - 1.96 * theta_c_se,
                      xmax = theta_c_mean + 1.96 * theta_c_se))







ca_county_names = tigris::counties(state = "CA") %>% 
  mutate(county_fips = as.factor(paste0(STATEFP, COUNTYFP))) %>% 
  select(county_fips, name = NAME) %>% 
  as_tibble()
View(ca_county_names)






mrp_estimates_vector <- epred_mat %*% postrat_df$n / sum(postrat_df$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))
cat("MRP estimate mean, sd: ", round(mrp_estimate, 3))


ces %>% 
  reframe(response = sum(response),
          .by = c("participant", "county_fips")) %>% 
  reframe(mean_r = mean(response),
          sd_r = sd(response),
          .by = "county_fips") %>% 
  arrange(mean_r) %>% 
  mutate(county_fips = forcats::fct_inorder(county_fips)) %>% 
  ggplot(aes(x = mean_r, y = county_fips)) +
  geom_pointrange(aes(xmin = mean_r - sd_r,
                      xmax = mean_r + sd_r))

postrat_df$county_fips <- as.factor(postrat_df$county_fips)
  
df_state_idealpoint <- data.frame(county = rep(NA, length(levels(postrat_df$county_fips))),
                                  idealpoint_mean = NA,
                                  idealpoint_sd = NA)
mu_alpha_adj = df_fit %>% 
  select(mu_theta, starts_with("theta_pred_raw")) %>% 
  mutate(across(starts_with("theta_pred_raw"), ~.x + mu_theta)) %>% 
  select(-mu_theta) %>% 
  as.matrix()
i = 1
for(s in levels(postrat_df$county_fips)){
  state_estimates <- (mu_alpha_adj[, which(postrat_df$county_fips==s)] %*% 
                        postrat_df$n[which(postrat_df$county_fips==s)]/
                        sum(postrat_df$n[which(postrat_df$county_fips==s)]) )
  df_state_idealpoint$county[i] <- s
  df_state_idealpoint$idealpoint_mean[i] <- mean(state_estimates)
  df_state_idealpoint$idealpoint_sd[i] <- sd(state_estimates)
  i = i + 1
}
df_state_idealpoint

ca_county_geos %>% 
  mutate(r = row_number()) %>% 
  select(r, everything()) %>% 
  View()
