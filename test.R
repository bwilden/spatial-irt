
tar_load(ideal_mrp_fit_draws_ideal_mrp)
tar_load(postrat_df)
tar_load(ces)
tar_load(ca_county_geos)

df_fit = list(
  mu_alpha = ideal_mrp_fit_draws_ideal_mrp$mu_theta,
  alpha_pred_raw = ideal_mrp_fit_draws_ideal_mrp %>% 
    mutate(across(starts_with("theta_pred_raw"), ~.x + mu_theta)) %>% 
    select(starts_with("theta_pred_raw")) %>% 
    as.matrix(),
  sigma_alpha = ideal_mrp_fit_draws_ideal_mrp$sigma_theta,
  county_only_pred = ideal_mrp_fit_draws_ideal_mrp %>% 
    select(starts_with("county_only_pred")) %>%
    pivot_longer(cols = starts_with("county_only_pred"),
                 names_to = "county_fips",
                 names_pattern = "county_only_pred\\[(\\d+)\\]",
                 names_transform = list(cell = as.integer),
                 values_to = "idealpoint") %>% 
    reframe(idealpoint_mean = mean(idealpoint),
            idealpoint_sd = sd(idealpoint),
            .by = county_fips)
)

ndraws <- nrow(df_fit$alpha_pred_raw)
L <- 10000

mu_alpha_adj <- matrix(NA, nrow = ndraws, ncol = nrow(postrat_df))
for(d in 1:ndraws){
  mu_alpha <- df_fit$mu_alpha[d] + df_fit$alpha_pred_raw[d,]
  sample_alphas_pop <- sample(mu_alpha, size = L, prob = postrat_df$n/sum(postrat_df$n), replace = TRUE) 
  sample_alphas_pop <- sample_alphas_pop + rnorm(L, 0, df_fit$sigma_alpha[d])
  mean_alpha <- mean(sample_alphas_pop)
  sd_alpha <- sd(sample_alphas_pop)
  
  mu_alpha_adj[d,] <- (mu_alpha - mean_alpha) / sd_alpha
}
dim(mu_alpha_adj)
# mu_alpha_adj %*% postrat_df$n / sum(postrat_df$n)

ces$county_fips <- as.factor(ces$county_fips)
df_state_idealpoint <- data.frame(
  county_fips = rep(NA, length(levels(ces$county_fips))),
  idealpoint_mean = NA,
  idealpoint_sd = NA
)
postrat_df$county_fips <- as.factor(postrat_df$county_fips)
i = 1
for(s in levels(ces$county_fips)){
  state_estimates <- (mu_alpha_adj[, which(postrat_df$county_fips==s)] %*% 
                        postrat_df$n[which(postrat_df$county_fips==s)]/
                        sum(postrat_df$n[which(postrat_df$county_fips==s)]) )
  df_state_idealpoint$county_fips[i] <- s
  df_state_idealpoint$idealpoint_mean[i] <- mean(state_estimates)
  df_state_idealpoint$idealpoint_sd[i] <- sd(state_estimates)
  i = i + 1
}

df_state_idealpoint <- df_state_idealpoint %>% 
  left_join(ca_county_geos, 
            by = c("county_fips" = "GEOID")) %>% 
  arrange(idealpoint_mean) %>% 
  mutate(name = forcats::fct_inorder(NAME))

df_state_idealpoint %>%
  ggplot(aes(x = idealpoint_mean, y = name)) +
  geom_pointrange(aes(xmin = idealpoint_mean - idealpoint_sd,
                      xmax = idealpoint_mean + idealpoint_sd)) +
  labs(y = "")


df_state_idealpoint %>% 
  ggplot(aes(fill = idealpoint_mean, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_void()
