
assemble_draws <- function(fit_draws) {
  draws_list <- list(
    mu_theta = fit_draws$mu_theta,
    theta_pred_raw = fit_draws %>% 
      mutate(across(starts_with("theta_pred_raw"), ~.x + mu_theta)) %>% 
      select(starts_with("theta_pred_raw")) %>% 
      as.matrix(),
    sigma_theta = fit_draws$sigma_theta,
    county_only_pred = fit_draws %>% 
      select(starts_with("county_only_pred")) %>% 
      pivot_longer(cols = starts_with("county_only_pred"),
                   names_to = "county_fips",
                   names_pattern = "county_only_pred\\[(\\d+)\\]",
                   names_transform = list(cell = as.integer),
                   values_to = "idealpoint") %>% 
      reframe(c_idealpoint_mean = mean(idealpoint),
              c_idealpoint_sd = sd(idealpoint),
              .by = county_fips)
  )
  return(draws_list)
}


poststratify_county_thetas <- function(draws_list, 
                                       survey_data, 
                                       postrat_data,
                                       county_data) {
  # convert raw thetas to theta^adj's
  ndraws <- nrow(draws_list$theta_pred_raw)
  L <- 1e5
  mu_theta_adj <- matrix(NA, nrow = ndraws, ncol = nrow(postrat_data))
  for(d in 1:ndraws) {
    mu_theta <- draws_list$mu_theta[d] + draws_list$theta_pred_raw[d,]
    sample_thetas_pop <- sample(
      mu_theta, 
      size = L, 
      prob = postrat_data$n / sum(postrat_data$n), 
      replace = TRUE
    ) 
    sample_thetas_pop <- sample_thetas_pop + rnorm(L, 0, draws_list$sigma_theta[d])
    mean_theta <- mean(sample_thetas_pop)
    sd_theta <- sd(sample_thetas_pop)
    
    mu_theta_adj[d,] <- (mu_theta - mean_theta) / sd_theta
  }
  
  # perform poststratification
  survey_data$county_fips <- as.factor(survey_data$county_fips)
  county_ideal_df <- data.frame(
    county_fips = rep(NA, length(levels(survey_data$county_fips))),
    idealpoint_mean = NA,
    idealpoint_sd = NA
  )
  postrat_data$county_fips <- as.factor(postrat_data$county_fips)
  i = 1
  for(s in levels(survey_data$county_fips)){
    county_estimates <- (mu_theta_adj[, which(postrat_data$county_fips==s)] %*% 
                          postrat_data$n[which(postrat_data$county_fips==s)]/
                          sum(postrat_data$n[which(postrat_data$county_fips==s)]) )
    county_ideal_df$county_fips[i] <- s
    county_ideal_df$idealpoint_mean[i] <- mean(county_estimates)
    county_ideal_df$idealpoint_sd[i] <- sd(county_estimates)
    i = i + 1
  }
  
  # county_ideal_df <- county_ideal_df %>% 
  #   mutate(county_fips = as.character(row_number())) %>% 
  #   right_join(draws_list$county_only_pred, county_ideal_df,
  #              by = "county_fips") %>% 
  #   mutate(idealpoint_mean = if_else(is.na(idealpoint_mean), 
  #                                    c_idealpoint_mean, idealpoint_mean),
  #          idealpoint_sd = if_else(is.na(idealpoint_sd), 
  #                                  c_idealpoint_sd, idealpoint_sd))
  
  # add county meta data in
  county_ideal_df <- county_ideal_df %>%
    right_join(county_data, by = c("county_fips" = "GEOID")) %>%
    mutate(# messed up polarity
      idealpoint_mean = -idealpoint_mean) %>% 
    arrange(idealpoint_mean) %>%
    mutate(name = forcats::fct_inorder(NAME))
  
  return(county_ideal_df)
}


tally_county_thetas <- function(fit, survey_data) {
  thetas <- coef(fit)$participant %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "participant") %>% 
    as_tibble() %>% 
    select(participant, theta = Estimate.theta_Intercept,
           theta_sd = Est.Error.theta_Intercept)
  
  county_thetas <- thetas %>% 
    left_join(survey_data, by = "participant") %>% 
    summarise(theta_county = mean(theta, na.rm = TRUE),
              theta_sd = sd(theta, na.rm = TRUE),
              .by = county_fips)
  
  return(county_thetas)
}
