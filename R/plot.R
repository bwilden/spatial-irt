
make_county_est_plot <- function(county_ideal_df) {
  p <- county_ideal_df %>%
    ggplot(aes(x = idealpoint_mean, y = name, color = idealpoint_mean)) +
    geom_pointrange(aes(xmin = idealpoint_mean - idealpoint_sd,
                        xmax = idealpoint_mean + idealpoint_sd)) +
    scale_color_gradientn(
      colours = c(met.brewer("Isfahan1")[8], 
                  "grey", 
                  met.brewer("Isfahan1")[1])
    ) +
    labs(y = "", x = expression(theta[c]^{MRP}),
         caption = "Mean posterior estimates and standard errors") +
    theme_ggdist() +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  
  return(p)
}
# make_county_est_plot(county_ideal_df)
# make_county_est_plot(draws_list$county_only_pred %>%
#                        arrange(idealpoint_mean) %>%
#                        mutate(name = forcats::fct_inorder(county_fips)))

make_county_map <- function(plot_data_long,
                            estimate_type) {
  p <- plot_data_long %>% 
    ggplot(aes(fill = mean, geometry = geometry)) +
    geom_sf(color = "gray30") +
    scale_fill_gradientn(
      colours = c(met.brewer("Isfahan1")[8], 
                  # met.brewer("Isfahan1")[6],
                  "gray",
                  met.brewer("Isfahan1")[2]),
      values = c(0, .37, 1),
    ) +
    theme_void() +
    labs(fill = estimate_type) +
    theme(legend.position = "bottom",
          text = element_text(family = "serif"))
  
  return(p)
}

