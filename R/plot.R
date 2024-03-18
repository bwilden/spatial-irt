
make_county_est_plot <- function(county_ideal_df) {
  p <- county_ideal_df %>%
    ggplot(aes(x = idealpoint_mean, y = name)) +
    geom_pointrange(aes(xmin = idealpoint_mean - idealpoint_sd,
                        xmax = idealpoint_mean + idealpoint_sd)) +
    labs(y = "")
  
  return(p)
}


make_county_map <- function(county_ideal_df) {
  p <- county_ideal_df %>% 
    ggplot(aes(fill = idealpoint_mean, geometry = geometry)) +
    geom_sf() +
    scale_fill_viridis_c() +
    theme_void()
  
  return(p)
}


