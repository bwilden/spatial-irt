
library(targets)
library(stantargets)

tar_option_set(
  packages = c("dplyr",
               "tidyr",
               "purrr",
               "here",
               "brms",
               "tidybayes",
               "stringr",
               "ipumsr",
               "INLA",
               "geostan"),
  seed = 123
)

options(mc.cores = 8)

tar_source("R")

list(
  tar_target(
    data_years,
    2019:2021
  ),
  tar_target(
    county_pres_vote_file,
    here::here("data-raw", "countypres_2000-2020.csv"),
    format = "file"
  ),
  tar_target(
    county_pres_vote,
    readr::read_csv(county_pres_vote_file)
  ),
  tar_target(
    county_data,
    clean_county_data(county_pres_vote) 
  ),
  tar_target(
    ces_policy_file,
    here::here("data-raw", "cumulative_ces_policy_preferences.tab"),
    format = "file"
  ),
  tar_target(
    ces_policy,
    readr::read_delim(ces_policy_file)
  ),
  tar_target(
    ces_cumulative_file,
    here::here("data-raw", "cumulative_2006-2022.rds"),
    format = "file"
  ),
  tar_target(
    ces_cumulative,
    readRDS(ces_cumulative_file)
  ),
  tar_target(
    ces_policy_qs,
    c(
      "abortion_always",
      "abortion_20weeks",
      "enviro_carbon",
      "enviro_renewable",
      "enviro_airwateracts",
      "guns_bgchecks",
      "guns_assaultban",
      "guns_permits",
      "healthcare_aca",
      "healthcare_medicare",
      "immig_legalize",
      "immig_border",
      "immig_wall"
    )
  ),
  tar_target(
    ces,
    clean_ces(ces_policy,
              ces_cumulative,
              policy_vars = ces_policy_qs,
              years = data_years)
  ),
  
  # Create poststratification table
  tar_target(
    poststrat_vars,
    c("SEX", "AGE", "RACE", "HISPAN", "CITIZEN", "EDUC", "HHINCOME", "PUMA")
  ),
  tar_target(
    ca_ipums,
    load_ipums(poststrat_vars)
  ),
  tar_target(
   puma_county_crosswalk_file,
   here::here("data-raw", "geocorr2022_2405102579.csv"),
   format = "file"
  ),
  tar_target(
    puma_county_crosswalk,
    readr::read_csv(puma_county_crosswalk_file)[-1,]
  ),
  tar_target(
    ca_ipums_counties,
    allocate_pumas_to_counties(ca_ipums,
                               puma_county_crosswalk)
  ),
  tar_target(
    postrat_df,
    make_poststrat_table(ca_ipums_counties) %>% 
      filter(county_fips %in% ces$county_fips)
  ),
  
  tar_target(
    ca_county_geos,
    tigris::counties(state = "CA")
  ),
  tar_target(
    county_nodes,
    ca_county_geos %>% 
      arrange(NAME) %>% 
      geostan::shape2mat(style = "B") %>% 
      geostan::prep_icar_data()
  ),
  tar_target(
    scaling_factor,
    calc_scaling_factor(county_nodes)
  ),
  
  tar_target(
    ideal_mrp_data_list,
    list(J = length(unique(ces$participant)), 
         K = length(unique(ces$question)), 
         N = nrow(ces), 
         N_ages = length(unique(ces$age)),
         N_races = length(unique(ces$race)),
         N_educs = length(unique(ces$educ)),
         N_hhincs = length(unique(ces$hhinc)),
         N_regions = length(unique(county_data$region)),
         
         P_cells = nrow(postrat_df),
         
         participant = as.numeric(as.factor(ces$participant)), 
         question = as.numeric(ces$question), 
         county = as.numeric(as.factor(ces$county_fips)),
         age = as.numeric(ces$age),
         race = as.numeric(ces$race),
         educ = as.numeric(ces$educ),
         hhinc = as.numeric(ces$hhinc),
         gender = as.numeric(ces$gender),
         
         region = as.numeric(as.factor(county_data$region)),
         repvote = county_data$repvote,
         
         y = ces$response,
         
         N_edges = county_nodes$n_edges,
         node1 = county_nodes$node1,
         node2 = county_nodes$node2,
         scaling_factor = scaling_factor,
         
         postrat_county = as.numeric(as.factor(postrat_df$county_fips)),
         postrat_age = postrat_df$age,
         postrat_race = postrat_df$race,
         postrat_educ = postrat_df$educ,
         postrat_hhinc = postrat_df$hhinc,
         postrat_gender = postrat_df$gender)
  ),
  tar_stan_mcmc(
    ideal_mrp_fit,
    stan_file = here::here("stan", "ideal_mrp.stan"),
    data = ideal_mrp_data_list,
    quiet = FALSE, 
    threads_per_chain = 2,
    cpp_options = list(stan_threads = TRUE),
    # adapt_delta = .95
  )
)
