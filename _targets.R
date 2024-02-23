
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
               "ccesMRPprep",
               "ipumsr"),
  seed = 111
)

options(mc.cores = 8)

tar_source("R")

list(
  tar_target(
    data_years,
    2021:2022
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
    c("abortion_always",
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
    c("SEX", "AGE", "RACE", "HISPAN", "CITIZEN", "EDUC", "COUNTYFIP", "PUMA")
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
    postrat_table,
    make_poststrat_table(ca_ipums_counties)
  ),
  tar_target(
    postrat_df,
    postrat_table %>% 
      filter(county_fips %in% ces$county_fips)
  ),
  
  tar_target(
    ideal_mrp_data_list,
    list(J = length(unique(ces$participant)), 
         K = length(unique(ces$question)), 
         N = nrow(ces), 
         C = length(unique(ces$county_fips)),
         P = nrow(postrat_df),
         A = length(unique(ces$age)),
         R = length(unique(ces$race)),
         E = length(unique(ces$educ)),
         participant = as.numeric(as.factor(ces$participant)), 
         question = as.numeric(ces$question), 
         county = as.numeric(as.factor(ces$county_fips)),
         age = as.numeric(ces$age),
         race = as.numeric(ces$race),
         educ = as.numeric(ces$educ),
         gender = as.numeric(ces$gender),
         postrat_county = as.numeric(as.factor(postrat_df$county_fips)),
         postrat_age = as.numeric(postrat_df$age),
         postrat_race = as.numeric(postrat_df$race),
         postrat_educ = as.numeric(postrat_df$educ),
         postrat_gender = as.numeric(postrat_df$gender),
         y = ces$response)
  ),
  tar_stan_mcmc(
    ideal_mrp_fit,
    stan_file = here::here("stan", "ideal_mrp.stan"),
    data = ideal_mrp_data_list,
    quiet = FALSE, 
    threads_per_chain = 2
  )
)
