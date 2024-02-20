
library(targets)

tar_option_set(
  packages = c("dplyr",
               "tidyr",
               "purrr",
               "here",
               "brms",
               "tidybayes",
               "stringr",
               "ccesMRPprep",
               "ipumsr")
)

tar_source()

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
    poststrat_table,
    make_poststrat_table(ca_ipums_counties)
  )
)
