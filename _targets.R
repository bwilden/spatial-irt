
library(targets)

tar_option_set(
  packages = c("dplyr",
               "here")
)

tar_source()

list(
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
      "immig_wall",
      "trade_china",
      "trade_canmex_include"
    )
  ),
  tar_target(
    ces,
    clean_ces(ces_policy,
              ces_cumulative,
              policy_vars = ces_policy_qs,
              years = 2017:2022)
  )
)
