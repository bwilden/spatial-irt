
load_ipums <- function(ipums_vars) {
  ipums_extract <- define_extract_usa(
    description = "IPUMS data for Postratification Table",
    samples = "us2021c",
    variables = append(
      ipums_vars,
      list(var_spec("STATEFIP", case_selections = "06"))
    ),
  ) %>% 
    submit_extract() %>% 
    wait_for_extract() %>% 
    download_extract(download_dir = here::here("data-raw", "ipums"),
                     overwrite = TRUE) %>% 
    read_ipums_micro()
  
  return(ipums_extract)
}


pad_out_geographies <- purrr::partial(
  dplyr::mutate,
  STATEFIP = str_pad(STATEFIP, width = 2, "left", pad = "0"),
  COUNTYFIP = str_pad(COUNTYFIP, width = 3, "left", pad = "0"),
  PUMA = str_pad(PUMA, width = 5, "left", pad = "0")
)


allocate_pumas_to_counties <- function(ipums_df, crosswalk_df) {
  out_df <- ipums_df %>% 
    pad_out_geographies() %>% 
    left_join(crosswalk_df %>% 
                select(PUMA = puma12, county, afact), 
              by = "PUMA") %>% 
    group_by(YEAR, SERIAL) %>% 
    mutate(county_fips = sample(county, 1, prob = afact)) %>% 
    ungroup() %>% 
    select(-c(afact, county)) %>% 
    distinct() %>%
    assertr::verify(nrow(.) == nrow(ipums_df))
  
  return(out_df)
}


make_poststrat_table <- function(ipums_df) {
  poststrat_df <- ipums_df %>% 
    filter(CITIZEN != 3) %>% 
    mutate(age = case_when(AGE >= 18 & AGE < 25 ~ 1,
                           AGE >= 25 & AGE < 35 ~ 2,
                           AGE >= 35 & AGE < 45 ~ 3,
                           AGE >= 45 & AGE < 65 ~ 4,
                           AGE >= 65 ~ 5,
                           .default = NA),
           age = as.character(age),
           gender = as.character(SEX),
           educ = case_when(EDUCD <= 64 & EDUCD > 1 ~ 1,
                            EDUCD <= 81 & EDUCD > 65 ~ 2,
                            EDUCD == 101 ~ 3,
                            EDUCD > 101 ~ 4,
                            .default = NA),
           educ = as.character(educ),
           race = case_when(HISPAN > 0 ~ 3,
                            RACE == 1 ~ 1,
                            RACE == 2 ~ 2,
                            RACE == 3 ~ 5,
                            RACE %in% c(4, 5, 6) ~ 4,
                            .default = 6),
           race = as.character(educ)) %>% 
    filter(!is.na(age), !is.na(educ)) %>%
    group_by(county_fips, age, gender, educ, race) %>% 
    summarise(n = sum(PERWT)) %>% 
    ungroup()
    
  return(poststrat_df)
}
