
clean_ces <- function(policy_df,
                      cumulative_df,
                      policy_vars,
                      years) {
  out_df <- left_join(policy_df, cumulative_df,
                      by = c("case_id", "year")) %>%
    filter(year %in% years,
           state == "California") %>% 
    mutate(# fix polarity of policy questions
           abortion_20weeks = if_else(abortion_20weeks == 1, 2, 1),
           guns_permits = if_else(guns_permits == 1, 2, 1),
           healthcare_aca = if_else(healthcare_aca == 1, 2, 1),
           immig_border = if_else(immig_border == 1, 2, 1),
           immig_wall = if_else(immig_wall == 1, 2, 1),
           across(all_of(policy_vars), ~ . - 1),
           
           # create unique participant id
           participant = paste0(year, case_id),
           
           # recode postrat vars 
           age = cut(age, breaks = seq(18, 110, length.out = 25)),
           race = if_else(race == 8, 7, race),
           educ = case_when(educ == 1 ~ 1,
                            educ == 2 ~ 2,
                            educ %in% c(3, 4) ~ 3,
                            educ == 5 ~ 4,
                            educ == 6 ~ 5,
                            .default = NA),
           faminc = as.numeric(faminc),
           hhinc = case_when(faminc < 3 ~ 1,
                             faminc < 10 ~ 2,
                             faminc < 13 ~ 3,
                             .default = NA)) %>% 
    select(participant, county_fips, gender, age, race, educ, hhinc,
           all_of(policy_vars)) %>%
    pivot_longer(cols = all_of(policy_vars),
                 names_to = "question",
                 values_to = "response") %>%
    tidyr::drop_na() %>% 
    mutate(question = as.numeric(as.factor(question)))
    
  return(out_df)
}


clean_county_data <- function(county_pres_vote_df) {
  out_df <- county_pres_vote_df %>% 
    filter(state == "CALIFORNIA", year == 2020, candidate == "DONALD J TRUMP") %>% 
    mutate(county_fips = str_pad(county_fips, 5, "0", side = "left"),
           repvote = candidatevotes / totalvotes,
           region = case_when(county_fips %in% c("06115", "06067", "06101",
                                                 "06113", "06017", "06011",
                                                 "06061") ~ "North-Central Valley",
                              county_fips %in% c("06107", "06039", "06025",
                                                 "06019", "06031", "06069",
                                                 "06047", "06077", "06029",
                                                 "06099", "06053", "06079") ~
                                "Central and Southern",
                              county_fips %in% c("06075", "06095", "06001",
                                                 "06085", "06087", "06013",
                                                 "06097", "06055", "06081") ~
                                "Bay Area",
                              county_fips %in% c("06037", "06071", "06073",
                                                 "06111", "06083", "06065",
                                                 "06059") ~ "Southern",
                              .default = "Northern Mountain")) %>% 
    select(county_fips, repvote, region) %>% 
    distinct()
  
  return(out_df)
}
