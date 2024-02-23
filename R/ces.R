
clean_ces <- function(policy_df,
                      cumulative_df,
                      policy_vars,
                      years) {
  out_df <- left_join(policy_df, cumulative_df,
                      by = c("case_id", "year")) %>% 
    filter(year %in% years,
           state == "California") %>% 
    mutate(abortion_20weeks = if_else(abortion_20weeks == 1, 2, 1),
           guns_permits = if_else(guns_permits == 1, 2, 1),
           healthcare_aca = if_else(healthcare_aca == 1, 2, 1),
           immig_border = if_else(immig_border == 1, 2, 1),
           immig_wall = if_else(immig_wall == 1, 2, 1),
           across(all_of(policy_vars), ~ . - 1),
           participant = paste0(year, case_id)) %>% 
    select(participant, year, st, state, cd, county_fips, ideo5, gender, age, race, 
           educ, hispanic,
           all_of(policy_vars)) %>%
    pivot_longer(cols = all_of(policy_vars),
                 names_to = "question",
                 values_to = "response") %>% 
    mutate(question = as.numeric(as.factor(question))) %>% 
    ccc_std_demographics() %>% 
    select(participant, question, county_fips, age, race, educ, gender, response) %>% 
    tidyr::drop_na()
    
  return(out_df)
}
# clean_ces(policy_df = ces_policy,
#           cumulative_df = ces_cumulative,
#           2017:2022)

