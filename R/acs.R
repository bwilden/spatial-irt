
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
  postrat_df <- ipums_df %>% 
    filter(CITIZEN != 3, AGE >= 18, AGE <= 110) %>% 
    mutate(age = cut(AGE, breaks = seq(18, 110, length.out = 25)),
           age = as.integer(age),
           gender = as.integer(SEX),
           educ = case_when(EDUCD < 63 ~ 1,
                            EDUCD < 65 ~ 2,
                            EDUCD < 101 ~ 3,
                            EDUCD < 114 ~ 4,
                            EDUCD < 999 ~ 5,
                            .default = NA),
           race = case_when(HISPAN > 0 ~ 3,
                            RACE == 1 ~ 1,
                            RACE == 2 ~ 2,
                            RACE == 3 ~ 5,
                            RACE %in% c(4, 5, 6) ~ 4,
                            RACE == 7 ~ 7,
                            RACE %in% c(8, 9) ~ 6,
                            .default = NA),
           hhinc = cut(HHINCOME, breaks = c(-Inf, 2e4, 10e4, 9999998)),
           hhinc = as.integer(hhinc)) %>% 
    tidyr::drop_na() %>% 
    # complete(county_fips, age, gender, educ, race, hhinc, fill = list(PERWT = 0)) %>%
    group_by(county_fips, age, gender, educ, race, hhinc) %>%
    summarise(n = sum(PERWT))
    
  return(postrat_df)
}


calc_scaling_factor <- function(node_object) {
  adj_matrix = sparseMatrix(
    i = node_object$node1,
    j = node_object$node2,
    x = 1,
    symmetric = TRUE
  )
  Q = Diagonal(node_object$group_size, rowSums(adj_matrix)) - adj_matrix
  #Add a small jitter to the diagonal for numerical stability (optional but recommended)
  Q_pert = Q + Diagonal(node_object$group_size) * 
    max(diag(Q)) * sqrt(.Machine$double.eps)
  
  # Compute the diagonal elements of the covariance matrix subject to the 
  # constraint that the entries of the ICAR sum to zero.
  #See the inla.qinv function help for further details.
  Q_inv = inla.qinv(
    Q_pert, 
    constr = list(A = matrix(1, 1, node_object$group_size), e = 0)
  )
  
  #Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  scaling_factor = exp(mean(log(diag(Q_inv))))
  
  return(scaling_factor)
}
