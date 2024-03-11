
data {
  int<lower=1> J; // Participants
  int<lower=1> K; // Questions
  int<lower=1> N; // no. of observations (Participant/Question Pair)
  int<lower=1> N_ages; // no. of age categories
  int<lower=1> N_races; // no. of race categories
  int<lower=1> N_educs; // no. of education categories
  int<lower=1> N_hhincs; // no. of hh income categories
  int<lower=1> N_regions; // no. of regions
  
  int<lower=1> P_cells; // no. of poststrat cells
    
  array[N] int<lower=1, upper=J> participant; // Participant for observation n
  array[N] int<lower=1, upper=K> question; // Question for observation n
  array[N] int<lower=1, upper=58> county; // County for observation n
  array[N] int<lower=1, upper=N_ages> age; // Age for observation n
  array[N] int<lower=1, upper=N_races> race; // Race for observation n
  array[N] int<lower=1, upper=N_educs> educ; // Education for observation n
  array[N] int<lower=1, upper=N_hhincs> hhinc; // Household Income for observation n
  array[N] real<lower=1, upper=2> gender; // Gender for observation n
  
  array[58] int<lower=1, upper=N_regions> region; // region for county
  vector[58] repvote; // republican voteshare for county
  
  array[N] int<lower=0, upper=1> y; // Support for observation n
  
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=58> node1;
  array[N_edges] int<lower=1, upper=58> node2;
  
  real<lower=0> scaling_factor;
    
  array[P_cells] int<lower=1, upper=58> postrat_county;
  array[P_cells] int<lower=1, upper=N_ages> postrat_age;
  array[P_cells] int<lower=1, upper=N_races> postrat_race;
  array[P_cells] int<lower=1, upper=N_educs> postrat_educ;
  array[P_cells] int<lower=1, upper=N_hhincs> postrat_hhinc;
  array[P_cells] real<lower=1, upper=2> postrat_gender;
}
parameters {
  vector[N_ages] theta_age_raw;
  vector[N_races] theta_race_raw;
  vector[N_educs] theta_educ_raw;
  vector[N_hhincs] theta_hhinc_raw;
  real beta_gender;
  
  vector[N_regions] theta_region_raw;
  real beta_repvote;
  
  vector[58] theta_county_raw;
  vector[58] phi;
  real<lower=0, upper=1> rho;
  
  real<lower=0> sigma_county;
  real<lower=0> sigma_age;
  real<lower=0> sigma_race;
  real<lower=0> sigma_educ;
  real<lower=0> sigma_hhinc;
  real<lower=0> sigma_region;

  real mu_theta;
  real<lower=0> sigma_theta;
  real mu_beta;
  real<lower=0> sigma_beta;
  real<lower=0> mu_gamma;
  real<lower=0> sigma_gamma;

  vector[K] beta_raw;
  vector[J] theta_raw;
  vector<lower=0>[K] gamma_raw;
}
transformed parameters{
  vector[N_ages] theta_age = 0 + sigma_age*theta_age_raw;
  vector[N_races] theta_race = 0 + sigma_race*theta_race_raw;
  vector[N_educs] theta_educ = 0 + sigma_educ*theta_educ_raw;
  vector[N_hhincs] theta_hhinc = 0 + sigma_hhinc*theta_hhinc_raw;
  vector[N_regions] theta_region = 0 + sigma_region*theta_region_raw;
  
  vector[K] beta = mu_beta + sigma_beta*beta_raw;
  vector[K] gamma = mu_gamma + sigma_gamma*gamma_raw;

  vector[58] convolved_re;
  convolved_re = sqrt(1 - rho) * theta_county_raw + sqrt(rho / scaling_factor) * phi;

  vector[J] theta;
  vector[58] theta_county;
  
  real theta_mean;
  real theta_sd;
  vector[J] theta_adj;
  vector[K] beta_adj;
  vector<lower=0>[K] gamma_adj;
  
  for(c in 1:58) {
    theta_county[c] = theta_region[region[c]] + 
    beta_repvote*repvote[c] + convolved_re[c] * sigma_county;
  }
  for (j in 1:J) {
    theta[j] = mu_theta + theta_county[county[j]] + theta_age[age[j]] + 
    theta_race[race[j]] + theta_educ[educ[j]] + theta_hhinc[hhinc[j]] +
    beta_gender*gender[j] + theta_raw[j] * sigma_theta;
  }

  theta_mean = mean(theta);
  theta_sd = sd(theta);
  theta_adj = (theta - theta_mean)/theta_sd;
  beta_adj = (beta - theta_mean)/theta_sd;
  gamma_adj = gamma*theta_sd;
}

model {
  //priors on predictors
  sigma_county ~ normal(0, 1); // prior for sigma_county
  sigma_age ~ normal(0, 1); // prior for sigma_age
  sigma_race ~ normal(0, 1); // prior for sigma_race
  sigma_educ ~ normal(0, 1); // prior for sigma_educ
  sigma_hhinc ~ normal(0, 1);
  sigma_region ~ normal(0, 1);
  
  beta_gender ~ normal(0, 2); // prior for beta_gender
  beta_repvote ~ normal(0, 2);

  //priors on parameters
  mu_beta ~ normal(0, 2); // prior for mu_beta
  sigma_beta ~ normal(0, 1); // prior for sigma_beta
  mu_gamma ~ normal(0, 2); // prior for mu_gamma
  sigma_gamma ~ normal(0, 1); // prior for sigma_gamma

  theta_county_raw ~ std_normal();
  theta_age_raw ~ std_normal(); // implies theta_age ~ normal(0, sigma_age)
  theta_race_raw ~ std_normal(); // implies theta_race ~ normal(0, sigma_race)
  theta_educ_raw ~ std_normal(); // implies theta_educ ~ normal(0, sigma_educ)
  theta_hhinc_raw ~ std_normal();
  theta_region_raw ~ std_normal();

  gamma_raw ~ std_normal(); // implies beta ~ normal(mu_beta, sigma_beta)
  beta_raw ~ std_normal(); // implies beta ~ normal(mu_beta, sigma_beta)
  theta_raw ~ std_normal(); // implies theta ~ normal(mu_theta + theta_county + theta_age + ..., sigma_theta)
  
  for (a in 2:N_ages) {
    theta_age_raw[a] ~ normal(theta_age_raw[a - 1], 1);
  }
  sum(theta_age_raw) ~ normal(0, 0.01 * N_ages);
  
  rho ~ beta(0.5, 0.5);
  target += -0.5 * dot_self(phi[node1] - phi[node2]);
  sum(phi) ~ normal(0, 0.01 * 58);
  
  for (n in 1:N)
    y[n] ~ bernoulli_logit(gamma_adj[question[n]] * 
    (theta_adj[participant[n]] - beta_adj[question[n]]));
}

generated quantities{
  vector[P_cells] theta_pred_raw;
  for (p in 1:P_cells)
    theta_pred_raw[p] = theta_county[postrat_county[p]] + 
    theta_age[postrat_age[p]] + theta_race[postrat_race[p]] + 
    theta_educ[postrat_educ[p]] + theta_hhinc[postrat_hhinc[p]] +
    beta_gender*postrat_gender[p];
    
  vector[58] county_only_pred;
  for (c in 1:58)
    county_only_pred[c] = theta_region[region[c]] + beta_repvote*repvote[c];
}
