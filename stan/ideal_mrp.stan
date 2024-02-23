
data {
  int<lower=1> J; // Participants
  int<lower=1> K; // Questions
  int<lower=1> N; // no. of observations
  int<lower=1> C; // no. of counties
  int<lower=1> P; // no. of poststrat cells
  int<lower=1> A; // no. of age categories
  int<lower=1> R; // no. of race categories
  int<lower=1> E; // no. of education categories
  int<lower=1, upper=J> participant[N]; // Participant for observation n
  int<lower=1, upper=K> question[N]; // Question for observation n
  int<lower=1, upper=C> county[N]; // County for observation n
  int<lower=1, upper=A> age[N]; // Age for observation n
  int<lower=1, upper=R> race[N]; // Race for observation n
  int<lower=1, upper=E> educ[N]; // Education for observation n
  real<lower=1, upper=2> gender[N]; // Gender for observation n
  int<lower=0, upper=1> y[N]; // Support for observation n
  int<lower=1, upper=C> postrat_county[P];
  int<lower=1, upper=A> postrat_age[P];
  int<lower=1, upper=R> postrat_race[P];
  int<lower=1, upper=E> postrat_educ[P];
  real<lower=1, upper=2> postrat_gender[P];
}
parameters {
  vector[C] theta_county_raw;
  vector[A] theta_age_raw;
  vector[R] theta_race_raw;
  vector[E] theta_educ_raw;
  real beta_gender;
  real<lower=0> sigma_county;
  real<lower=0> sigma_age;
  real<lower=0> sigma_race;
  real<lower=0> sigma_educ;

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
  vector[A] theta_age = 0 + sigma_age*theta_age_raw;
  vector[E] theta_educ = 0 + sigma_educ*theta_educ_raw;
  vector[R] theta_race = 0 + sigma_race*theta_race_raw;
  vector[K] beta = mu_beta + sigma_beta*beta_raw;
  vector[K] gamma = mu_gamma + sigma_gamma*gamma_raw;

  vector[C] theta_county;
  vector[J] theta;

  // real theta_mean;
  // real theta_sd;
  // vector[J] theta_adj;
  // vector[K] beta_adj;
  // vector<lower=0>[K] gamma_adj;

  for(c in 1:C)
    theta_county[c] = sigma_county*theta_county_raw[c];
  for (j in 1:J)
    theta[j] = mu_theta + theta_county[county[j]] + theta_age[age[j]] + 
    theta_race[race[j]] + theta_educ[educ[j]] + beta_gender*gender[j] + 
    sigma_theta*theta_raw[j];

  // theta_mean = mean(theta);
  // theta_sd = sd(theta);
  // theta_adj = (theta - theta_mean)/theta_sd;
  // beta_adj = (beta - theta_mean)/theta_sd;
  // gamma_adj = gamma*theta_sd;
}



model {
  //priors on predictors
  sigma_county ~ exponential(0.5); // prior for sigma_county
  sigma_age ~ exponential(0.5); // prior for sigma_age
  sigma_race ~ exponential(0.5); // prior for sigma_race
  sigma_educ ~ exponential(0.5); // prior for sigma_educ
  beta_gender ~ normal(0, 2); // prior for beta_male

  //priors on parameters
  mu_beta ~ normal(0, 2); // prior for mu_beta
  sigma_beta ~ exponential(1); // prior for sigma_beta
  mu_gamma ~ normal(0, 2); // prior for mu_gamma
  sigma_gamma ~ exponential(1); // prior for sigma_gamma

  theta_county_raw ~ std_normal(); // implies theta_county ~ normal(theta_region, sigma_county)
  theta_age_raw ~ std_normal(); // implies theta_age ~ normal(0, sigma_age)
  theta_race_raw ~ std_normal(); // implies theta_race ~ normal(0, sigma_race)
  theta_educ_raw ~ std_normal(); // implies theta_educ ~ normal(0, sigma_educ)

  gamma_raw ~ std_normal(); // implies beta ~ normal(mu_beta, sigma_beta)
  beta_raw ~ std_normal(); // implies beta ~ normal(mu_beta, sigma_beta)
  theta_raw ~ std_normal(); // implies theta ~ normal(mu_theta + theta_county + theta_age + ..., sigma_theta)
  for (n in 1:N)
    y[n] ~ bernoulli_logit(gamma[question[n]] * (theta[participant[n]] - beta[question[n]]));
}

generated quantities{
  vector[P] theta_pred_raw;
  vector[P] theta_pred;


  for (p in 1:P)
    theta_pred_raw[p] = theta_county[postrat_county[p]] + 
    theta_age[postrat_age[p]] + theta_race[postrat_race[p]] + 
    theta_educ[postrat_educ[p]] + beta_gender*postrat_gender[p];

}
