data {
  int<lower=1> N;
  int<lower=1> K; // K counties
  array[N] int<lower=1, upper=K> county;
  int<lower=1> N_edges;
  array[N_edges] int<lower=1, upper=K> node1;
  array[N_edges] int<lower=1, upper=K> node2; 
  
  vector[N] x;
  vector[N] y;
  
  real<lower=0> scaling_factor;
}
parameters {
  real<lower=0, upper=1> rho;
  vector[K] phi;
  vector[K] theta;
  
  real beta;
  real alpha;
  
  vector[K] county_alpha;
  real<lower=0> county_sigma;
  
  real<lower=0> sigma;
}
transformed parameters {
  vector[K] convolved_re;
  convolved_re = sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
}
model {
  rho ~ beta(0.5, 0.5);
  sigma ~ normal(0, 1);
  theta ~ normal(0, 2);
  beta ~ normal(0, 2);
  alpha ~ normal(0, 2);
  county_sigma ~ normal(0, 1);
  
  for (k in 1:K) {
    county_alpha[k] ~ normal(convolved_re[k], county_sigma);
  }
  
  target += -0.5 * dot_self(phi[node1] - phi[node2]);
  sum(phi) ~ normal(0, 0.01 * K);

  y ~ normal(alpha + county_alpha[county] + x * beta, sigma);
}
