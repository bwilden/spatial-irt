library(geostan)
library(dplyr)
library(cmdstanr)
library(INLA)

georgia_long = georgia %>%
  select(GEOID, income, college) %>% 
  tidyr::crossing(x = 1:10) %>%
  mutate(x = rnorm(n(), mean = college),
         .by = GEOID) %>% 
  slice_sample(prop = .75) %>%
  sf::st_as_sf()

C <- shape2mat(georgia, "B")
nbs <- prep_icar_data(C)

#Build the adjacency matrix using INLA library functions
adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)
#The ICAR precision matrix (note! This is singular)
Q =  Diagonal(nbs$group_size, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(nbs$group_size) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$group_size),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))


icar_model = cmdstan_model("icar.stan")
icar_fit = icar_model$sample(
  data = list(
    N = nrow(georgia_long),
    K = length(unique(georgia_long$GEOID)),
    county = as.numeric(as.factor(georgia_long$GEOID)),
    N_edges = nodes$n_edges,
    node1 = nodes$node1,
    node2 = nodes$node2,
    scaling_factor = scaling_factor,
    x = georgia_long$x,
    y = georgia_long$income
  ),
  parallel_chains = 4,
  adapt_delta = 0.95,
  max_treedepth = 12
)
icar_fit$summary()


c(87.5, 92.5, 98.5, 95.5, 100, 100, 101, 98, 96.5, 92.5, 96.5, 98.5, 95, 96, 91,
  88, 70, 98.5, 95.5, 100, 95, 100, 97.5, 89.5, 98, 94) %>% median()
