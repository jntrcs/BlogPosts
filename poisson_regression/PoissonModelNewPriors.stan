
data {
  int<lower=0> N_games;
  int<lower=0> N_refs;
  int bad_calls[N_games];
  vector[N_games] minutes;
  matrix[N_games, N_refs] refs;
}


parameters {
  vector<lower=0>[N_refs] r;
  real<lower=0> theta;
  real<lower=0> sigma;
}

transformed parameters{
  vector<lower=0>[N_games] mu;
  for (i in 1:N_games){
   mu[i] = minutes[i]*dot_product(r, row(refs, i));
  }
}


model {
  for (i in 1:N_games){
    target += poisson_lpmf(bad_calls[i] | mu[i]);
  }
  target += normal_lpdf(r | theta, sigma);
  target += normal_lpdf(theta | .2, .5);
  target += normal_lpdf(sigma | 0, 1);
}
