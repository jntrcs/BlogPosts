//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N_games;
  int<lower=0> N_refs;
  int bad_calls[N_games];
  vector[N_games] minutes;
  matrix[N_games, N_refs] refs;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector<lower=0>[N_refs] r;
  real<lower=0> theta;
}

transformed parameters{
  vector<lower=0>[N_games] mu;
  for (i in 1:N_games){
   mu[i] = minutes[i]*dot_product(r, row(refs, i));
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:N_games){
    target += poisson_lpmf(bad_calls[i] | mu[i]);
  }
  target += exponential_lpdf(r | theta);
  target += exponential_lpdf(theta | 10);

}

