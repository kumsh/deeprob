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

data{  
  int N;                        // number of individuals  
  vector [N] size_t0;           // size of focal plants  
  vector [N] growth;            // growth of focal plants, response  
  vector [N] sizemat[N];        // full size matrix  
  vector [N] distmat[N];        // full distance matrix  
}
parameters{
  real alpha;                       // Intercept  
  real beta;                        // Effect of target plant size on response  
  real sigma;                       // global SD  
  real<lower=0> a1;                 // size dependent effect of the neighbor    
  real a3;                          // net crowding effect  
  real<lower=0> a2;                 // spatial scale  
  }
transformed parameters{  
  vector[N] kernel;  
  vector[N] mu;
  vector[N] smat[N];
  vector[N] dmat[N];
  for(i in 1:N){
    for(j in 1:N){
     smat[i,j]=sizemat[i,j]^a1;  
     dmat[i,j]=distmat[i,j]^2*a2;
    }}
  for(n in 1:N)
      kernel[n]=sum(smat[n]./exp(dmat[n]));    //we use ./ to perform a vectorial division
  for(n in 1:N)
      mu[n]=alpha+size_t0[n]*beta+a3*kernel[n];
}
model{
  alpha~normal(0,5);   // uninformed priors centered at zero;
  beta~normal(0,5);
  a1~normal(0,5);
  a2~normal(0,5);
  a3~normal(0,5);
  sigma~exponential(1);
  growth ~ normal(mu,sigma);
}
