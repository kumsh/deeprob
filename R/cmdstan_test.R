library(bayesplot)
file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()
mod$exe_file()

# name correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

#fit model
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)
fit$summary()

#this is a draws_array object from the posterior package
draws_array <- fit$draws()
str(draws_array)

#convert to matrix or dataframe
draws_df <- as_draws_df(draws_array)
print(draws_df)

mcmc_hist(fit$draws("theta"))

fit$cmdstan_diagnose()
fit$cmdstan_summary()

stanfit <- rstan::read_stan_csv(fit$output_files())

#optimize

fit_mle <- mod$optimize(data = data_list, seed = 123)
fit_mle$summary()

fit_vb <- mod$variational(data = data_list, seed = 123, output_samples = 40000)
fit_vb$summary("theta")

bayesplot_grid(
  mcmc_hist(fit$draws("theta"), bindwidth = 0.025),
  mcmc_hist(fit_vb$draws("theta"), binwidth = 0.025),
  titles = c("posterior distribution from MCMC", "approximate posterior from vb"),
  xlim = c(0,1)
)
