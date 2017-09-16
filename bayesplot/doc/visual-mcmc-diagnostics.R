## ---- SETTINGS-knitr, include=FALSE--------------------------------------
stopifnot(require("knitr"))
library("bayesplot")
knitr::opts_chunk$set(
  dev = "png",
  dpi = 150,
  fig.asp = 0.618,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center",
  comment = NA
)

## ---- pkgs, include=FALSE------------------------------------------------
library("ggplot2")
library("rstan")

## ---- eval=FALSE---------------------------------------------------------
#  library("bayesplot")
#  library("ggplot2")
#  library("rstan")

## ---- schools_dat--------------------------------------------------------
schools_dat <- list(
  J = 8, 
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

## ---- compile-models, eval=FALSE-----------------------------------------
#  schools_mod_cp <- stan_model("schools_mod_cp.stan")
#  schools_mod_ncp <- stan_model("schools_mod_ncp.stan")

## ---- fit-models-hidden, results='hide', message=FALSE-------------------
fit_cp <- sampling(schools_mod_cp, data = schools_dat)
fit_ncp <- sampling(schools_mod_ncp, data = schools_dat)

## ---- extract-draws------------------------------------------------------
# Extract posterior draws for later use
posterior_cp <- as.array(fit_cp)
posterior_ncp <- as.array(fit_ncp)

## ---- results='hide'-----------------------------------------------------
fit_cp_100iter <- sampling(schools_mod_cp, data = schools_dat, 
                           chains = 2, iter = 100)

## ----print-rhats---------------------------------------------------------
rhats <- rhat(fit_cp_100iter)
print(rhats)

## ----mcmc_rhat-1---------------------------------------------------------
color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats)

## ---- mcmc_rhat-2--------------------------------------------------------
mcmc_rhat(rhats) + yaxis_text(hjust = 1)

## ---- mcmc_rhat-3--------------------------------------------------------
mcmc_rhat(rhat = rhat(fit_cp)) + yaxis_text(hjust = 0)

## ----print-neff-ratios---------------------------------------------------
ratios_cp <- neff_ratio(fit_cp)
print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

## ----mcmc_neff-compare---------------------------------------------------
# A function we'll use several times to plot comparisons of the centered 
# parameterization (cp) and the non-centered parameterization (ncp). See
# help("bayesplot_grid") for details on the bayesplot_grid function used here.
compare_cp_ncp <- function(cp_plot, ncp_plot, ncol = 2) {
  bayesplot_grid(
    cp_plot, ncp_plot, 
    grid_args = list(ncol = ncol),
    subtitles = c("Centered parameterization", 
                  "Non-centered parameterization")
  )
}

neff_cp <- neff_ratio(fit_cp, pars = c("theta", "mu", "tau"))
neff_ncp <- neff_ratio(fit_ncp, pars = c("theta", "mu", "tau"))
compare_cp_ncp(mcmc_neff(neff_cp), mcmc_neff(neff_ncp), ncol = 1)

## ----mcmc_acf------------------------------------------------------------
compare_cp_ncp(
  mcmc_acf(posterior_cp, pars = "theta[1]", lags = 10),
  mcmc_acf(posterior_ncp, pars = "eta[1]", lags = 10)
)

## ---- available_mcmc-nuts------------------------------------------------
available_mcmc(pattern = "_nuts_")

## ---- extract-nuts-info--------------------------------------------------
lp_cp <- log_posterior(fit_cp)
head(lp_cp)
np_cp <- nuts_params(fit_cp)
head(np_cp)

# for the second model
lp_ncp <- log_posterior(fit_ncp)
np_ncp <- nuts_params(fit_ncp)

## ---- mcmc_trace---------------------------------------------------------
color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_cp, pars = "tau", np = np_cp) + 
  xlab("Post-warmup iteration")

## ---- mcmc_nuts_divergence-----------------------------------------------
color_scheme_set("red")
mcmc_nuts_divergence(np_cp, lp_cp)

## ---- mcmc_nuts_divergence-chain-----------------------------------------
mcmc_nuts_divergence(np_cp, lp_cp, chain = 4)

## ---- mcmc_nuts_divergence-2---------------------------------------------
mcmc_nuts_divergence(np_ncp, lp_ncp)

## ---- fit-adapt-delta, results='hide', message=FALSE---------------------
fit_cp_2 <- sampling(schools_mod_cp, data = schools_dat,
                     control = list(adapt_delta = 0.99))
fit_ncp_2 <- sampling(schools_mod_ncp, data = schools_dat,
                      control = list(adapt_delta = 0.99))

## ---- mcmc_nuts_divergence-3---------------------------------------------
mcmc_nuts_divergence(nuts_params(fit_cp_2), log_posterior(fit_cp_2))
mcmc_nuts_divergence(nuts_params(fit_ncp_2), log_posterior(fit_ncp_2))

## ---- mcmc_parcoord-1----------------------------------------------------
color_scheme_set("darkgray")
mcmc_parcoord(posterior_cp, np = np_cp)

## ---- mcmc_scatter-1-----------------------------------------------------
mcmc_scatter(
  posterior_cp, 
  pars = c("theta[1]", "tau"), 
  transform = list(tau = "log"), # actually show log(tau)
  np = np_cp
)

## ---- mcmc_nuts_energy-1, message=FALSE----------------------------------
color_scheme_set("red")
mcmc_nuts_energy(np_cp)

## ---- mcmc_nuts_energy-3, message=FALSE, fig.width=8---------------------
compare_cp_ncp(
  mcmc_nuts_energy(np_cp, binwidth = 1/2),
  mcmc_nuts_energy(np_ncp, binwidth = 1/2)
)

## ---- mcmc_nuts_energy-4, message=FALSE,  fig.width=8--------------------
np_cp_2 <- nuts_params(fit_cp_2)
np_ncp_2 <- nuts_params(fit_ncp_2)

compare_cp_ncp(
  mcmc_nuts_energy(np_cp_2), 
  mcmc_nuts_energy(np_ncp_2)
)

