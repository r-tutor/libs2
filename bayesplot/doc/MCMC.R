params <-
structure(list(EVAL = TRUE), .Names = "EVAL")

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
  comment = NA,
  eval = params$EVAL
)

## ---- eval=FALSE, results='hide'-----------------------------------------
#  library("rstanarm")
#  fit <- stan_glm(
#    mpg ~ ., # ~ . includes all other variables in dataset
#    data = mtcars,
#    chains = 4,
#    iter = 2000,
#    seed = 1111
#  )
#  print(fit)

## ----stan_glm, echo=FALSE, results='hide'--------------------------------
suppressPackageStartupMessages(library("rstanarm"))
fit <- stan_glm(
  mpg ~ ., 
  data = mtcars, 
  chains = 4, 
  iter = 2000,
  seed = 1111
)

## ---- print-fit, echo=FALSE----------------------------------------------
print(fit)

## ---- get-draws----------------------------------------------------------
posterior <- as.array(fit)
dim(posterior)
dimnames(posterior)

## ---- mcmc_intervals-----------------------------------------------------
library("bayesplot")
color_scheme_set("red")
mcmc_intervals(posterior, pars = c("cyl", "drat", "am", "sigma"))

## ---- mcmc_areas---------------------------------------------------------
mcmc_areas(
  posterior, 
  pars = c("cyl", "drat", "am", "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

## ---- mcmc_hist, message=FALSE-------------------------------------------
color_scheme_set("green")
mcmc_hist(posterior, pars = c("wt", "am"))
mcmc_dens(posterior, pars = c("wt", "am"))

## ---- mcmc_hist_by_chain, message=FALSE----------------------------------
color_scheme_set("brightblue")
mcmc_hist_by_chain(posterior, pars = c("wt", "am"))
mcmc_dens_overlay(posterior, pars = c("wt", "am"))

## ---- mcmc_violin--------------------------------------------------------
mcmc_violin(posterior, pars = c("wt", "am"), probs = c(0.1, 0.5, 0.9))

## ---- mcmc_scatter-------------------------------------------------------
color_scheme_set("gray")
mcmc_scatter(posterior, pars = c("(Intercept)", "wt"), size = 1.5, alpha = 0.5)

## ---- mcmc_hex-----------------------------------------------------------
mcmc_hex(posterior, pars = c("(Intercept)", "wt"))

## ---- mcmc_trace---------------------------------------------------------
color_scheme_set("blue")
mcmc_trace(posterior, pars = c("wt", "sigma"))

## ---- change-scheme------------------------------------------------------
color_scheme_set("mix-blue-red")
mcmc_trace(posterior, pars = c("wt", "sigma"), 
           facet_args = list(ncol = 1, strip.position = "left"))

## ---- viridis-scheme-----------------------------------------------------
color_scheme_set("viridis")
mcmc_trace(posterior, pars = "(Intercept)")

## ---- mcmc_trace_highlight-----------------------------------------------
mcmc_trace_highlight(posterior, pars = "sigma", highlight = 3)

