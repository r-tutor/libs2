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

## ---- roaches-model, results="hide", message=FALSE,warning=FALSE---------
library("rstanarm")
head(roaches) # see help("rstanarm-datasets")

roaches$roach1 <- roaches$roach1 / 100 # pre-treatment number of roaches (in 100s)
fit_poisson <- stan_glm(y ~ roach1 + treatment + senior,
                        offset = log(exposure2),
                        family = poisson(link = "log"),
                        data = roaches,
                        seed = 1111)

## ---- print--------------------------------------------------------------
print(fit_poisson)

## ---- roaches-model-2, results="hide", message=FALSE,warning=FALSE-------
fit_nb <- update(fit_poisson, family = "neg_binomial_2")

## ---- print-2------------------------------------------------------------
print(fit_nb)

## ---- posterior_predict--------------------------------------------------
yrep_poisson <- posterior_predict(fit_poisson, draws = 500)
yrep_nb <- posterior_predict(fit_nb, draws = 500)
dim(yrep_poisson)
dim(yrep_nb)

## ----ppc_dens_overlay----------------------------------------------------
library("ggplot2")
library("bayesplot")

color_scheme_set("brightblue") # see help("bayesplot-colors")

y <- roaches$y
ppc_dens_overlay(y, yrep_poisson[1:50, ])

## ----ppc_hist, message=FALSE---------------------------------------------
ppc_hist(y, yrep_poisson[1:5, ])

## ----ppc_hist-nb, message=FALSE------------------------------------------
ppc_hist(y, yrep_nb[1:5, ])

## ----ppc_hist-nb-2, message=FALSE----------------------------------------
ppc_hist(y, yrep_nb[1:5, ], binwidth = 20) + 
  coord_cartesian(xlim = c(-1, 500))

## ---- prop_zero----------------------------------------------------------
prop_zero <- function(x) mean(x == 0)
prop_zero(y) # check proportion of zeros in y

## ----ppc_stat, message=FALSE---------------------------------------------
ppc_stat(y, yrep_poisson, stat = "prop_zero")

## ----ppc_stat-nb, message=FALSE------------------------------------------
ppc_stat(y, yrep_nb, stat = "prop_zero")

## ----ppc_stat-max, message=FALSE-----------------------------------------
ppc_stat(y, yrep_poisson, stat = "max")
ppc_stat(y, yrep_nb, stat = "max")
ppc_stat(y, yrep_nb, stat = "max", binwidth = 100) + 
  coord_cartesian(xlim = c(-1, 5000))

## ---- available_ppc------------------------------------------------------
available_ppc()

## ----ppc_stat_grouped, message=FALSE-------------------------------------
ppc_stat_grouped(y, yrep_nb, group = roaches$treatment, stat = "prop_zero")

## ---- available_ppc-grouped----------------------------------------------
available_ppc(pattern = "_grouped")

## ---- pp_check.foo-------------------------------------------------------
pp_check.foo <- function(object, ..., type = c("multiple", "overlaid")) {
  y <- object[["y"]]
  yrep <- object[["yrep"]]
  switch(
    match.arg(type),
    multiple = ppc_hist(y, yrep[1:min(8, nrow(yrep)),, drop = FALSE]),
    overlaid = ppc_dens_overlay(y, yrep)
  )
}

## ---- foo-object---------------------------------------------------------
x <- list(y = rnorm(50), yrep = matrix(rnorm(5000), nrow = 100, ncol = 50))
class(x) <- "foo"

## ---- pp_check-1, eval=FALSE---------------------------------------------
#  pp_check(x)

## ---- print-1, echo=FALSE------------------------------------------------
gg <- pp_check(x)
suppressMessages(print(gg))

## ---- pp_check-2---------------------------------------------------------
pp_check(x, type = "overlaid")

