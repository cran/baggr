## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, fig.width=7, 
  fig.height = 5, comment = "#>")
library(baggr)
library(ggplot2)
library(gridExtra)

## -----------------------------------------------------------------------------
df_yusuf <- read.table(text="
       trial  a n1i  c n2i
      Balcon 14  56 15  58
     Clausen 18  66 19  64
 Multicentre 15 100 12  95
      Barber 10  52 12  47
      Norris 21 226 24 228
      Kahler  3  38  6  31
     Ledwich  2  20  3  20
", header=TRUE)

## -----------------------------------------------------------------------------
# This is a calculation we could do by hand:
# df <- df_yusuf
# df$b <- df$n1i-df$a
# df$d <- df$n2i-df$c
# df$tau <- log((df$a*df$d)/(df$b*df$c))
# df$se <- sqrt(1/df$a + 1/df$b + 1/df$c + 1/df$d)

# But prepare_ma() automates these operations:
df_ma <- prepare_ma(df_yusuf, group = "trial", effect = "logOR")
df_ma

## ---- include = F-------------------------------------------------------------
bg_model_agg <- baggr(df_ma, iter = 2000, effect = "logarithm of odds ratio")

## ---- eval = F, echo = T------------------------------------------------------
#  bg_model_agg <- baggr(df_ma, effect = "logarithm of odds ratio")

## -----------------------------------------------------------------------------
labbe(df_ma, plot_model = TRUE, shade_se = "or")

## -----------------------------------------------------------------------------
a <- 9; b <- 1; c <- 99; d <- 1
cat("Risk ratio is", (a/(a+b))/(c/(c+d)), "\n" )
cat("Odds ratio is", a*d/(b*c), "\n")

## -----------------------------------------------------------------------------
a <- 10; b <- 20; c <- 100; d <- 100
cat("Risk ratio is", (a/(a+b))/(c/(c+d)), "\n" )
cat("Odds ratio is", a*d/(b*c), "\n")

## -----------------------------------------------------------------------------
par(mfrow = c(2,3), oma = rep(2,4))
for(es in c(1, .9, .8, .5, .25, .1)){
  p_bsl    <- seq(0,1,length=100)
  p_trt_rr <- es*p_bsl
  odds_trt <- es*(p_bsl/(1-p_bsl))
  p_trt_or <- odds_trt / (1 + odds_trt)
  plot(p_trt_or ~ p_bsl, type = "l", 
       xlab = "control event rate", ylab = "treatment event rate", main = paste0("RR=OR=",es))
  lines(p_trt_rr ~ p_bsl, lty = "dashed")
}
title(outer = TRUE, "Compare RR (dashed) and OR (solid) of the same magnitude")

## -----------------------------------------------------------------------------
bg_model_agg

## -----------------------------------------------------------------------------
forest_plot(bg_model_agg, show = "both", print = "inputs")

## -----------------------------------------------------------------------------
effect_plot(bg_model_agg)

## ---- warning = FALSE---------------------------------------------------------
gridExtra::grid.arrange(
  plot(bg_model_agg, transform = exp) + xlab("Effect on OR"),
  effect_plot(bg_model_agg, transform = exp) + xlim(0, 3) + xlab("Effect on OR"),
  ncol = 2)

## ---- echo=T, eval = F--------------------------------------------------------
#  # Instead of writing...
#  # bg1 <- baggr(df_ma, pooling = "none")
#  # bg2 <- baggr(df_ma, pooling = "partial")
#  # bg3 <- baggr(df_ma, pooling = "full")
#  
#  # ...we use this one-liner
#  bg_c <- baggr_compare(df_ma, effect = "logarithm of odds ratio")

## ---- include = FALSE---------------------------------------------------------
bg_c <- baggr_compare(df_ma, what = "pooling", effect = "logarithm of odds ratio")

## -----------------------------------------------------------------------------
plot(bg_c)

## -----------------------------------------------------------------------------
effect_plot(
  "Partial pooling, default prior" = bg_c$models$partial,
  "Full pooling, default prior" = bg_c$models$full) +
  theme(legend.position = "bottom")

## ---- include = F, warning = F------------------------------------------------
a <- loocv(df_ma, pooling = "partial", iter = 500, chains = 2)
b <- loocv(df_ma, pooling = "full", iter = 500, chains = 2)
#a; b; #you can print out individual loocv() calculations
loo_compare(a,b) #...but typically we compare them to each other

## ---- echo = T, eval = F------------------------------------------------------
#  a <- loocv(df_ma, pooling = "partial")
#  b <- loocv(df_ma, pooling = "full")
#  #a; b; #you can print out individual loocv() calculations
#  loo_compare(a,b) #...but typically we compare them to each other

## -----------------------------------------------------------------------------
df_ind <- binary_to_individual(df_yusuf, group = "trial")
head(df_ind)

## ----logit model, include = F-------------------------------------------------
bg_model_ind <- baggr(df_ind, model = "logit", effect = "logarithm of odds ratio", chains = 2, iter = 500)

## ---- echo = T, eval = F------------------------------------------------------
#  bg_model_ind <- baggr(df_ind, model = "logit", effect = "logarithm of odds ratio")

## -----------------------------------------------------------------------------
baggr_compare(bg_model_agg, bg_model_ind)

## -----------------------------------------------------------------------------
prepare_ma(df_ind, effect = "logOR")
prepare_ma(df_ind, effect = "logRR")

## -----------------------------------------------------------------------------
df_rare <- data.frame(group = paste("Study", LETTERS[1:5]),
                      a = c(0, 2, 1, 3, 1), c = c(2, 2, 3, 3, 5),
                      n1i = c(120, 300, 110, 250, 95),
                      n2i = c(120, 300, 110, 250, 95))

df_rare

## -----------------------------------------------------------------------------
df_rare_logor <- prepare_ma(df_rare, effect = "logOR")
# df_rare_logor <- prepare_ma(df_rare_ind, effect = "logOR")
df_rare_logor

## -----------------------------------------------------------------------------
pma01 <- prepare_ma(df_rare, effect = "logOR", 
                            rare_event_correction = 0.1)
pma1 <- prepare_ma(df_rare, effect = "logOR", 
                            rare_event_correction = 1)
pma01

## ----rare event comparison, include=F-----------------------------------------
bg_correction01 <- baggr(pma01, effect = "logOR", iter = 500)
bg_correction025 <- baggr(df_rare_logor, effect = "logOR", iter = 500)
bg_correction1 <- baggr(pma1, effect = "logOR", iter = 500)
bg_rare_ind <- baggr(df_rare, model = "logit", effect = "logOR")

## ---- echo=T, eval=F----------------------------------------------------------
#  bg_correction01 <- baggr(pma01, effect = "logOR")
#  bg_correction025 <- baggr(df_rare_logor, effect = "logOR")
#  bg_correction1 <- baggr(pma1, effect = "logOR")
#  bg_rare_ind <- baggr(df_rare, model = "logit", effect = "logOR")

## -----------------------------------------------------------------------------
bgc <- baggr_compare(
  "Correct by .10" = bg_correction01,
  "Correct by .25" = bg_correction025,
  "Correct by 1.0" = bg_correction1,
  "Individual data" = bg_rare_ind
)
bgc
plot(bgc) + theme(legend.position = "right")

## -----------------------------------------------------------------------------
df_rare <- data.frame(group = paste("Study", LETTERS[1:5]),
                      a = c(1, 2, 1, 3, 1), c = c(2, 2, 3, 3, 5),
                      n1i = c(120, 300, 110, 250, 95),
                      n2i = c(120, 300, 110, 250, 95))

df_rare_logor <- prepare_ma(df_rare, effect = "logOR")

## ---- include=F---------------------------------------------------------------
bg_rare_agg <- baggr(df_rare_logor, effect = "logOR")
bg_rare_ind <- baggr(df_rare, effect = "logOR", model = "logit", iter = 500, chains = 2)

## ---- eval=F, echo=T----------------------------------------------------------
#  bg_rare_agg <- baggr(df_rare_logor, effect = "logOR")
#  bg_rare_ind <- baggr(df_rare, effect = "logOR", model = "logit")

## -----------------------------------------------------------------------------
bgc <- baggr_compare(
  "Summary-level (Rubin model on logOR)" = bg_rare_agg,
  "Individual-level (logistic model)"    = bg_rare_ind
)
bgc
plot(bgc)

## ----rare events with bsl priors, include = F---------------------------------
# bg_rare_prior2 <- baggr(df_rare_logor, effect = "logOR", model = "logit",
#                         prior_control = normal(-4.59, 2))
bg_rare_pool_bsl <- baggr(df_rare, effect = "logOR", model = "logit",
                        pooling_control = "partial",
                        chains = 2, iter = 500,
                        prior_control = normal(-4.59, 1), prior_control_sd = normal(0, 2))
bg_rare_strong_prior <- baggr(df_rare, effect = "logOR", model = "logit",
                        chains = 2, iter = 500,
                        prior_control = normal(-4.59, 10))

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  bg_rare_pool_bsl <- baggr(df_rare, effect = "logOR", model = "logit",
#                          pooling_control = "partial",
#                          prior_control = normal(-4.59, 1), prior_control_sd = normal(0, 2))
#  bg_rare_strong_prior <- baggr(df_rare, effect = "logOR", model = "logit",
#                          prior_control = normal(-4.59, 10))

## -----------------------------------------------------------------------------
bgc <- baggr_compare(
  "Rubin model" = bg_rare_agg, 
  "Independent N(0,10^2)" = bg_rare_ind, 
  # "Prior N(-4.59, 2^2)" = bg_rare_prior2, 
  "Hierarchical prior" = bg_rare_pool_bsl,
  "Independent N(-4.59, 10^2)" = bg_rare_strong_prior
)

bgc

## -----------------------------------------------------------------------------
plot(bgc) + theme(legend.position = "right")

## ---- include = FALSE, echo = FALSE-------------------------------------------
#let's use the data.frame we created from Yusuf et al earlier
df_ma$study_grouping      <- c(1,1,1,0,0,0,0)
df_ma$different_contrasts <- c(1,1,1,0,0,0,0) - .5
bg_cov1 <- baggr(df_ma, covariates = c("study_grouping"), effect = "logarithm of odds ratio")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  #let's use the data.frame we created from Yusuf et al earlier
#  df_ma$study_grouping      <- c(1,1,1,0,0,0,0)
#  df_ma$different_contrasts <- c(1,1,1,0,0,0,0) - .5
#  bg_cov1 <- baggr(df_ma, covariates = c("study_grouping"), effect = "logarithm of odds ratio")

## -----------------------------------------------------------------------------
baggr_compare("No covariate" = bg_model_agg, 
              "With covariates, 0-1 coding" = bg_cov1)

## -----------------------------------------------------------------------------
bg_cov1

