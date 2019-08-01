## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
library(baggr)
library(ggplot2)
baggr_schools <- baggr(schools, model = "rubin", pooling = "partial")
# baggr_plot(baggr_schools)
# baggr_compare(schools)
# my_baggr_plot <- baggr_compare(schools)


## ------------------------------------------------------------------------
colnames(microcredit_simplified) <- c("group", "outcome", "treatment")
prepare_ma(microcredit_simplified)

## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
schools

## ----eval=FALSE----------------------------------------------------------
#  baggr_schools <- baggr(schools, model = "rubin", pooling = "partial")

## ------------------------------------------------------------------------
print(baggr_schools)

## ----eval=FALSE----------------------------------------------------------
#  baggr(schools, "rubin", prior = c("prior_upper_sigma_tau" = 10000,
#                                    "prior_tau_mean" = -10,
#                                    "prior_tau_scale" = 100))
#  baggr_schools

## ----eval=FALSE----------------------------------------------------------
#  baggr_schools <- baggr(schools, model = "rubin", pooling = "partial", iter = 10000, chains = 8)

## ----fig.width=4---------------------------------------------------------
baggr_plot(baggr_schools, order = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  baggr_compare(schools)

## ---- echo=FALSE, eval = TRUE, include = FALSE---------------------------
my_baggr_plot <- baggr_compare(schools)

## ---- echo=TRUE, eval = FALSE--------------------------------------------
#  my_baggr_plot <- baggr_compare(schools)

## ----fig.width=7, fig.height=4, echo = TRUE------------------------------
my_baggr_plot$plot + ggtitle("8 schools: model comparison")

## ----loocv, eval = FALSE-------------------------------------------------
#  loocv_res <- loocv(schools, return_models = FALSE, "rubin", pooling = "partial")

## ----echo = FALSE, include = FALSE, eval = TRUE--------------------------
#this is ugly but effective way to stop output from loocv getting printed
loocv_res <- loocv(schools, return_models = FALSE, "rubin", pooling = "partial")

## ------------------------------------------------------------------------
loocv_res

## ------------------------------------------------------------------------
names(attributes(loocv_res))
attr(loocv_res, "df")

## ---- echo = FALSE, include = FALSE--------------------------------------
fit1 <- baggr(data = schools[1:7,], test_data = schools[8,], model = "rubin", pooling = "partial")
fit2 <- baggr(data = schools[1:7,], test_data = schools[8,], model = "rubin", pooling = "full")

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  fit1 <- baggr(data = schools[1:7,], test_data = schools[8,], model = "rubin", pooling = "partial")
#  fit2 <- baggr(data = schools[1:7,], test_data = schools[8,], model = "rubin", pooling = "full")

## ------------------------------------------------------------------------
fit1$mean_lpd
fit2$mean_lpd

