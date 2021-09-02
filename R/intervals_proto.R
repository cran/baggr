intervals_proto <- function(..., what = c("mean", "sd", "effect")) {
  ll <- list(...)
  what <- match.arg(what, c("mean", "sd", "effect"))
  label <- c("mean" = "Hypermean",
             "sd" = "Hyper-SD",
             "effect" = "Posterior predictive effect")

  m <- do.call(cbind, lapply(ll, function(x){
    if(what == "mean")   y <- treatment_effect(x)$tau
    if(what == "sd")     y <- treatment_effect(x)$sigma_tau
    if(what == "effect") y <- effect_draw(x)
    y
  }))
  colnames(m) <- names(ll)
  bayesplot::mcmc_intervals(m) +
    theme(axis.ticks.y = element_blank()) +
    xlab(label[what])
}

# intervals_proto("ITT" = bg_itt, "TOT" = bg_tot)
