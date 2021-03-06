% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loocv.R
\name{loo_compare}
\alias{loo_compare}
\title{Compare fitted models on loo}
\usage{
loo_compare(x, ...)
}
\arguments{
\item{x}{An object of class \code{baggr_cv} or a list of such objects.}

\item{...}{Additional objects of class "baggr_cv"}
}
\description{
Compare fitted models on loo
}
\examples{
\dontrun{
# 2 models with more/less informative priors -- this will take a while to run
cv_1 <- loocv(schools, model = "rubin", pooling = "partial")
cv_2 <- loocv(schools, model = "rubin", pooling = "partial",
              prior_hypermean = normal(0, 5), prior_hypersd = cauchy(0,4))
loo_compare(cv_1, cv_2)
}
}
