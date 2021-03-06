% Generated by roxygen2 (4.0.1): do not edit by hand
\name{run.all.models}
\alias{run.all.models}
\title{run.all.models}
\usage{
run.all.models(data.dir, model.file, model.fixed.volatility.file, n.chains,
  n.samples, n.burnin)
}
\description{
Automates process of running all the combinations of models for every subject
}
\examples{
run.all.models(data.dir = './data', model.file = 'model.txt', model.fixed.volatility = 'model-fixed-volatility.txt', n.chains = 1, n.samples = 15000, n.burnin = 5000)
}

