\name{BehrensRJags-package}
\alias{BehrensRJags-package}
\alias{BehrensRJags}
\docType{package}
\title{
BehrensRJags
}
\description{
Provides wrapper tools to analyze Timothy Behren's behavior model with R and JAGS.
}
\details{
\tabular{ll}{
Package: \tab BehrensRJags\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-14\cr
License: \tab UALR\cr
}
}
\author{
Jeff Long, Keith Bush

Maintainer: Jeff Long <jmlong@ualr.edu>
}
\references{
 - Behrens, Timothy E.J., Nat. Neurosci. Vol 10, 1214-1221, September 2007
}
\keyword{ package }
\seealso{
}
\examples{
  ##### Set up ##########
  install.packages('devtools')
  library(devtools)
  install_github('jmlong94/BehrensRJags')
  
  library(BehrensRJags)
  BehrensRJags.init() #Only need to call this once, when first installed
  #######################
  
  
  ##### Run model with volatility as time-series variable #######
  model.file <- '.../model.txt'
  data.file <- '.../subj_005_social.csv'
  
  run.model(model.file = model.file, data.file = data.file, n.chains = 1, n.samples = 15000, n.burnin = 5000)
  #############################################################################
  
  
  ##### Run model with volatility as variable that is stationary in time ######
  model.file <- '.../model-fixed-volatility.txt'
  data.file <- '.../subj_005_social.csv'
  
  run.model(volatility.fixed = TRUE, model.file = model.file, data.file = data.file, n.chains = 1, n.samples = 15000, n.burnin = 5000)
  #############################################################################
  
}
