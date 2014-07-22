
#' run.all.models
#'
#' Automates process of running all the combinations of models for every subject
#' 
#' @export
#' @examples
#' run.all.models(data.dir = './data', model.file = 'model.txt', model.fixed.volatility = 'model-fixed-volatility.txt', n.chains = 1, n.samples = 15000, n.burnin = 5000)
run.all.models <- function(data.dir, model.file, model.fixed.volatility.file, n.chains, n.samples, n.burnin){
  
  range <- c(5,6,8,10,11,13,15,16,18,19,20,seq(23,28),30,31,seq(33,36),39,40,seq(42,45))
  
  for(i in range){
    
    data.social.file <- NULL
    data.house.file <- NULL
    
    if(i < 10){
      data.social.file <- paste(data.dir, '/subj_00', i, '_social.csv', sep='')
      data.house.file <- paste(data.dir, '/subj_00', i, '_house.csv', sep='')
    }
    else{
      data.social.file <- paste(data.dir, '/subj_0', i, '_social.csv', sep='')
      data.house.file <- paste(data.dir, '/subj_0', i, '_house.csv', sep='')
    }
    
    run.model(model.file = model.file, data.file = data.social.file, n.chains = n.chains, n.samples = n.samples, n.burnin = n.burnin)
    run.model(model.file = model.file, data.file = data.house.file, n.chains = n.chains, n.samples = n.samples, n.burnin = n.burnin)
    run.model(volatility.fixed = TRUE, model.file = model.fixed.volatility.file, data.file = data.social.file, n.chains = n.chains, n.samples = n.samples, n.burnin = n.burnin)
    run.model(volatility.fixed = TRUE, model.file = model.fixed.volatility.file, data.file = data.house.file, n.chains = n.chains, n.samples = n.samples, n.burnin = n.burnin)
  }
}


#' run.model
#'
#' Runs Behren's model with R2jags and plots means of expecation and volatility through time.
#'
#' Output is in CSV format. File names are as follows: \cr\cr <data.file.name>_ts_*.csv - The "_ts_" (time-series) indicates that these are the results for the samples of the time series variables \cr\cr <data.file.name>_s_*.csv - The "_s_" (stationary) indicates that these are the results for the samples of the time stationary variables \cr\cr <data.file.name>_*_v.csv - The "_v" (volatility time series) indicates that these are the results for the samples of the variables with v being a time series variable \cr\cr <data.file.name>_*_vf.csv - The "_vf" (volatility fixed) indicates that these are the results for the samples of the variables with v being a time stationary variable \cr\cr Columns are as follows: \cr\cr *_ts_v.csv - expectation mean, expectation standard deviation, volatility mean, volatility standard deviation \cr\cr *_s_v.csv - k mean, k standard deviation, expectation beta parameter mean, expectation beta parameter standard deviation, action beta parameter mean, action beta parameter standard deviation \cr\cr *_ts_vf.csv - expectation mean, expectation standard deviation \cr\cr *_s_vf.csv - volatility mean, volatility standard deviation, expectation beta parameter mean, expectation beta parameter standard deviation, action beta parameter mean, action beta parameter standard deviation
#' 
#' @param volatility.fixed Determines whether the volatility is fixed and stationary in time if true, or changes throughout time if false. Defaults to false
#' @param model.file The path to the text file containing the model
#' @param data.file The path to the csv file containing the data
#' @param n.chains The amount of Markov chains to run
#' @param n.samples The amount of samples to take. More samples = more convergence
#' @param n.burnin The amount of burnin samples to discard
#' 
#' @keywords behren model
#' @export
#' @examples
#' run.model(model.file=".../model.txt", data.file=".../data/subj_005_social.csv", n.chains=1, n.samples=15000, n.burnin=1000)
#' run.model(volatility.fixed=TRUE, model.file=".../model-fixed-volatility.txt", data.file=".../data/subj_005_social.csv", n.chains=1, n.samples=15000, n.burnin=1000)

run.model <- function(volatility.fixed=FALSE, model.file, data.file, n.chains, n.samples, n.burnin) {

  #Make sure jagstools is loaded before continuing.
  #Don't want to run model then 
  test <- require('jagstools')
  
  if(!test){
    return("Please run the BehrensRJags.init() function before continuing.")
  }
  
  set.seed(1234)
  
  dataCSV <- read.csv(data.file, header=FALSE)
  
  dataMat <- as.matrix(dataCSV)
  dataMat[,3] <- replace(dataMat[,3], dataMat[,3] == 2, 0)
  model.data <- list(a = dataMat[,3], e = dataMat[,4], N = 100)
  
  if(volatility.fixed == FALSE){
    monitors <- c('r','v','k','beta.e','beta.a')
  }
  else{
    monitors <- c('r','v','beta.e','beta.a')
  }
  
  model.fit <- jags(model.file = model.file, data = model.data, parameters.to.save = monitors, n.chains = 1, n.iter = n.samples, n.burnin = n.burnin)
  
  #This was giving errors and aborting the function, so the samples are just specified now
  #model.fit <- autojags(model.fit)
  
  r.results <- jagsresults(x = model.fit, params = c('r'))
  r.mean <- r.results[,c('mean')]
  r.sd <- r.results[,c('sd')]
  
  v.results <- jagsresults(x = model.fit, params = c('v'))
  v.mean <- v.results[,c('mean')]
  v.sd <- v.results[,c('sd')]
  
  k.results <- jagsresults(x = model.fit, params = c('k'))
  k.mean <- k.results[,c('mean')]
  k.sd <- k.results[,c('sd')]
  
  beta.e.results <- jagsresults(x = model.fit, params = c('beta.e'))
  beta.e.mean <- beta.e.results[,c('mean')]
  beta.e.sd <- beta.e.results[,c('sd')]
  
  beta.a.results <- jagsresults(x = model.fit, params = c('beta.a'))
  beta.a.mean <- beta.a.results[,c('mean')]
  beta.a.sd <- beta.a.results[,c('sd')]
  
  data.in.file.length <- nchar(data.file)
  
  output.dir <- paste(getwd(),'/model-results',sep='')
  dir.create(output.dir, showWarnings = FALSE)
  
  if(volatility.fixed == FALSE){
    data.out.time.series <- data.frame(r.mean, r.sd, v.mean, v.sd)
    data.out.stationary <- data.frame(k.mean, k.sd, beta.e.mean, beta.e.sd, beta.a.mean, beta.a.sd)
    
    data.out.time.series.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_ts_vts.csv', sep="")
    data.out.stationary.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_s_vts.csv', sep="")
  }
  else {
    data.out.time.series <- data.frame(r.mean, r.sd)
    data.out.stationary <- data.frame(v.mean, v.sd, beta.e.mean, beta.e.sd, beta.a.mean, beta.a.sd)
    
    data.out.time.series.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_ts_vf.csv', sep="")
    data.out.stationary.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_s_vf.csv', sep="")
  }
  
  
  
  data.out.time.series.file.path = paste('/model-results/', data.out.time.series.file.name, sep="")
  data.out.time.series.file.path = paste(getwd(), data.out.time.series.file.path, sep="")
  
  print(paste("Writing file", data.out.time.series.file.path))
  write.table(data.out.time.series, file=data.out.time.series.file.path, row.names=FALSE, col.names=FALSE, sep=',')
  
  
  data.out.stationary.file.path = paste('/model-results/', data.out.stationary.file.name, sep="")
  data.out.stationary.file.path = paste(getwd(), data.out.stationary.file.path, sep="")
  
  print(paste("Writing file", data.out.stationary.file.path))
  write.table(data.out.stationary, file=data.out.stationary.file.path, row.names=FALSE, col.names=FALSE, sep=',')
  
  print('Done!')
}

#' BehrensRJags.Init
#'
#' Retrieves a dependency package from Github that is not available on the CRAN repositories. This only needs to be run when BehrensRJags is first installed.
#' 
#' run.model(...) will not work without calling this function at least once to install the jagstools package. The model will build and update, but the output will break.
#' @export
#' @examples
#' BehrensRJags.init()

BehrensRJags.init <- function(){
  install.packages('devtools')
  library(devtools)
  install_github('johnbaums/jagstools')
  library(jagstools)
}