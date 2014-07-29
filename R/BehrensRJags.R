


generate.data.matrices <- function(model.results.dir, model.data.dir, subject.number, type) {
  
  if(type != 'social' && type != 'house'){
    print("Please input a valid type (social or house).")
    return
  }
  
  if(subject.number < 10){
    data.file <- paste(model.results.dir,'/subj_00',subject.number,'_',type,'_results_',sep='')
    model.data.file <- paste(model.data.dir,'/subj_00',subject.number,'_',type,'.csv',sep='')
  }
  else{
    data.file <- paste(model.results.dir,'/subj_0',subject.number,'_',type,'_results_',sep='')
    model.data.file <- paste(model.data.dir,'/subj_0',subject.number,'_',type,'.csv',sep='')
  }
  
  data.file.vts <- paste(data.file,'ts_vts.csv',sep='')
  data.csv.vts <- read.csv(data.file.vts, header=FALSE)
  data.mat.vts <- as.matrix(data.csv.vts)
  
  data.file.vf <- paste(data.file,'ts_vf.csv',sep='')
  data.csv.vf <- read.csv(data.file.vf, header=FALSE)
  data.mat.vf <- as.matrix(data.csv.vf)
  
  subject.evidence.csv <- read.csv(model.data.file, header=FALSE)
  subject.evidence.mat <- as.matrix(subject.evidence.csv)
  subject.evidence.mat[,3] <- replace(subject.evidence.mat[,3], subject.evidence.mat[,3] == 2, 0)
  
  return(list(data.mat.vts, data.mat.vf, subject.evidence.mat))
}

plot.subject.results <- function(model.results.dir, model.data.dir, subject.number, type) {
  
  set.seed(1234)

  mat.list <- generate.data.matrices(model.results.dir, model.data.dir, subject.number, type)

  data.mat.vts <- mat.list[[1]]
  data.mat.vf <- mat.list[[2]]
  subject.evidence.mat <- mat.list[[3]]
  
  e <- subject.evidence.mat[,4]
  a <- subject.evidence.mat[,3]
  
  #------------Set up variables for plotting volatility time series model--------------------------
  
  delta.mean.vts <- data.mat.vts[,3]
  epsilon.mean.vts <- data.mat.vts[,5]
  
  #Calc beta distribution mean and sd
  beta.info <- calculate.beta.info(delta.mean.vts, epsilon.mean.vts)
  
  e.mean.estimated.vts <- beta.info[[1]]
  e.sd.estimated.vts <- beta.info[[2]]
  
  theta.mean.vts <- data.mat.vts[,7]
  phi.mean.vts <- data.mat.vts[,9]
  
  beta.info <- calculate.beta.info(theta.mean.vts, phi.mean.vts)
  
  a.mean.estimated.vts <- beta.info[[1]]
  a.sd.estimated.vts <- beta.info[[2]]
  
  
  #------------------------------------------------------------------------------------------------
  
  
  #------------Set up variables for plotting volatility fixed model--------------------------------
  
  delta.mean.vf <- data.mat.vf[,3]
  epsilon.mean.vf <- data.mat.vf[,5]
  
  beta.info <- calculate.beta.info(delta.mean.vf, epsilon.mean.vf)
  
  e.mean.estimated.vf <- beta.info[[1]]
  e.sd.estimated.vf <- beta.info[[2]]
  
  theta.mean.vf <- data.mat.vf[,7]
  phi.mean.vf <- data.mat.vf[,9]
  
  beta.info <- calculate.beta.info(theta.mean.vf, phi.mean.vf)
  
  a.mean.estimated.vf <- beta.info[[1]]
  a.sd.estimated.vf <- beta.info[[2]]
  
  #------------------------------------------------------------------------------------------------
  
  #Make the evidence in terms of the left face, conditioned on the action
  for(i in 1:100){
    
    if(a[i] == 0){
      
      e[i] <- 1 - e[i]
      e.mean.estimated.vts[i] <- 1 - e.mean.estimated.vts[i]
      e.mean.estimated.vf[i] <- 1 - e.mean.estimated.vf[i]
    }
  }
  
  #Set plotting parameters
  par(mfcol=c(2,2), oma = c(1, 1, 0, 0), mar = c(3, 2.5, 2, 2), mgp = c(1.6, 0.6, 0), xpd = FALSE)
  
  #Plot evidence for time series volatility model
  plot(x=e, ylim=c(0,1), type='l', lwd=1, col='red', main='Time-Series Volatility')
  points(x=e.mean.estimated.vts, type='l', lwd=2, col='blue')
#   points(x=e.mean.estimated.vts + e.sd.estimated.vts, type='l', lwd=1, col='blue')
#   points(x=e.mean.estimated.vts - e.sd.estimated.vts, type='l', lwd=1, col='blue')
  
  #Plot action for time series volatility model
  plot(x=a, ylim=c(0,1), type='l', lwd=1, col='red')
  points(x=a.mean.estimated.vts, type='l', lwd=2, col='blue')
#   points(x=a.mean.estimated.vts + a.sd.estimated.vts, type='l', lwd=1, col='blue')
#   points(x=a.mean.estimated.vts - a.sd.estimated.vts, type='l', lwd=1, col='blue')
  
  #Plot evidence for fixed volatility model
  plot(x=e, ylim=c(0,1), type='l', lwd=1, col='red', main='Fixed Volatility')
  points(x=e.mean.estimated.vf, type='l', lwd=2, col='blue')
#   points(x=e.mean.estimated.vf + e.sd.estimated.vf, type='l', lwd=1, col='blue')
#   points(x=e.mean.estimated.vf - e.sd.estimated.vf, type='l', lwd=1, col='blue')
  
  #Plot action for fixed volatility model
  plot(x=a, ylim=c(0,1), type='l', lwd=1, col='red')
  points(x=a.mean.estimated.vf, type='l', lwd=2, col='blue')
#   points(x=a.mean.estimated.vf + a.sd.estimated.vf, type='l', lwd=1, col='blue')
#   points(x=a.mean.estimated.vf - a.sd.estimated.vf, type='l', lwd=1, col='blue')
  
}

compute.log.likelihoods <- function(model.results.dir, model.data.dir, subject.number, type){#evidence, actions, delta.vts, epsilon.vts, theta.vts, phi.vts, delta.vf, epsilon.vf, theta.vf, phi.vf){
  
  log.likelihood.vts.sum <- 0
  log.likelihood.vf.sum <- 0

  mat.list <- generate.data.matrices(model.results.dir, model.data.dir, subject.number, type)
  
  data.file <- NULL
  if(subject.number < 10){
    data.file <- paste(model.data.dir,'/subj_00',subject.number,'_',type,'.csv',sep='')
  }
  else{
    data.file <- paste(model.data.dir,'/subj_0',subject.number,'_',type,'.csv',sep='')
  }

  data.mat.vts <- mat.list[[1]]
  data.mat.vf <- mat.list[[2]]
  subject.evidence.mat <- mat.list[[3]]
  
  evidence <- subject.evidence.mat[,4]
  actions <- subject.evidence.mat[,3]
  
  
  #------------Set up variables for plotting volatility time series model--------------------------
  
  delta.mean.vts <- data.mat.vts[,3]
  epsilon.mean.vts <- data.mat.vts[,5]
  
  #Calc beta distribution mean and sd
  beta.info <- calculate.beta.info(delta.mean.vts, epsilon.mean.vts)
  
  e.mean.estimated.vts <- beta.info[[1]]
  e.sd.estimated.vts <- beta.info[[2]]
  
  theta.mean.vts <- data.mat.vts[,7]
  phi.mean.vts <- data.mat.vts[,9]
  
  #Calc beta distribution mean and sd
  beta.info <- calculate.beta.info(theta.mean.vts, phi.mean.vts)
  
  a.mean.estimated.vts <- beta.info[[1]]
  a.sd.estimated.vts <- beta.info[[2]]
  
  #------------------------------------------------------------------------------------------------
  
  
  #------------Set up variables for plotting volatility fixed model--------------------------------
  
  delta.mean.vf <- data.mat.vf[,3]
  epsilon.mean.vf <- data.mat.vf[,5]
  
  #Calc beta distribution mean and sd
  beta.info <- calculate.beta.info(delta.mean.vf, epsilon.mean.vf)
  
  e.mean.estimated.vf <- beta.info[[1]]
  e.sd.estimated.vf <- beta.info[[2]]
  
  theta.mean.vf <- data.mat.vf[,7]
  phi.mean.vf <- data.mat.vf[,9]
  
  #Calc beta distribution mean and sd
  beta.info <- calculate.beta.info(theta.mean.vf, phi.mean.vf)
  
  a.mean.estimated.vf <- beta.info[[1]]
  a.sd.estimated.vf <- beta.info[[2]]
  
  #------------------------------------------------------------------------------------------------
  
  for(i in 1:100){
    
    if(actions[i] == 0){
      
      evidence[i] <- 1 - evidence[i]
      e.mean.estimated.vts[i] <- 1 - e.mean.estimated.vts[i]
      e.mean.estimated.vf[i] <- 1 - e.mean.estimated.vf[i]
    }
    
    e <- evidence[i]
    a <- actions[i]
    
    #--------------------------Volatility time series model calculations ---------------------
    
    #Find likelihood for evidence variable 
    #e.p.vts <- dbeta(e, delta.mean.vts[i], epsilon.mean.vts[i])
    e.p.vts <- dnorm(e, e.mean.estimated.vts[i], e.sd.estimated.vts[i])
    log.l.e <- 0
    
    if(e.p.vts != 0)
      log.l.e <- log(e.p.vts)

    #Find likelihood for action variable
    #a.p.vts <- dbeta(a, theta.mean.vts[i], phi.mean.vts[i])
    a.p.vts <- dnorm(a, a.mean.estimated.vts[i], a.sd.estimated.vts[i])
    log.l.a <- 0
    
    if(a.p.vts != 0)
      log.l.a <- log(a.p.vts)
    
    #Add both to overall log likelihood
    log.likelihood.vts.sum <- log.likelihood.vts.sum + log.l.e + log.l.a
    #-----------------------------------------------------------------------------------------
    
    
    #-----------------------------------Volatility fixed model calculations-------------------
    
    #Find likelihood for evidence variable 
    #e.p.vf <- dbeta(e, delta.mean.vf[i], epsilon.mean.vf[i])
    e.p.vf <- dnorm(e, e.mean.estimated.vf[i], e.sd.estimated.vf[i])
    log.l.e <- 0
    
    if(e.p.vf != 0)
      log.l.e <- log(e.p.vf)
    
    #Find likelihood for action variable
    #a.p.vf <- dbeta(a, theta.mean.vf[i], phi.mean.vf[i])
    a.p.vf <- dnorm(a, a.mean.estimated.vf[i], a.sd.estimated.vf[i])
    log.l.a <- 0
    
    if(a.p.vf != 0)
      log.l.a <- log(a.p.vf)
    
    #Add both to overall log likelihood
    log.likelihood.vf.sum <- log.likelihood.vf.sum + log.l.e + log.l.a
    
    #-------------------------------------------------------------------------------------------
  }
  
  #---------------------Get AICs----------------------------------------------------------------
  
  aic.list <- calculate.aic(log.likelihood.vts.sum, log.likelihood.vf.sum)
  
  aic.vts <- aic.list[1]
  aic.vf <- aic.list[2]
  
  #---------------------------------------------------------------------------------------------
  
  #---------------------Generate output files---------------------------------------------------
  
  data.in.file.length <- nchar(data.file)
  
  output.dir <- paste(getwd(),'/likelihoods',sep='')
  dir.create(output.dir, showWarnings = FALSE)

  data.out <- data.frame(log.likelihood.vts.sum, aic.vts, log.likelihood.vf.sum, aic.vf)
   
  data.out.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_likelihoods.csv', sep="")
  
  data.out.file.path = paste('/likelihoods/', data.out.file.name, sep="")
  data.out.file.path = paste(getwd(), data.out.file.path, sep="")

  print(paste("Writing file", data.out.file.path))
  write.table(data.out, file=data.out.file.path, row.names=FALSE, col.names=FALSE, sep=',')
  
  print('Done!')

  #----------------------------------------------------------------------------------------------
  
  
  return(c(log.likelihood.vts.sum, log.likelihood.vf.sum))
}

calculate.beta.info <- function(alpha, beta){
  
  mean <- 0
  
  ifelse(alpha + beta != 0, mean <- ((alpha) / (alpha + beta)), mean <- 0)
  
  sd <- 0
  var <- 0

  denom <- ((alpha + beta)^2) * (alpha + beta + 1)
  
  ifelse(denom != 0, var <- ((alpha * beta) / denom), var <- 0)
  ifelse(var != 0, sd <- sqrt(var), sd <- 0)
  
  return(list(mean, sd))
  
}

calculate.aic <- function(log.likelihood.vts, log.likelihood.vf){
  num.params.vts <- 405
  num.params.vf <- 304
  
  aic.vts <- (2 * num.params.vts) - (2 * log.likelihood.vts)
  aic.vf <- (2 * num.params.vf) - (2 * log.likelihood.vf)
  
  return(c(aic.vts, aic.vf))
}

calculate.bic <- function(log.likelihood.vts, log.likelihood.vf){
  
}

calculate.all.likelihoods <- function(model.results.dir, model.data.dir){
  
  range <- c(5,6,8,10,11,13,15,16,18,19,20,seq(23,28),30,31,seq(33,36),39,40,seq(42,45))
  
  for(i in range){
    
    for(type in c('social','house')){
      compute.log.likelihoods(model.results.dir = model.results.dir, model.data.dir = model.data.dir, subject.number = i, type = type)
    } 
  }
}

average.aic <- function(likelihood.dir){
  
  aic.vts <- 0
  aic.vf <- 0
  
  range <- c(5,6,8,10,11,13,15,16,18,19,20,seq(23,28),30,31,seq(33,36),39,40,seq(42,45))
  
  for(subject.number in range){
    
    for(type in c('social','house')){
      
      if(subject.number < 10){
        data.file <- paste(likelihood.dir,'/subj_00',subject.number,'_',type,'_likelihoods.csv',sep='')
      }
      else{
        data.file <- paste(likelihood.dir,'/subj_0',subject.number,'_',type,'_likelihoods.csv',sep='')
      }
      
      data.csv <- read.csv(data.file, header=FALSE)
      data.mat <- as.matrix(data.csv)
      
      aic.vts <- aic.vts + data.mat[2]
      aic.vf <- aic.vf + data.mat[4]
    } 
  }
  
  aic.vts <- aic.vts / length(range)
  aic.vf <- aic.vf / length(range)
  
  return(c(aic.vts, aic.vf))
}


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
#' Output is in CSV format. File names are as follows: \cr\cr <data.file.name>_ts_*.csv - The "_ts_" (time-series) indicates that these are the results for the samples of the time series variables \cr\cr <data.file.name>_s_*.csv - The "_s_" (stationary) indicates that these are the results for the samples of the time stationary variables \cr\cr <data.file.name>_*_vts.csv - The "_vts" (volatility time series) indicates that these are the results for the samples of the variables with v being a time series variable \cr\cr <data.file.name>_*_vf.csv - The "_vf" (volatility fixed) indicates that these are the results for the samples of the variables with v being a time stationary variable \cr\cr\cr\cr Columns are as follows: \cr\cr *_ts_vts.csv - expectation mean, expectation standard deviation, volatility mean, volatility standard deviation \cr\cr *_s_vts.csv - k mean, k standard deviation, expectation beta parameter mean, expectation beta parameter standard deviation, action beta parameter mean, action beta parameter standard deviation \cr\cr *_ts_vf.csv - expectation mean, expectation standard deviation \cr\cr *_s_vf.csv - volatility mean, volatility standard deviation, expectation beta parameter mean, expectation beta parameter standard deviation, action beta parameter mean, action beta parameter standard deviation
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
    monitors <- c('r','v','delta','epsilon','theta','phi','k','beta.e','beta.a')
  }
  else{
    monitors <- c('r','v','delta','epsilon','theta','phi','beta.e','beta.a')
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
  
  delta.results <- jagsresults(x = model.fit, params = c('delta'))
  delta.mean <- delta.results[,c('mean')]
  delta.sd <- delta.results[,c('sd')]
  
  epsilon.results <- jagsresults(x = model.fit, params = c('epsilon'))
  epsilon.mean <- epsilon.results[,c('mean')]
  epsilon.sd <- epsilon.results[,c('sd')]
  
  theta.results <- jagsresults(x = model.fit, params = c('theta'))
  theta.mean <- theta.results[,c('mean')]
  theta.sd <- theta.results[,c('sd')]
  
  phi.results <- jagsresults(x = model.fit, params = c('phi'))
  phi.mean <- phi.results[,c('mean')]
  phi.sd <- phi.results[,c('sd')]
  
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
    data.out.time.series <- data.frame(r.mean, r.sd, delta.mean, delta.sd, epsilon.mean, epsilon.sd, theta.mean, theta.sd, phi.mean, phi.sd, v.mean, v.sd)
    data.out.stationary <- data.frame(k.mean, k.sd, beta.e.mean, beta.e.sd, beta.a.mean, beta.a.sd)
    
    data.out.time.series.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_ts_vts.csv', sep="")
    data.out.stationary.file.name = paste(substr(data.file, data.in.file.length - 18, data.in.file.length - 4), '_results_s_vts.csv', sep="")
  }
  else {
    data.out.time.series <- data.frame(r.mean, r.sd, delta.mean, delta.sd, epsilon.mean, epsilon.sd, theta.mean, theta.sd, phi.mean, phi.sd)
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
