
#' runModel
#'
#' Runs Behren's model with R2jags and plots means of expecation and volatility through time.
#' @param modelFile The path to the text file containing the model
#' @param dataFile The path to the csv file containing the data
#' @param n.chains The amount of Markov chains to run
#' @param n.samples The amount of samples to take. More samples = more convergence
#' @param n.burnin The amount of burnin samples to discard
#' @keywords behren model
#' @export
#' @examples
#' runModel(modelFile=".../model.txt", dataFile=".../data/subj_005_social.csv", n.chains=1, n.samples=15000, n.burnin=1000)

runModel <- function(modelFile, dataFile, n.chains, n.samples, n.burnin) {

  #modelFile <- 'C:/Users/Jeff/Documents/SUPER_2014/JAGS/BehrensModel/model-fixed-volatility.txt'
  #dataFile <- 'C:/Users/Jeff/Documents/SUPER_2014/JAGS/BehrensModel/Data/subj_006_social.csv'
  
  set.seed(1234)
  
  dataCSV <- read.csv(dataFile, header=FALSE)
  
  dataMat <- as.matrix(dataCSV)
  dataMat[,3] <- replace(dataMat[,3], dataMat[,3] == 2, 0)
  model.data <- list(a = dataMat[,3], e = dataMat[,4], N = 100)
  
  monitors <- c('r','v','beta.e','beta.a','k')
  
  model.fit <- jags(model.file = modelFile, data = model.data, parameters.to.save = monitors, n.chains = 1, n.iter = n.samples, n.burnin = n.burnin)
  
  #This was giving errors and aborting the function, so the samples are just specified now
  #model.fit <- autojags(model.fit)
  
  r.results <- jagsresults(x = model.fit, params = c('r'))
  r.mean <- r.results[,c('mean')]
  
  v.results <- jagsresults(x = model.fit, params = c('v'))
  v.mean <- v.results[,c('mean')]
  
  dev.off()
  dev.new()
  
  par(mfrow = c(2,1))
  
  plot(x = r.mean, type = 'l', main = "Expectation through time", xlab = "time", ylab = "expectation (r)")
  
  plot(x = v.mean, type = 'l', main = "Belief of volatility through time", xlab = "time", ylab = "volatility (v)")
}