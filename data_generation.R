#####
# generates data that is clustered. the response values of observations within a given cluster are drawn from the same normal distribution
#
# num.dim (double): number of predictor variables for the generated dataset
# sample.size (double): sample size for the generated dataset
# num.clusters (double): number of clusters into which the data will be divided. Each cluster will contain roughly, but not necessarily exactly, the same number of observations
# cluster.means (vector): a vector of length num.clusters specifying the mean response value for observations in each cluster
# error.SD (double/vector): the standard deviation of the error term. If constant.SD is FALSE, this must be a vector of length num.clusters
# seed (double): seed state
# constant.SD (boolean): TRUE if the standard deviation of the error term is the same for each cluster, FALSE if the standard deviation of the error term is not the same for each cluster
# RETURNS: a generated sample in a sample.size x (num.dim + 1) dataframe
#
#####
get.clustered.data <- function(num.dim, sample.size, num.clusters, cluster.means, error.SD, seed, constant.SD = TRUE) {
  
  # set seed
  set.seed(seed)
  
  # use the MixSim package to generate a finite mixture model with Gaussian components with 0 average overlap and 0 maximum overlap of clusters desired
  repeat {
    mixModel <- MixSim(BarOmega = 0, MaxOmega = 0, K = num.clusters, p = num.dim)
    if (mixModel$fail == 0) break
  }
  
  # use the MixSim package to generate explanatory variable values for the observations, as well as a label for the cluster to which the observation belongs
  data.points <- simdataset(n = sample.size, Pi = mixModel$Pi, Mu = mixModel$Mu, S = mixModel$S)
  
  # if error standard deviation is the same for all clusters
  if (constant.SD) {
    # each observation's response value is its cluster's mean response plus an error term with fixed standard deviation
    data.points$y <- cluster.means[data.points$id] + rnorm(sample.size, 0, error.SD)
  } else {
    # each observation's response value is its cluster's mean response plus an error term with cluster-specific standard deviation
    data.points$y <- cluster.means[data.points$id] + rnorm(sample.size, 0, error.SD[data.points$id])
  }
  
  # save sample
  write.csv(data.points, paste0(Sys.Date(), "clustered", "Mean", paste0(cluster.means, collapse = "_"), "SD", paste0(error.SD, collapse = "_"), "Dataset.csv"), row.names = FALSE)
  
  # convert to dataframe and remove cluster label
  data.points <- as.data.frame(cbind(xvals = data.points$X, yvals = data.points$y))
  
  # return dataframe
  return(data.points)
}

####
# gets Forest Fires data and (optional) log-transforms the response variable
# note: the dataset must be downloaded from the University of California, Irvine Machine Learning Repository
# http://archive.ics.uci.edu/ml/datasets/Forest+Fires
#
# address (character): the directory address of the dataset
# log.transform (boolean): TRUE if the response variable should be log-transformed, else FALSE
# RETURNS: the Forest Fires dataset
#
####
get.fires.data <- function(address, log.transform = TRUE) {
  
  df <- read.csv(address)
  
  if (log.transform) {
    df$area <- log(df$area + 1)
  }
  
  return(df)
}

####
# gets Boston data
#
# RETURNS: The Boston dataset
#
####
get.boston.data <- function() {
  
  require(MASS)
  
  return(Boston)
}

####
# gets Ozone data and omits observations with NA values, following Meinshausen (2006)
#
# RETURNS: The Ozone dataset without observations with missing values
#
####
get.ozone.data <- function() {
  
  require(mlbench)
  
  data(Ozone)
  
  Ozone <- na.omit(Ozone)
  
  Ozone <- Ozone[, c(1:3, 5:ncol(Ozone), 4)]
  
  return(Ozone)
}

####
# gets the MPG dataset
#
# address (character): the directory address of the MPG dataset
# RETURNS: The MPG dataset
#
####
get.mpg.data <- function(address) {
  
  df <- read.csv(address)
  
  return(df[, -1])
}