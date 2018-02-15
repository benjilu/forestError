source("dependencies.R")
source("data_generation.R")
source("prediction_error.R")
source("sim.R")

############################################## DATASET GENERATION ##################################################

################################################ CLUSTERED DATA ####################################################
# parameter specifications
nDim <- 10
sampSize <- 500
nClusters <- 5
clusterMeans <- c(1, 7, 17, 31, 49)
constantSD <- FALSE
errorSD <- c(1, 2, 3, 4, 5)
seed <- 47

# generate.clusters(num.dim, sample.size, num.clusters, cluster.means, error.SD, seed, constant.SD = TRUE)
dataset <- get.clustered.data(nDim, sampSize, nClusters, clusterMeans, errorSD, seed, constantSD)
dataset.name <- paste0("clusteredMean", paste0(clusterMeans, collapse = "_"), "SD", paste0(errorSD, collapse = "_"))

############################################### FOREST FIRES DATA ##################################################
# # parameter specifications
# address <- "~/Code/Intervals/Paper/Data/forestfires.csv"
# logTransform <- TRUE
# 
# # generate forest fires data
# dataset <- get.fires.data(address, logTransform)
# dataset.name <- "forestFires"

################################################## BOSTON DATA #####################################################
# dataset <- get.boston.data()
# dataset.name <- "boston"

################################################### OZONE DATA #####################################################
# dataset <- get.ozone.data()
# dataset.name <- "ozone"

#################################################### MPG DATA ######################################################
# address <- "~/Code/Intervals/Paper/Data/mpg.csv"
# dataset <- get.mpg.data(address)
# dataset.name <- "mpg"
# print(names(dataset))

######################################## GENERAL SIMULATION PARAMETERS #############################################
n.tree <- 500
node.size <- 15
nReps <- 40
seed <- 47
train.frac <- 0.75
response.col <- ncol(dataset)

############################################## EXECUTE SIMULATION ##################################################
# compare.intervals(reps, seed)
compare.intervals(nReps, seed)