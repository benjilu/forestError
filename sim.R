#####
# function to compile parallel output
#
#####
comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}

#####
# main function. returns empirical simulated capture rates of prediction intervals generated using a variety of variance estimators
#
# dataset (dataframe): the full dataframe of observations
# address (string): address where results should be saved
# reps (double): number of repetitions of simulation
# train.frac (double): fraction of observations to train the random forest
# response.col (double): the column index of the response values in dataset
# node.size (double): minimum number of in-bag training observations required in a node for the decision tree to split the node further
# n.tree (double): number of decision trees in forest
# seed (double): seed for random number generation
# RETURNS: dataframe of simulation results
#
#####
compare.intervals <- function(reps, seed) {

  ### setup for parallel processing ###
  num.cluster1 <- 25
  num.runs1 <- reps * num.cluster1
  
  run1 <- list()
  length(run1) <- num.runs1
  
  c1 <- makeCluster(num.cluster1)
  registerDoParallel(c1)
  
  # set seed
  registerDoRNG(seed)
  
  # do the following many times in parallel
  results.sim <- foreach(p = 1:num.runs1, .combine = "comb", .multicombine = TRUE, .packages = c("randomForest", "dplyr", "tidyr", "quantregForest"), .export = c("get.forest.breakdown", "get.mse1.mse2", "get.mse3", "train.frac", "dataset", "response.col", "node.size", "n.tree", "dataset.name")) %dopar% {
    
    # number of observations
    n.obs <- nrow(dataset)
    
    # randomly sample indices for training data set
    training <- sample(n.obs, train.frac * n.obs)
    
    # number of training and test observations
    n.train <- length(training)
    n.test <- n.obs - n.train
    
    # define training and test data sets
    X.train <- dataset[training, -response.col]
    X.test <- dataset[-training, -response.col]
    Y.train <- dataset[training, response.col]
    Y.test <- dataset[-training, response.col]
    
    # get test indices
    test.indices <- as.numeric(rownames(X.test))
    
    # grow quantile regression forest (Meinshausen 2006) on training data
    forest <- quantregForest(X.train, Y.train, nodesize = node.size, ntree = n.tree, keep.inbag = TRUE)
    
    # get 0.025, 0.5, and 0.975 quantiles
    quantiles <- predict(forest, X.test, what = c(0.025, 0.5, 0.975))
    
    # get 0.025, 0.5, and 0.975 percentiles of ECDFs
    ecdfs <- predict(forest, X.test, what = ecdf)
    lower.bound.ecdf <- sapply(ecdfs, function(x) knots(x)[which.max(x(knots(x)) >= 0.025)])
    upper.bound.ecdf <- sapply(ecdfs, function(x) knots(x)[which.min(x(knots(x)) <= 0.975)])
    median.ecdf <- sapply(ecdfs, function(x) knots(x)[which.max(x(knots(x)) >= 0.5)])
    
    # convert to regular random forest
    class(forest) <- "randomForest"

    # get terminal nodes of all observations
    train.terminal.nodes <- attr(predict(forest, X.train, nodes = TRUE), "nodes")
    test.terminal.nodes <- attr(predict(forest, X.test, nodes = TRUE), "nodes")
    
    # get test observation predictions
    test.preds <- predict(forest, X.test)
    
    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag
    
    # compile tree index, terminal node index, training index, tree prediction, and true response value for out-of-bag training observations
    oobs <- get.forest.breakdown(forest, train.terminal.nodes, bag.count, Y.train, "oob")
    
    # calculate MSPE1 and NA counter and associated prediction interval bounds
    MSPE1.and.counter <- get.mspe1(forest, dataset)
    MSPE1 <- MSPE1.and.counter[1]
    na.counter <- MSPE1.and.counter[2]
    lower.bound.MSPE1 <- test.preds - 2 * sqrt(MSPE1)
    upper.bound.MSPE1 <- test.preds + 2 * sqrt(MSPE1)
    
    # calculate MSPE2 and associated prediction interval bounds
    MSPE2 <- apply(test.terminal.nodes, 1, function(x) get.mspe2(oobs[oobs$Node == x[oobs$Tree], ], oobs))
    lower.bound.MSPE2 <- test.preds - 2 * sqrt(MSPE2)
    upper.bound.MSPE2 <- test.preds + 2 * sqrt(MSPE2)
    
    # fit linear regression model to data
    lin.reg <- lm(as.formula(paste0("Y.train ~ ", paste0(names(dataset)[-response.col], collapse = " + "), collapse = "")),
                  data = data.frame(X.train, Y.train))
    
    # get linear regression predictions
    pred.lin.reg <- predict.lm(lin.reg, newdata = X.test)
    
    # calculate linear regression 95% prediction interval
    lin.reg.pred.intervals <- predict.lm(lin.reg, newdata = X.test, se.fit=TRUE, interval="prediction")
    
    # get prediction interval bounds
    lower.bound.lin.reg <- lin.reg.pred.intervals$fit[, 2]
    upper.bound.lin.reg <- lin.reg.pred.intervals$fit[, 3]
    
    # combine data
    df <- data.frame(p, test.indices, MSPE1, MSPE2, na.counter, test.preds, Y.test, lower.bound.MSPE1, upper.bound.MSPE1, lower.bound.MSPE2, upper.bound.MSPE2, lower.bound.ecdf, median.ecdf, upper.bound.ecdf, quantiles, lower.bound.lin.reg, pred.lin.reg, upper.bound.lin.reg)
    
    # name columns
    names(df) <- c("repetition", "observation", "MSPE1", "MSPE2", "NACounter", "pred", "true", "lower.MSPE1", "upper.MSPE1", "lower.MSPE2", "upper.MSPE2", "lower.ecdf", "median.ecdf", "upper.ecdf", "lower.qrf", "median.qrf", "upper.qrf", "lower.linreg", "pred.linreg", "upper.linreg")
    
    # calculate capture rates
    capture.rates <- c(sum(df$lower.MSPE1 <= df$true & df$true <= df$upper.MSPE1) / n.test, 
                       sum(df$lower.MSPE2 <= df$true & df$true <= df$upper.MSPE2) / n.test, 
                       sum(df$lower.ecdf <= df$true & df$true <= df$upper.ecdf) / n.test, 
                       sum(df$lower.qrf <= df$true & df$true <= df$upper.qrf) / n.test,
                       sum(df$lower.linreg <= df$true & df$true <= df$upper.linreg) /n.test
    )
    
    # return output
    list(df, capture.rates)
  }
  
  # label output
  results.sim[[2]] <- data.frame(results.sim[[2]])
  names(results.sim[[2]]) <- c("MSPE1.CR", "MSPE2.CR", "ECDF.CR", "QRF.CR", "LINREG.CR")
  
  # close connections
  closeAllConnections()
  
  # save final outputs
  write.csv(results.sim[[1]], paste0(Sys.Date(), dataset.name, "detailedOutput.csv"), row.names = FALSE)
  write.csv(results.sim[[2]], paste0(Sys.Date(), dataset.name, "captureRates.csv"), row.names = FALSE)
}