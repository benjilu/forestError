# avoid check note
if(getRversion() >= "2.15.1"){utils::globalVariables(c("n.test", "ordered.oob.errors", "cumsums", "unit"))}

#' Quantify random forest prediction error
#'
#' Estimates the conditional mean squared prediction errors, conditional biases,
#' conditional prediction intervals, and conditional error distributions of
#' random forest predictions.
#'
#' This function accepts regression random forests built using the \code{randomForest},
#' \code{ranger}, \code{randomForestSRC}, and \code{quantregForest} packages.
#' When training the random forest using \code{randomForest}, \code{ranger}, or
#' \code{quantregForest}, \code{keep.inbag} must be set to \code{TRUE}. When
#' training the random forest using \code{randomForestSRC}, \code{membership}
#' must be set to \code{TRUE}.
#'
#' The computation can be parallelized by setting the value of \code{n.cores} to
#' be greater than 1.
#'
#' The random forest predictions are always returned as a \code{data.frame}. Additional
#' columns are included in the \code{data.frame} depending on the user's selections in
#' the argument \code{what}. In particular, including \code{"mspe"} in \code{what}
#' will add an additional column with the conditional mean squared prediction
#' error of each test prediction to the \code{data.frame}; including \code{"bias"} in
#' \code{what} will add an additional column with the conditional bias of each test
#' prediction to the \code{data.frame}; and including \code{"interval"} in \code{what}
#' will add to the \code{data.frame} two additional columns with the lower and
#' upper bounds of a conditional prediction interval for each test prediction.
#'
#' If \code{"p.error"} or \code{"q.error"} is included in \code{what}, then a
#' list will be returned as output. The first element of the list, named
#' \code{"estimates"}, is the \code{data.frame} described in the above paragraph. The
#' other one or two elements of the list are the estimated cumulative distribution
#' functions (\code{perror}) and/or the estimated quantile functions (\code{qerror})
#' of the conditional error distributions associated with the test predictions.
#'
#' @param forest The random forest object being used for prediction.
#' @param X.train A \code{matrix} or \code{data.frame} with the observations
#'   that were used to train \code{forest}; each row should be an observation,
#'   and each column should be a predictor variable.
#' @param X.test A \code{matrix} or \code{data.frame} with the observations to
#'   be predicted; each row should be an observation, and each column should be
#'   a predictor variable.
#' @param Y.train A vector of the responses of the observations that were used
#'   to train \code{forest}. Required if \code{forest} was created using
#'   \code{ranger}, but not if \code{forest} was created using \code{randomForest},
#'   \code{randomForestSRC}, or \code{quantregForest}.
#' @param what A vector of characters indicating what estimates are desired.
#'   Possible options are conditional mean squared prediction errors (\code{"mspe"}),
#'   conditional biases (\code{"bias"}), conditional prediction intervals (\code{"interval"}),
#'   conditional error distribution functions (\code{"p.error"}), and
#'   conditional error quantile functions (\code{"q.error"}).
#' @param alpha The type-I error rate desired for the conditional prediction
#'   intervals; required if \code{"interval"} is included in \code{what}.
#' @param n.cores Number of cores to use (for parallel computation).
#'
#' @return A \code{data.frame} with one or more of the following columns, as described
#'   in the details section:
#'
#'   \item{pred}{The random forest predictions of the test observations}
#'   \item{mspe}{The estimated conditional mean squared prediction errors of
#'   the random forest predictions}
#'   \item{bias}{The estimated conditional biases of the random forest
#'   predictions}
#'   \item{lower}{The estimated lower bounds of the conditional prediction
#'   intervals for the test observations}
#'   \item{upper}{The estimated upper bounds of the conditional prediction
#'   intervals for the test observations}
#'
#'   In addition, one or both of the following functions, as described in the
#'   details section:
#'
#'   \item{perror}{The estimated cumulative distribution functions of the
#'   conditional error distributions associated with the test predictions}
#'   \item{qerror}{The estimated quantile functions of the conditional error
#'   distributions associated with the test predictions}
#'
#' @seealso \code{\link{perror}}, \code{\link{qerror}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#'
#' @examples
#' # load data
#' data(airquality)
#'
#' # remove observations with missing predictor variable values
#' airquality <- airquality[complete.cases(airquality), ]
#'
#' # get number of observations and the response column index
#' n <- nrow(airquality)
#' response.col <- 1
#'
#' # split data into training and test sets
#' train.ind <- sample(1:n, n * 0.9, replace = FALSE)
#' Xtrain <- airquality[train.ind, -response.col]
#' Ytrain <- airquality[train.ind, response.col]
#' Xtest <- airquality[-train.ind, -response.col]
#' Ytest <- airquality[-train.ind, response.col]
#'
#' # fit random forest to the training data
#' rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 5,
#'                                  ntree = 500,
#'                                  keep.inbag = TRUE)
#'
#' # estimate conditional mean squared prediction errors,
#' # biases, prediction intervals, and error distribution
#' # functions for the test observations
#' output <- quantForestError(rf, Xtrain, Xtest,
#'                            alpha = 0.05)
#'
#' # do the same as above but in parallel
#' output <- quantForestError(rf, Xtrain, Xtest, alpha = 0.05,
#'                            n.cores = 2)
#'
#' # estimate just the conditional mean squared prediction errors
#' # and prediction intervals for the test observations
#' output <- quantForestError(rf, Xtrain, Xtest,
#'                            what = c("mspe", "interval"),
#'                            alpha = 0.05)
#'
#' # estimate just the conditional error distribution
#' # functions for the test observations
#' output <- quantForestError(rf, Xtrain, Xtest,
#'                            what = c("p.error", "q.error"))
#' @aliases forestError
#'
#' @useDynLib forestError
#' @importFrom Rcpp sourceCpp
#' @importFrom stats predict
#' @importFrom foreach %dopar% foreach
#' @importFrom doParallel registerDoParallel
#' @export
quantForestError <- function(forest, X.train, X.test, Y.train = NULL, what = c("mspe", "bias", "interval", "p.error", "q.error"), alpha = 0.05, n.cores = 1) {

  # check forest, X.train, X.test arguments for issues
  checkForest(forest)
  checkXtrainXtest(X.train, X.test)
  # get number of training and test observations
  n.train <- nrow(X.train)
  n.test <- nrow(X.test)
  # check Y.train and n.cores arguments for issues
  checkYtrain(forest, Y.train, n.train)
  checkcores(n.cores)

  # if the forest is from the quantregForest package
  if ("quantregForest" %in% class(forest)) {

    # convert to random forest class
    class(forest) <- "randomForest"

    # get test predictions
    test.pred.list <- predict(forest, X.test, nodes = TRUE)

    # get terminal nodes of all observations
    train.terminal.nodes <- attr(predict(forest, X.train, nodes = TRUE), "nodes")
    test.terminal.nodes <- attr(test.pred.list, "nodes")

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$y - forest$predicted

    # get test observation predictions
    attributes(test.pred.list) <- NULL
    test.preds <- test.pred.list

  # else if the forest is from the randomForest package
  } else if ("randomForest" %in% class(forest)) {

    # get test predictions
    test.pred.list <- predict(forest, X.test, nodes = TRUE)

    # get terminal nodes of all observations
    train.terminal.nodes <- attr(predict(forest, X.train, nodes = TRUE), "nodes")
    test.terminal.nodes <- attr(test.pred.list, "nodes")

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$y - forest$predicted

    # get test observation predictions
    attributes(test.pred.list) <- NULL
    test.preds <- test.pred.list

  # else, if the forest is from the ranger package
  } else if ("ranger" %in% class(forest)) {

    # get terminal nodes of all observations
    train.terminal.nodes <- predict(forest, X.train, type = "terminalNodes")$predictions
    test.terminal.nodes <- predict(forest, X.test, type = "terminalNodes")$predictions

    # get number of times each training observation appears in each tree
    bag.count <- matrix(unlist(forest$inbag.counts, use.names = FALSE), ncol = forest$num.trees, byrow = FALSE)

    # get the OOB prediction error of each training observation
    oob.errors <- Y.train - forest$predictions

    # get test observation predictions
    test.preds <- predict(forest, X.test)$predictions

  # else, if the forset is from the randomForestSRC package
  } else if ("rfsrc" %in% class(forest)) {

    # get test predictions
    test.pred.list <- predict(forest, X.test, membership = TRUE)

    # get terminal nodes of all observations
    train.terminal.nodes <- forest$membership
    test.terminal.nodes <- test.pred.list$membership

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$yvar - forest$predicted.oob

    # get test observation predictions
    test.preds <- test.pred.list$predicted
  }

  # get the terminal nodes of the training observations in the trees in which they are OOB
  # (for all other trees, set the terminal node to be 0)
  train.oob.terminal.nodes <- train.terminal.nodes * as.numeric(bag.count == 0)

  ##############################################################################
  ### run C++ function to compute out-of-bag cohabitants either in parallel or not
  ##############################################################################

  # if user specifies a number of cores for parallel processing
  if (n.cores > 1) {

    # if the number of cores is larger than both n.train and n.test, revise it down
    if (n.cores > n.train & n.cores > n.test) {
      n.cores <- max(n.train, n.test)
    }

    # if the number of cores is larger than only one set, parallelize over the other
    if (n.cores > n.train & n.cores <= n.test) {

      # construct cluster
      cl <- parallel::makeCluster(n.cores)

      # register parallel backend
      doParallel::registerDoParallel(cl)

      # count out-of-bag cohabitants in parallel over the test units
      oob.weights <-
        foreach::foreach(unit = 1:n.test,
                         .combine = "rbind",
                         .packages = "forestError") %dopar% {
                           countOOBCohabitantsTestPar(train.oob.terminal.nodes, test.terminal.nodes[unit, ], n.train)
                         }

      # shut down the cluster.
      parallel::stopCluster(cl)

      # rename rows
      rownames(oob.weights) <- NULL

    } else if (n.cores <= n.train & n.cores > n.test) {

      # construct cluster
      cl <- parallel::makeCluster(n.cores)

      # register parallel backend
      doParallel::registerDoParallel(cl)

      # count out-of-bag cohabitants in parallel over the training units
      oob.weights <-
        foreach::foreach(unit = 1:n.train,
                         .combine = "cbind",
                         .packages = "forestError") %dopar% {
                           countOOBCohabitantsTrainPar(train.oob.terminal.nodes[unit, ], test.terminal.nodes, n.test)
                         }

      # shut down the cluster.
      parallel::stopCluster(cl)

      # rename columns
      colnames(oob.weights) <- NULL

    # else, check the relative sizes of the training and test sets
    } else if (n.train > 10 * n.test) {

      # construct cluster
      cl <- parallel::makeCluster(n.cores)

      # register parallel backend
      doParallel::registerDoParallel(cl)

      # count out-of-bag cohabitants in parallel over the test units
      oob.weights <-
        foreach::foreach(unit = 1:n.test,
                         .combine = "rbind",
                         .packages = "forestError") %dopar% {
                           countOOBCohabitantsTestPar(train.oob.terminal.nodes, test.terminal.nodes[unit, ], n.train)
                         }

      # shut down the cluster.
      parallel::stopCluster(cl)

      # rename rows
      rownames(oob.weights) <- NULL

    } else {

      # construct cluster
      cl <- parallel::makeCluster(n.cores)

      # register parallel backend
      doParallel::registerDoParallel(cl)

      # count out-of-bag cohabitants in parallel over the training units
      oob.weights <-
        foreach::foreach(unit = 1:n.train,
                         .combine = "cbind",
                         .packages = "forestError") %dopar% {
                           countOOBCohabitantsTrainPar(train.oob.terminal.nodes[unit, ], test.terminal.nodes, n.test)
                         }

      # shut down the cluster.
      parallel::stopCluster(cl)

      # rename columns
      colnames(oob.weights) <- NULL
    }

    # else, count out-of-bag cohabitants without parallel computation
  } else {

    oob.weights <- countOOBCohabitants(train.oob.terminal.nodes, test.terminal.nodes, n.train, n.test)
  }

  ##############################################################################
  ### end C++ section
  ##############################################################################

  # for each test observation, convert the number of times each training observation
  # is an OOB cohabitant to the proportion of times each training observation is an
  # OOB cohabitant
  oob.weights <- oob.weights / rowSums(oob.weights)

  # check what user wants to produce
  mspewhat <- "mspe" %in% what
  biaswhat <- "bias" %in% what
  intervalwhat <- "interval" %in% what
  pwhat <- "p.error" %in% what
  qwhat <- "q.error" %in% what

  # calculate the number of columns the output dataframe will have
  output.cols <- 1 + as.numeric(mspewhat) + as.numeric(biaswhat) + (2 * as.numeric(intervalwhat))

  # current column tracker
  current.col <- 1

  # initialize output dataframe
  output.df <- data.frame(matrix(rep(NA, n.test * output.cols), ncol = output.cols))

  # add test predictions to output dataframe
  output.df[, 1] <- test.preds

  # increment column tracker
  current.col <- current.col + 1

  # initialize overall output
  output <- NULL

  # if user requests mean squared prediction error estimates
  if (mspewhat) {

    # get the MSPE2 for each test observation
    mspe2s <- as.matrix(oob.weights) %*% (oob.errors ^ 2)

    # add to output dataframe
    output.df[, current.col] <- as.vector(mspe2s)

    # increment column tracker
    current.col <- current.col + 1
  }

  # if user requests bias estimates
  if (biaswhat) {

    # get the bias for each test observation
    biases <- as.matrix(oob.weights) %*% oob.errors

    # add to output dataframe
    output.df[, current.col] <- as.vector(biases)

    # increment column tracker
    current.col <- current.col + 1
  }

  # if the user requests anything else
  if (any(c(intervalwhat, pwhat, qwhat))) {

    # sort the OOB errors of the training observations
    ordered.oob.errors <- sort(oob.errors, index.return = TRUE)

    # sort the OOB weight assigned to each OOB error according to the value of
    # the OOB error, in ascending order
    ordered.oob.weights <- oob.weights[, ordered.oob.errors$ix]

    # get the cumulative sum of weights
    cumsums <- t(apply(ordered.oob.weights, 1, cumsum))

    # if the user requests regular prediction intervals
    if (intervalwhat) {

      # check the alpha argument for issues
      checkAlpha(alpha)

      # get the index of the training observation for which the cumulative sum of the OOB weights
      # initially exceeds alpha / 2
      lower.ind <- apply(cumsums, 1, FUN = function(x) max(suppressWarnings(min(which(x >= alpha / 2))), 1))
      # get the index of the training observation for which the cumulative sum of the OOB weights
      # initially exceeds 1 - alpha / 2
      upper.ind <- apply(cumsums, 1, FUN = function(x) min(which(x >= 1 - (alpha / 2))))

      # get the corresponding OOB errors and use them to construct prediction intervals
      # for the test observations
      lower.bounds <- test.preds + ordered.oob.errors$x[lower.ind]
      upper.bounds <- test.preds + ordered.oob.errors$x[upper.ind]

      # add to output dataframe
      output.df[, c(current.col, current.col + 1)] <- data.frame(lower.bounds, upper.bounds)

      # increment column tracker
      current.col <- current.col + 2
    }

    # if the user requests the estimated CDF
    if (pwhat) {

      # define the estimated CDF function
      perror <- function(q, xs = 1:n.test) {

        # check xs argument for issus
        checkxs(xs, n.test)

        # define the function for a single quantile
        singleperror <- function(singleq, exes) {

          # get the index of the maximum error that is less than or equal to the stated quantile
          max.error.ind <- suppressWarnings(max(which(ordered.oob.errors$x <= singleq)))

          # if the index is at least 1
          if (max.error.ind != -Inf) {

            # get the cumulative weights up to that index
            prob <- apply(matrix(cumsums[exes, ], nrow = length(exes)), 1, FUN = function(x) x[max.error.ind])

          # else
          } else {

            # the probability of being less than the inputted value is zero
            prob <- rep(0, length(exes))
          }

          # return the probability
          return(prob)
        }

        # if more than one quantile is provided, vectorize the evaluation of
        # singleperror over the quantiles
        if (length(q) > 1) {
          to.return <- data.frame(sapply(q, FUN = function(x) singleperror(x, xs)))
          if (length(xs) == 1) {
            to.return <- unlist(to.return)
            names(to.return) <- as.character(q)
          } else {
            row.names(to.return) <- xs
            names(to.return) <- q
          }
          return(to.return)
        # else, evaluate singleperror on the single quantile provided
        } else {
          return(singleperror(q, xs))
        }
      }

      # add to output
      output <- list(output.df, perror)
      names(output) <- c("estimates", "perror")
    }

    # if the user requests quantile function
    if (qwhat) {

      # define quantile function
      qerror <- function(p, xs = 1:n.test) {

        # check p and xs arguments for issus
        checkps(p)
        checkxs(xs, n.test)

        # define quantile function evaluated for single probability
        singleqerror <- function(singlep, exes) {

          return(unname(ordered.oob.errors$x[apply(matrix(cumsums[exes, ], nrow = length(exes)), 1, FUN = function(x) min(which(x >= singlep)))]))
        }

        # if more than one probability is provided, vectorize the evaluation of
        # singleqerror over the probabilities
        if (length(p) > 1) {
          to.return <- data.frame(sapply(p, FUN = function(x) singleqerror(x, xs)))
          if (length(xs) == 1) {
            to.return <- unlist(to.return)
            names(to.return) <- as.character(p)
          } else {
            row.names(to.return) <- xs
            names(to.return) <- p
          }
          return(to.return)
          # else, evaluate singleqerror on the single probability provided
        } else {
          return(singleqerror(p, xs))
        }
      }

      # add quantile function to output
      if (pwhat) {
        output[["qerror"]] <- qerror
      } else {
        output <- list(output.df, qerror)
        names(output) <- c("estimates", "qerror")
      }
    }
  }

  # return output
  if (is.null(output)) {
    names(output.df) <- c("pred", "mspe", "bias", "lower", "upper")[c(TRUE, mspewhat, biaswhat, intervalwhat, intervalwhat)]
    return(output.df)
  } else {
    names(output$estimates) <- c("pred", "mspe", "bias", "lower", "upper")[c(TRUE, mspewhat, biaswhat, intervalwhat, intervalwhat)]
    return(output)
  }
}
