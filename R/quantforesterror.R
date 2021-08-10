# avoid check note
if(getRversion() >= "2.15.1"){utils::globalVariables(c("n.test", "tree", "terminal_node",
                                                     ".", "oob_error", "rowid_test", "pred",
                                                     "node_errs", "pred", "all_errs", "..col_names",
                                                     "..p_col_names", "..q_col_names"))}

#' Quantify random forest prediction error
#'
#' Estimates the conditional misclassification rates, conditional mean squared
#' prediction errors, conditional biases, conditional prediction intervals, and
#' conditional error distributions of random forest predictions.
#'
#' This function accepts classification or regression random forests built using
#' the \code{randomForest}, \code{ranger}, \code{randomForestSRC}, and
#' \code{quantregForest} packages. When training the random forest using
#' \code{randomForest}, \code{ranger}, or \code{quantregForest}, \code{keep.inbag}
#' must be set to \code{TRUE}. When training the random forest using
#' \code{randomForestSRC}, \code{membership} must be set to \code{TRUE}.
#'
#' The predictions computed by \code{ranger} can be parallelized by setting the
#' value of \code{n.cores} to be greater than 1.
#'
#' The random forest predictions are always returned as a \code{data.frame}. Additional
#' columns are included in the \code{data.frame} depending on the user's selections in
#' the argument \code{what}. In particular, including \code{"mspe"} in \code{what}
#' will add an additional column with the conditional mean squared prediction
#' error of each test prediction to the \code{data.frame}; including \code{"bias"} in
#' \code{what} will add an additional column with the conditional bias of each test
#' prediction to the \code{data.frame}; including \code{"interval"} in \code{what}
#' will add to the \code{data.frame} additional columns with the lower and
#' upper bounds of conditional prediction intervals for each test prediction;
#' and including \code{"mcr"} in \code{what} will add an additional column with
#' the conditional misclassification rate of each test prediction to the
#' \code{data.frame}. The conditional misclassification rate can be estimated
#' only for classification random forests, while the other parameters can be
#' estimated only for regression random forests.
#'
#' If \code{"p.error"} or \code{"q.error"} is included in \code{what}, or if
#' \code{return_train_nodes} is set to \code{TRUE}, then a list will be returned
#' as output. The first element of the list, named \code{"estimates"}, is the
#' \code{data.frame} described in the above paragraph. The other elements of the
#' list are the estimated cumulative distribution functions (\code{perror}) of
#' the conditional error distributions, the estimated quantile functions
#' (\code{qerror}) of the conditional error distributions, and/or a \code{data.table}
#' indicating what out-of-bag prediction errors each terminal node of each tree
#' in the random forest contains.
#'
#' @param forest The random forest object being used for prediction.
#' @param X.train A \code{matrix} or \code{data.frame} with the observations
#'   that were used to train \code{forest}. Each row should be an observation,
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
#'   conditional error distribution functions (\code{"p.error"}), conditional
#'   error quantile functions (\code{"q.error"}), and conditional
#'   misclassification rate (\code{"mcr"}). Note that the conditional
#'   misclassification rate is available only for categorical outcomes, while
#'   the other parameters are available only for real-valued outcomes.
#' @param alpha A vector of type-I error rates desired for the conditional prediction
#'   intervals; required if \code{"interval"} is included in \code{what}.
#' @param train_nodes A \code{data.table} indicating what out-of-bag prediction
#'   errors each terminal node of each tree in \code{forest} contains. It should
#'   be formatted like the output of \code{findOOBErrors}. If not provided,
#'   it will be computed internally.
#' @param return_train_nodes A boolean indicating whether to return the
#'   \code{train_nodes} computed and/or used.
#' @param n.cores Number of cores to use (for parallel computation in \code{ranger}).
#'
#' @return A \code{data.frame} with one or more of the following columns, as described
#'   in the details section:
#'
#'   \item{pred}{The random forest predictions of the test observations}
#'   \item{mspe}{The estimated conditional mean squared prediction errors of
#'   the random forest predictions}
#'   \item{bias}{The estimated conditional biases of the random forest
#'   predictions}
#'   \item{lower_alpha}{The estimated lower bounds of the conditional alpha-level
#'   prediction intervals for the test observations}
#'   \item{upper_alpha}{The estimated upper bounds of the conditional alpha-level
#'   prediction intervals for the test observations}
#'   \item{mcr}{The estimated conditional misclassification rate of the random
#'   forest predictions}
#'
#'   In addition, one or both of the following functions, as described in the
#'   details section:
#'
#'   \item{perror}{The estimated cumulative distribution functions of the
#'   conditional error distributions associated with the test predictions}
#'   \item{qerror}{The estimated quantile functions of the conditional error
#'   distributions associated with the test predictions}
#'
#'   In addition, if \code{return_train_nodes} is \code{TRUE}, then a \code{data.table}
#'   called \code{train_nodes} indicating what out-of-bag prediction errors each
#'   terminal node of each tree in \code{forest} contains.
#'
#' @seealso \code{\link{perror}}, \code{\link{qerror}}, \code{\link{findOOBErrors}}
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
#' train.ind <- sample(c("A", "B", "C"), n,
#'                     replace = TRUE, prob = c(0.8, 0.1, 0.1))
#' Xtrain <- airquality[train.ind == "A", -response.col]
#' Ytrain <- airquality[train.ind == "A", response.col]
#' Xtest1 <- airquality[train.ind == "B", -response.col]
#' Xtest2 <- airquality[train.ind == "C", -response.col]
#'
#' # fit regression random forest to the training data
#' rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 5,
#'                                  ntree = 500,
#'                                  keep.inbag = TRUE)
#'
#' # estimate conditional mean squared prediction errors,
#' # biases, prediction intervals, and error distribution
#' # functions for the observations in Xtest1. return
#' # train_nodes to avoid recomputation in the next
#' # line of code.
#' output1 <- quantForestError(rf, Xtrain, Xtest1,
#'                             return_train_nodes = TRUE)
#'
#' # estimate just the conditional mean squared prediction errors
#' # and prediction intervals for the observations in Xtest2.
#' # avoid recomputation by providing train_nodes from the
#' # previous line of code.
#' output2 <- quantForestError(rf, Xtrain, Xtest2,
#'                             what = c("mspe", "interval"),
#'                             train_nodes = output1$train_nodes)
#'
#' # for illustrative purposes, convert response to categorical
#' Ytrain <- as.factor(Ytrain > 31.5)
#'
#' # fit classification random forest to the training data
#' rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 3,
#'                                  ntree = 500,
#'                                  keep.inbag = TRUE)
#'
#' # estimate conditional misclassification rate of the
#' # predictions of Xtest1
#' output <- quantForestError(rf, Xtrain, Xtest1)
#'
#' @aliases forestError
#'
#' @importFrom stats predict
#' @import data.table
#' @importFrom purrr map_dbl
#' @export
quantForestError <- function(forest, X.train, X.test, Y.train = NULL, what = if (grepl("class", c(forest$type, forest$family, forest$treetype), TRUE)) "mcr" else c("mspe", "bias", "interval", "p.error", "q.error"), alpha = 0.05, train_nodes = NULL, return_train_nodes = FALSE, n.cores = 1) {

  # check forest, X.train, X.test arguments for issues
  checkForest(forest)
  checkXtrainXtest(X.train, X.test)
  # get number of training and test observations
  n.train <- nrow(X.train)
  n.test <- nrow(X.test)
  # check Y.train and n.cores arguments for issues
  checkYtrain(forest, Y.train, n.train)
  checkcores(n.cores)
  # check requested error parameters
  checkwhat(what, forest)

  # check what user wants to produce
  mspewhat <- "mspe" %in% what
  biaswhat <- "bias" %in% what
  intervalwhat <- "interval" %in% what
  pwhat <- "p.error" %in% what
  qwhat <- "q.error" %in% what
  mcrwhat <- "mcr" %in% what

  # compute and locate out-of-bag training errors and test predictions
  if (is.null(train_nodes)) {
    train_nodes <- findOOBErrors(forest, X.train, Y.train, n.cores)
  }
  test_nodes <- findTestPreds(forest, X.test, n.cores)

  # estimate the requested prediction error distribution parameters
  output <- estimateErrorParams(train_nodes, test_nodes, mspewhat, biaswhat,
                                intervalwhat, pwhat, qwhat, mcrwhat,
                                alpha, n.test)

  # add train_nodes if requested
  if (return_train_nodes) {
    if (pwhat | qwhat) {
      output[["train_nodes"]] <- train_nodes
    } else {
      output <- list(output, train_nodes)
      names(output) <- c("estimates", "train_nodes")
    }
  }

  # return output
  return(output)
}
