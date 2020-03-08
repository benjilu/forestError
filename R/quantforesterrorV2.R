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
#' @param precomputed_train_nodes dataframe result of a prior call with the oob
#' error statistics by training node.
#' @param return_train_nodes defaults TRUE; return the oob error statistics by
#' training set leaf node in table form for faster computation on new test observations
#' in future calls.
#' @param alpha The type-I error rate desired for the conditional prediction;
#' can be a vector of values strictly between 0 and 1 with an upper/lower pred
#' interval generated for each value of alpha.
#' @param n.cores Number of cores to use in ranger predict function.
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
#' @import data.table
#' @importFrom purrr map_dbl
#' @export

quantForestErrorV2 <- function(forest, X.train, X.test, Y.train = NULL,
                               precomputed_train_nodes = NULL,
                               return_train_nodes = TRUE,
                               alpha = c(0.05, 0.5),
                               n.cores = 1) {

  # check forest, X.train, X.test arguments for issues
  checkForest(forest)
  checkXtrainXtest(X.train, X.test)
  # get number of training and test observations
  n.train <- nrow(X.train)
  n.test <- nrow(X.test)
  # check Y.train and n.cores arguments for issues
  checkYtrain(forest, Y.train, n.train)
  checkcores(n.cores)
  checkAlpha(alpha)

  if (inherits(forest, "ranger")) {

    # get test statistics
    test.terminal.nodes <- predict(forest, X.test, num.threads = n.cores, type = "terminalNodes")$predictions
    test.preds <- predict(forest, X.test)$predictions

    if (is.null(precomputed_train_nodes)) {
      train.terminal.nodes <- predict(forest, X.train, num.threads = n.cores, type = "terminalNodes")$predictions
      bag.count <- matrix(unlist(forest$inbag.counts, use.names = FALSE), ncol = forest$num.trees, byrow = FALSE)
      oob.errors <- Y.train - forest$predictions
    }
  } else {
    stop("V2 only works for forests of class ranger right now")
  }

  long_test_nodes <- melt(
    as.data.table(test.terminal.nodes)[, `:=`(rowid_test = .I, pred_test = test.preds)],
    id.vars = c("rowid_test", "pred_test"),
    measure.vars = patterns("\\d+"),
    variable.name = "tree",
    variable.factor = FALSE,
    value.name = "terminal_node"
  )

  if (is.null(precomputed_train_nodes)) {
    # get the terminal nodes of the training observations in the trees in which
    # they are OOB (for all other trees, set the terminal node to be NA)
    train.terminal.nodes[bag.count != 0] <- NA

    long_train_nodes <- as.data.table(train.terminal.nodes)
    long_train_nodes[, `:=`(oob_error = oob.errors)]
    long_train_nodes <- melt(
      long_train_nodes,
      id.vars = c("oob_error"),
      measure.vars = patterns("\\d+"),
      variable.name = "tree",
      value.name = "terminal_node",
      variable.factor = FALSE,
      na.rm = TRUE)
    long_train_nodes <- long_train_nodes[
      ,
      .(node_mspe = mean(oob_error^2),
        node_bias = mean(oob_error),
        node_errs = list(oob_error),
        node_obsN = .N),
      keyby = c("tree", "terminal_node")
      ]
  } else {
    # this is a memory/time hit but lower risk of mistakes
    long_train_nodes <- data.table::copy(precomputed_train_nodes)
  }

  data.table::setkey(long_test_nodes, tree, terminal_node)
  data.table::setkey(long_train_nodes, tree, terminal_node)

  # Join the train/test edgelists on tree/node
  oob_error_stats <- long_train_nodes[
    long_test_nodes,
    .(rowid_test, pred_test, node_mspe, node_bias, node_errs, node_obsN),
    by = .EACHI
    ]

  # Get weighted mean of node-level statistics, where weights for each node are
  # the N train observations that are cohabitants in that node. Some of these are
  # commented out now; ideas for future applications but not short-term relevant.
  oob_error_stats <- oob_error_stats[
    ,
    .(mspe = weighted.mean(node_mspe, node_obsN, na.rm = TRUE),
      bias = weighted.mean(node_bias, node_obsN, na.rm = TRUE),
      #len_errs = sum(lengths(node_errs)),
      all_errs = list(sort(unlist(node_errs)))),
    keyby = c("rowid_test", "pred_test")
    ]

  # interval calculations
  for (one_alpha in alpha) {
    cut_lwr <- one_alpha/2
    cut_upr <- 1 - one_alpha/2
    name_lwr <- paste0("predint_", cut_lwr)
    name_upr <- paste0("predint_", cut_upr)

    oob_error_stats[, (name_lwr) := pred_test +
                      purrr::map_dbl(all_errs, ~.x[ceiling(length(.x) * cut_lwr)])]
    oob_error_stats[, (name_upr) := pred_test +
                      purrr::map_dbl(all_errs, ~.x[ceiling(length(.x) * cut_upr)])]
  }

  ord1 <- c("rowid_test", "pred_test", "mspe", "bias")
  ord2 <- names(oob_error_stats)[grepl("predint_", names(oob_error_stats))]
  ord3 <- names(oob_error_stats)[!(names(oob_error_stats) %in% c(ord1, ord2))]

  data.table::setcolorder(
    oob_error_stats,
    neworder = unique(c(ord1, ord2, ord3))
  )

  if (return_train_nodes) {
    return(list(oob_error_stats = oob_error_stats,
                train_nodes = long_train_nodes))
  } else {
    return(list(oob_error_stats = oob_error_stats,
                train_nodes = NULL))
  }
}

