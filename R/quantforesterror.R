# avoid check note
if(getRversion() >= "2.15.1"){utils::globalVariables(c("n.test", "tree", "terminal_node",
                                                     ".", "oob_error", "rowid_test", "pred",
                                                     "node_errs", "pred", "all_errs", "..col_names",
                                                     "..p_col_names", "..q_col_names"))}

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
#' The predictions computed by \code{ranger} can be parallelized by setting the
#' value of \code{n.cores} to be greater than 1.
#'
#' The random forest predictions are always returned as a \code{data.frame}. Additional
#' columns are included in the \code{data.frame} depending on the user's selections in
#' the argument \code{what}. In particular, including \code{"mspe"} in \code{what}
#' will add an additional column with the conditional mean squared prediction
#' error of each test prediction to the \code{data.frame}; including \code{"bias"} in
#' \code{what} will add an additional column with the conditional bias of each test
#' prediction to the \code{data.frame}; and including \code{"interval"} in \code{what}
#' will add to the \code{data.frame} additional columns with the lower and
#' upper bounds of conditional prediction intervals for each test prediction.
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
#'   called \code{train_nodes} indicating which out-of-bag prediction errors each
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
#' # fit random forest to the training data
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
#' @aliases forestError
#'
#' @importFrom stats predict
#' @import data.table
#' @importFrom purrr map_dbl
#' @export
quantForestError <- function(forest, X.train, X.test, Y.train = NULL, what = c("mspe", "bias", "interval", "p.error", "q.error"), alpha = 0.05, train_nodes = NULL, return_train_nodes = FALSE, n.cores = 1) {

  # check forest, X.train, X.test arguments for issues
  checkForest(forest)
  checkXtrainXtest(X.train, X.test)
  # get number of training and test observations
  n.train <- nrow(X.train)
  n.test <- nrow(X.test)
  # check Y.train and n.cores arguments for issues
  checkYtrain(forest, Y.train, n.train)
  checkcores(n.cores)

  # compute and locate out-of-bag training errors and test predictions
  if (is.null(train_nodes)) {
    train_nodes <- findOOBErrors(forest, X.train, Y.train, n.cores)
  }
  test_nodes <- findTestPreds(forest, X.test, n.cores)

  # check what user wants to produce
  mspewhat <- "mspe" %in% what
  biaswhat <- "bias" %in% what
  intervalwhat <- "interval" %in% what
  pwhat <- "p.error" %in% what
  qwhat <- "q.error" %in% what

  # set columns to return
  col_names <- c("pred", "mspe", "bias")[c(TRUE, mspewhat, biaswhat)]

  # initialize overall output
  output <- NULL

  # if the user does not want intervals, p.error, or q.error
  if (!intervalwhat & !pwhat & !qwhat) {

    # produce whichever of mspe and bias the user wants
    if (mspewhat & !biaswhat) {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(mspe = mean(unlist(node_errs) ^ 2)),
                                                                              keyby = c("rowid_test", "pred")]
    } else if (!mspewhat & biaswhat) {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(bias = -mean(unlist(node_errs))),
                                                                              keyby = c("rowid_test", "pred")]
    } else {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(mspe = mean(unlist(node_errs) ^ 2),
                                                                                bias = -mean(unlist(node_errs))),
                                                                              keyby = c("rowid_test", "pred")]
    }

  # else, do the same as above but keep the full list of OOB cohabitant prediction errors
  } else {

    # produce whichever of mspe and bias the user wants
    if (mspewhat & !biaswhat) {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(mspe = mean(unlist(node_errs) ^ 2),
                                                                                all_errs = list(sort(unlist(node_errs)))),
                                                                              keyby = c("rowid_test", "pred")]
    } else if (!mspewhat & biaswhat) {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(bias = -mean(unlist(node_errs)),
                                                                                all_errs = list(sort(unlist(node_errs)))),
                                                                              keyby = c("rowid_test", "pred")]
    } else if (mspewhat & biaswhat) {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(mspe = mean(unlist(node_errs) ^ 2),
                                                                                bias = -mean(unlist(node_errs)),
                                                                                all_errs = list(sort(unlist(node_errs)))),
                                                                              keyby = c("rowid_test", "pred")]
    } else {
      # join the train and test edgelists by tree/node and compute relevant summary
      # statistics via chaining
      oob_error_stats <-
        train_nodes[test_nodes,
                         .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                              .(all_errs = list(sort(unlist(node_errs)))),
                                                                              keyby = c("rowid_test", "pred")]
    }
  }

  # if the user requests prediction intervals
  if (intervalwhat) {

    # check the alpha argument for issues
    checkAlpha(alpha)

    # format prediction interval output
    percentiles <- sort(c(alpha / 2, 1 - (alpha / 2)))
    interval_col_names <- paste0(rep(c("lower_", "upper_"), each = length(alpha)),
                                 c(alpha, rev(alpha)))
    col_names <- c(col_names, interval_col_names)

    # compute prediction intervals
    oob_error_stats[, (interval_col_names) := lapply(percentiles, FUN = function(p) {
      pred + purrr::map_dbl(all_errs, ~.x[ceiling(length(.x) * p)])})]
  }

  # extract summary statistics requested as data.frame
  output_df <- data.table::setDF(oob_error_stats[, ..col_names])

  # if the user requests p.error
  if (pwhat) {

    # remove summary statistics from oob_error_stats
    oob_error_stats <- oob_error_stats[, .(all_errs)]

    # define the estimated CDF function
    perror <- function(q, xs = 1:n.test) {

      # check xs argument for issues
      checkxs(xs, n.test)

      # format output
      p_col_names <- paste0("p_", q)

      # compute CDF
      oob_error_stats[xs, (p_col_names) := lapply(q, FUN = function(que) {
        purrr::map_dbl(all_errs, ~mean(.x <= que))})]

      # format and return output
      to.return <- data.table::setDF(oob_error_stats[xs, ..p_col_names])
      row.names(to.return) <- xs
      return(to.return)
      }

    # add to output
    output <- list(output_df, perror)
    names(output) <- c("estimates", "perror")
    }

  # if the user requests q.error
  if (qwhat) {

    # remove summary statistics from oob_error_stats
    oob_error_stats <- oob_error_stats[, .(all_errs)]

    # define the estimated quantile function
    qerror <- function(p, xs = 1:n.test) {

      # check p and xs arguments for issus
      checkps(p)
      checkxs(xs, n.test)

      # format output
      q_col_names <- paste0("q_", p)

      # compute quantiles
      oob_error_stats[xs, (q_col_names) := lapply(p, FUN = function(pe) {
        purrr::map_dbl(all_errs, ~.x[ceiling(length(.x) * pe)])})]

      # format and return output
      to.return <- data.table::setDF(oob_error_stats[xs, ..q_col_names])
      row.names(to.return) <- xs
      return(to.return)
    }

    # add quantile function to output
    if (pwhat) {
      output[["qerror"]] <- qerror
    } else {
      output <- list(output_df, qerror)
      names(output) <- c("estimates", "qerror")
    }
  }

  # add train_nodes if requested
  if (return_train_nodes) {
    if (pwhat | qwhat) {
      output[["train_nodes"]] <- train_nodes
    } else {
      output <- list(output_df, train_nodes)
      names(output) <- c("estimates", "train_nodes")
    }
  }

  # return output
  if (is.null(output)) {
    return(output_df)
  } else {
    return(output)
  }
}
