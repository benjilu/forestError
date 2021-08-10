#' Compute and locate test predictions
#'
#' Predicts each test observation's response using the random forest and, for
#' each test observation and tree, finds the terminal node of the tree in which
#' the test observation falls.
#'
#' This function accepts regression random forests built using the \code{randomForest},
#' \code{ranger}, \code{randomForestSRC}, and \code{quantregForest} packages.
#'
#' @param forest The random forest object being used for prediction.
#' @param X.test A \code{matrix} or \code{data.frame} with the observations to
#'   be predicted. Each row should be an observation, and each column should be
#'   a predictor variable.
#' @param n.cores Number of cores to use (for parallel computation in \code{ranger}).
#'
#' @return A \code{data.table} with the following four columns:
#'
#'   \item{rowid_test}{The row ID of the test observation as provided by \code{X.test}}
#'   \item{pred}{The random forest prediction of the test observation}
#'   \item{tree}{The ID of the tree of the random forest}
#'   \item{terminal_node}{The ID of the terminal node of the tree in which the
#'   test observation falls}
#'
#' @seealso \code{\link{findOOBErrors}}, \code{\link{quantForestError}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#'
#' @importFrom stats predict
#' @import data.table
#' @keywords internal
findTestPreds <- function(forest, X.test, n.cores = 1) {

  # determine whether this is a regression or classification random forest
  categorical <- grepl("class", c(forest$type, forest$family, forest$treetype), TRUE)

  # if the forest is from the quantregForest package
  if ("quantregForest" %in% class(forest)) {

    # convert to random forest class
    class(forest) <- "randomForest"

    # get test predictions
    test.preds <- predict(forest, X.test, nodes = TRUE)

    # get terminal nodes of test observations
    test.terminal.nodes <- attr(test.preds, "nodes")

    # format test observation predictions
    attr(test.preds, "nodes") <- NULL

    # else if the forest is from the randomForest package
  } else if ("randomForest" %in% class(forest)) {

    # get test predictions
    test.preds <- predict(forest, X.test, nodes = TRUE)

    # get terminal nodes of test observations
    test.terminal.nodes <- attr(test.preds, "nodes")

    # format test observation predictions
    attr(test.preds, "nodes") <- NULL

    # else, if the forest is from the ranger package
  } else if ("ranger" %in% class(forest)) {

    # get terminal nodes of test observations
    test.terminal.nodes <- predict(forest, X.test, num.threads = n.cores, type = "terminalNodes")$predictions

    # get test observation predictions
    test.preds <- predict(forest, X.test)$predictions

    # else, if the forest is from the randomForestSRC package
  } else if ("rfsrc" %in% class(forest)) {

    # get test predictions
    test.pred.list <- predict(forest, X.test, membership = TRUE)

    # get terminal nodes of test observations
    test.terminal.nodes <- test.pred.list$membership

    # format test observation predictions
    if (categorical) {
      test.preds <- test.pred.list$class
    } else {
      test.preds <- test.pred.list$predicted
    }
  }

  # reshape test.terminal.nodes to be a long data.table and
  # add unique IDs and predicted values
  test_nodes <- data.table::melt(
    data.table::as.data.table(test.terminal.nodes)[, `:=`(rowid_test = .I, pred = test.preds)],
    id.vars = c("rowid_test", "pred"),
    measure.vars = 1:ncol(test.terminal.nodes),
    variable.name = "tree",
    variable.factor = FALSE,
    value.name = "terminal_node")

  # set key columns for faster indexing
  data.table::setkey(test_nodes, tree, terminal_node)

  # return data.table
  return(test_nodes)
}
