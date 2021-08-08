#' Compute and locate out-of-bag prediction errors
#'
#' Computes each training observation's out-of-bag prediction error using the
#' random forest and, for each training observation and tree, finds the terminal
#' node of the tree in which the training observation falls.
#'
#' This function accepts regression random forests built using the \code{randomForest},
#' \code{ranger}, \code{randomForestSRC}, and \code{quantregForest} packages.
#' When training the random forest using \code{randomForest}, \code{ranger}, or
#' \code{quantregForest}, \code{keep.inbag} must be set to \code{TRUE}. When
#' training the random forest using \code{randomForestSRC}, \code{membership}
#' must be set to \code{TRUE}.
#'
#' @param forest The random forest object being used for prediction.
#' @param X.train A \code{matrix} or \code{data.frame} with the observations
#'   that were used to train \code{forest}; each row should be an observation,
#'   and each column should be a predictor variable.
#' @param Y.train A vector of the responses of the observations that were used
#'   to train \code{forest}. Required if \code{forest} was created using
#'   \code{ranger}, but not if \code{forest} was created using \code{randomForest},
#'   \code{randomForestSRC}, or \code{quantregForest}.
#' @param n.cores Number of cores to use (for parallel computation in \code{ranger}).
#'
#' @return A \code{data.table} with the following three columns:
#'
#'   \item{tree}{The tree of the random forest}
#'   \item{terminal_node}{The terminal node of the tree}
#'   \item{node_errs}{A vector of the out-of-bag prediction errors that fall
#'   within the terminal node of the tree}
#'
#' @seealso \code{\link{quantForestError}}
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
#'
#' # fit random forest to the training data
#' rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 5,
#'                                  ntree = 500,
#'                                  keep.inbag = TRUE)
#'
#' # compute out-of-bag prediction errors and locate each
#' # training observation in the trees for which it is out
#' # of bag
#' train_nodes <- findOOBErrors(rf, Xtrain)
#'
#' # estimate conditional mean squared prediction errors,
#' # biases, prediction intervals, and error distribution
#' # functions for the test observations. provide
#' # train_nodes to avoid recomputing that step.
#' output <- quantForestError(rf, Xtrain, Xtest,
#'                            train_nodes = train_nodes)
#'
#' @importFrom stats predict
#' @import data.table
#' @export
findOOBErrors <- function(forest, X.train, Y.train = NULL, n.cores = 1) {

  # if the forest is from the quantregForest package
  if ("quantregForest" %in% class(forest)) {

    # convert to random forest class
    class(forest) <- "randomForest"

    # get terminal nodes of training observations
    train.terminal.nodes <- attr(predict(forest, X.train, nodes = TRUE), "nodes")

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$y - forest$predicted

    # else if the forest is from the randomForest package
  } else if ("randomForest" %in% class(forest)) {

    # get terminal nodes of training observations
    train.terminal.nodes <- attr(predict(forest, X.train, nodes = TRUE), "nodes")

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$y - forest$predicted

    # else, if the forest is from the ranger package
  } else if ("ranger" %in% class(forest)) {

    # get terminal nodes of training observations
    train.terminal.nodes <- predict(forest, X.train, num.threads = n.cores, type = "terminalNodes")$predictions

    # get number of times each training observation appears in each tree
    bag.count <- matrix(unlist(forest$inbag.counts, use.names = FALSE), ncol = forest$num.trees, byrow = FALSE)

    # get the OOB prediction error of each training observation
    oob.errors <- Y.train - forest$predictions

    # else, if the forset is from the randomForestSRC package
  } else if ("rfsrc" %in% class(forest)) {

    # get terminal nodes of all observations
    train.terminal.nodes <- forest$membership

    # get number of times each training observation appears in each tree
    bag.count <- forest$inbag

    # get the OOB prediction error of each training observation
    oob.errors <- forest$yvar - forest$predicted.oob

  }

  # get the terminal nodes of the training observations in the trees in which
  # they are OOB (for all other trees, set the terminal node to be NA)
  train.terminal.nodes[bag.count != 0] <- NA

  # reshape train.terminal.nodes to be a long data.table and include OOB
  # prediction errors as a column
  long_train_nodes <- data.table::as.data.table(train.terminal.nodes)
  long_train_nodes[, `:=`(oob_error = oob.errors)]
  long_train_nodes <- data.table::melt(
    long_train_nodes,
    id.vars = c("oob_error"),
    measure.vars = 1:ncol(train.terminal.nodes),
    variable.name = "tree",
    value.name = "terminal_node",
    variable.factor = FALSE,
    na.rm = TRUE)

  # collapse the long data.table by unique tree/node
  long_train_nodes <- long_train_nodes[,
                                       .(node_errs = list(oob_error)),
                                       keyby = c("tree", "terminal_node")]

  # return long data.table of OOB error values and locations
  return(long_train_nodes)
}
