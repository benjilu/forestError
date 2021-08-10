#' Estimate prediction error distribution parameters
#'
#' Estimates the prediction error distribution parameters requested in the input
#' to \code{quantForestError}.
#'
#' This function is for internal use.
#'
#' @param train_nodes A \code{data.table} indicating which out-of-bag prediction
#'   errors are in each terminal node of each tree in the random forest. It
#'   must be formatted like the output of the \code{findOOBErrors} function.
#' @param test_nodes A \code{data.table} indicating which test observations are
#'   in each terminal node of each tree in the random forest. It must be
#'   formatted like the output of the \code{findTestPreds} function.
#' @param mspewhat A boolean indicating whether to estimate conditional MSPE.
#' @param biaswhat A boolean indicating whether to estimate conditional bias.
#' @param intervalwhat A boolean indicating whether to estimate conditional
#'   prediction intervals.
#' @param pwhat A boolean indicating whether to estimate the conditional
#'   prediction error CDFs.
#' @param qwhat A boolean indicating whether to estimate the conditional
#'   prediction error quantile functions.
#' @param mcrwhat A boolean indicating whether to estimate the conditional
#'   misclassification rate.
#' @param alpha A vector of type-I error rates desired for the conditional prediction
#'   intervals; required if \code{intervalwhat} is \code{TRUE}.
#' @param n.test The number of test observations.
#'
#' @return A \code{data.frame} with one or more of the following columns:
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
#'   In addition, one or both of the following functions:
#'
#'   \item{perror}{The estimated cumulative distribution functions of the
#'   conditional error distributions associated with the test predictions}
#'   \item{qerror}{The estimated quantile functions of the conditional error
#'   distributions associated with the test predictions}
#'
#' @seealso \code{\link{quantForestError}}, \code{\link{findOOBErrors}}, \code{\link{findTestPreds}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#'
#' @import data.table
#' @importFrom purrr map_dbl
#' @keywords internal
estimateErrorParams <- function(train_nodes, test_nodes, mspewhat, biaswhat, intervalwhat, pwhat, qwhat, mcrwhat, alpha, n.test) {

  # set columns to return
  col_names <- c("pred", "mspe", "bias", "mcr")[c(TRUE, mspewhat, biaswhat, mcrwhat)]

  # initialize overall output
  output <- NULL

  # if the user wants misclassification rate
  if (mcrwhat) {
    # join the train and test edgelists by tree/node and compute relevant summary
    # statistics via chaining
    oob_error_stats <-
      train_nodes[test_nodes,
                  .(tree, terminal_node, rowid_test, pred, node_errs)][,
                                                                       .(mcr = mean(unlist(node_errs))),
                                                                       keyby = c("rowid_test", "pred")]
  # else if the user does not want intervals, p.error, or q.error
  } else if (!intervalwhat & !pwhat & !qwhat) {

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

  # return output
  if (is.null(output)) {
    return(output_df)
  } else {
    return(output)
  }
}
