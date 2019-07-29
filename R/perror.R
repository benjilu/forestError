#' Empirical cumulative distribution functions of conditional errors
#'
#' Returns probabilities from the estimated cumulative distribution function
#' of the conditional error distribution associated with each test prediction.
#'
#' @usage perror(p, xs)
#'
#' @param p Vector of quantiles
#' @param xs Indices of the test observations for which the conditional error
#'   CDFs are desired. Defaults to all test observations given in the call of
#'   \code{quantForestError}.
#'
#' @return If either \code{p} or \code{xs} has length one, then a vector is
#'   returned with the desired probabilities. If both have length greater than
#'   one, then a \code{data.frame} is returned, with rows corresponding to the
#'   inputted \code{xs} and columns corresponding to the inputted \code{p}.
#'
#' @seealso \code{\link{quantForestError}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#'
#' @examples
#' \donttest{
#' # get the probability that the error associated with each test prediction
#' # is less than -5 and the probability that the error associated with each
#' # test prediction is less than 5
#' perror(c(-5, 5))
#'
#' # same as above but only for the first three test observations
#' perror(c(-5, 5), 1:3)
#' }
perror <- function(p, xs = 1:n.test) {

  # check xs argument for issus
  checkxs(xs, n.test)

  # define CDF evaluated for a single quantile
  singleperror <- function(singlep, exes) {

    # get the index of the maximum error that is less than or equal to the stated quantile
    max.error.ind <- suppressWarnings(max(which(ordered.oob.errors$x <= singlep)))

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
  if (length(p) > 1) {
    to.return <- data.frame(sapply(p, FUN = function(x) singleperror(x, xs)))
    row.names(to.return) <- xs
    names(to.return) <- p
    return(to.return)
    # else, evaluate singleperror on the single quantile provided
  } else {
    return(singleperror(p, xs))
  }
}
