#' Empirical quantile functions of conditional errors
#'
#' Returns quantiles from the estimated quantile function of the conditional
#' error distribution associated with each test prediction.
#'
#' @param q Vector of probabilities
#' @param xs Indices of the test observations for which the conditional error
#'   CDFs are desired. Defaults to all test observations given in the call of
#'   \code{quantForestError}.
#'
#' @return If either \code{q} or \code{xs} has length one, then a vector is
#'   returned with the desired probabilities. If both have length greater than
#'   one, then a \code{data.frame} is returned, with rows corresponding to the
#'   inputted \code{xs} and columns corresponding to the inputted \code{q}.
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#' @examples
#' \donttest{
#' # get the 0.25 and 0.8 quantiles of the error distribution associated with
#' # each test observation
#' qerror(c(0.25, 0.8))
#'
#' # same as above but only for the first three test observations
#' qerror(c(0.25, 0.8), 1:3)
#' }
qerror <- function(q, xs = 1:n.test) {

  # check xs argument for issus
  checkxs(xs, n.test)

  # define quantile function evaluated for single probability
  singleqerror <- function(singleq, exes) {

    return(unname(ordered.oob.errors$x[apply(matrix(cumsums[exes, ], nrow = length(exes)), 1, FUN = function(x) min(which(x >= singleq)))]))
  }

  # if more than one probability is provided, vectorize the evaluation of
  # singleqerror over the probabilities
  if (length(q) > 1) {
    to.return <- data.frame(sapply(q, FUN = function(x) singleqerror(x, xs)))
    row.names(to.return) <- xs
    names(to.return) <- q
    return(to.return)
    # else, evaluate singleqerror on the single probability provided
  } else {
    return(singleqerror(q, xs))
  }
}
