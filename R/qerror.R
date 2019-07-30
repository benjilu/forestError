#' Empirical quantile functions of conditional errors
#'
#' Returns quantiles from the estimated quantile function of the conditional
#' error distribution associated with each test prediction.
#'
#' @usage qerror(q, xs)
#'
#' @param q A vector of probabilities.
#' @param xs A vector of the indices of the test observations for which the
#'   conditional error quantiles are desired. Defaults to all test observations
#'   given in the call of \code{quantForestError}.
#'
#' @return If either \code{q} or \code{xs} has length one, then a vector is
#'   returned with the desired quantiles. If both have length greater than
#'   one, then a \code{data.frame} of quantiles is returned, with rows
#'   corresponding to the inputted \code{xs} and columns corresponding to the
#'   inputted \code{q}.
#'
#' @seealso \code{\link{quantForestError}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#' @examples
#' \donttest{
#' # get the 0.25 and 0.8 quantiles of the error distribution associated
#' # with each test observation
#' qerror(c(0.25, 0.8))
#'
#' # same as above but only for the first three test observations
#' qerror(c(0.25, 0.8), 1:3)
#' }
qerror <- function(q, xs = 1:n.test) {}
