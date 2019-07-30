#' Empirical cumulative distribution functions of conditional errors
#'
#' Returns probabilities from the estimated cumulative distribution function
#' of the conditional error distribution associated with each test prediction.
#'
#' @usage perror(p, xs)
#'
#' @param p A vector of quantiles.
#' @param xs A vector of the indices of the test observations for which the
#'   conditional error CDFs are desired. Defaults to all test observations
#'   given in the call of \code{quantForestError}.
#'
#' @return If either \code{p} or \code{xs} has length one, then a vector is
#'   returned with the desired probabilities. If both have length greater than
#'   one, then a \code{data.frame} of probabilities is returned, with rows
#'   corresponding to the inputted \code{xs} and columns corresponding to the
#'   inputted \code{p}.
#'
#' @seealso \code{\link{quantForestError}}
#'
#' @author Benjamin Lu \code{<b.lu@berkeley.edu>}; Johanna Hardin \code{<jo.hardin@pomona.edu>}
#'
#' @examples
#' \donttest{
#' # get the probability that the error associated with each test
#' # prediction is less than -4 and the probability that the error
#' # associated with each test prediction is less than 7
#' perror(c(-4, 7))
#'
#' # same as above but only for the first three test observations
#' perror(c(-4, 7), 1:3)
#' }
perror <- function() {}
