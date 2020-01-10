#' Estimated conditional prediction error CDFs
#'
#' Returns probabilities from the estimated conditional cumulative distribution
#' function of the prediction error associated with each test observation.
#'
#' This function is only defined as output of the \code{quantForestError} function.
#' It is not exported as a standalone function. See the example.
#'
#' @usage perror(q, xs)
#'
#' @param q A vector of quantiles.
#' @param xs A vector of the indices of the test observations for which the
#'   conditional error CDFs are desired. Defaults to all test observations
#'   given in the call of \code{quantForestError}.
#'
#' @return If either \code{q} or \code{xs} has length one, then a vector is
#'   returned with the desired probabilities. If both have length greater than
#'   one, then a \code{data.frame} of probabilities is returned, with rows
#'   corresponding to the inputted \code{xs} and columns corresponding to the
#'   inputted \code{q}.
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
#' Ytest <- airquality[-train.ind, response.col]
#'
#' # fit random forest to the training data
#' rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 5,
#'                                  ntree = 500,
#'                                  keep.inbag = TRUE)
#'
#' # estimate conditional error distribution functions
#' output <- quantForestError(rf, Xtrain, Xtest,
#'                            what = c("p.error", "q.error"))
#'
#' # get the probability that the error associated with each test
#' # prediction is less than -4 and the probability that the error
#' # associated with each test prediction is less than 7
#' output$perror(c(-4, 7))
#'
#' # same as above but only for the first three test observations
#' output$perror(c(-4, 7), 1:3)
perror <- function(q, xs) {stop("perror is not exported as a standalone function. You must first run the quantForestError function to define perror. See documentation.")}
