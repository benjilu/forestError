checkForest <- function(forest) {
  if (typeof(forest) != "list") {
    stop("'forest' is not of the correct type")
  } else if (class(forest)[1] != "randomForest" & class(forest)[1] != "ranger" & !("rfsrc" %in% class(forest))) {
    stop("'forest' is not of the correct class")
  } else if (is.null(forest$inbag)) {
    stop("'forest' does not have record of which training observations are in bag for each tree. Re-fit the random forest with argument keep.inbag = TRUE")
  }
}

checkXtrainXtest <- function(X.train, X.test) {
  if (length(dim(X.train)) != 2) {
    stop("'X.train' must be a matrix or data.frame of dimension 2")
  } else if (length(dim(X.test)) != 2) {
    stop("'X.test' must be a matrix or data.frame of dimension 2")
  } else if (ncol(X.train) != ncol(X.test)) {
    stop("'X.train' and 'X.test' must have the same predictor variables")
  }
}

checkAlpha <- function(alpha) {
  if (typeof(alpha) != "double") {
    stop("'alpha' must be of type double")
  } else if (alpha <= 0 | alpha >= 1) {
    stop("'alpha' must be in (0, 1)")
  }
}

checkConservative <- function(conservative) {
  if (typeof(conservative) != "logical" | is.na(conservative)) {
    stop("'conservative' must be TRUE or FALSE")
  }
}
