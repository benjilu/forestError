checkForest <- function(forest) {
  if (typeof(forest) != "list") {
    stop("'forest' is not of the correct type")
  } else if (!any(c("randomForest", "ranger", "rfsrc", "quantregForest") %in% class(forest))) {
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

checkYtrain <- function(forest, Y.train, n.train) {
  if ("ranger" %in% class(forest)) {
    if (is.null(Y.train)) {
      stop("You must supply the training responses (Y.train)")
    } else if (length(Y.train) != n.train) {
      stop("Number of training responses does not match number of training observations")
    }
  } else if (!is.null(Y.train)) {
    if (Y.train == "train" | Y.train == "test") {
      stop("It looks like you passed your argument for par.over through Y.train. Make sure you explicitly label your arguments")
    }
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

checkxs <- function(xs, n.test) {
  if (max(xs) > n.test | min(xs) < 1) {
    stop("Test indices are out of bounds")
  } else if (any(xs %% 1 != 0)) {
    stop("Test indices must be whole numbers")
  }
}

checkqs <- function(q) {
  if (max(q) > 1 | min(q) < 0) {
    stop("Probabilities must be between 0 and 1")
  }
}

checkparcores <- function(n.cores, par.over) {
  if (is.null(n.cores)) {
    stop("Number of cores must be specified")
  } else if (n.cores < 1) {
    stop("Number of cores must be at least 1")
  } else if (n.cores %% 1 != 0) {
    stop("Number of cores must be integer")
  } else if (!is.null(n.cores) & is.null(par.over)) {
    if (n.cores > 1) {
      stop("You must specify whether to parallelize over the training set or the test set")
    }
  } else if (!is.null(n.cores) & !is.null(par.over)) {
     if (par.over != "train" & par.over != "test") {
      stop("par.over must be either train, test, or NULL")
    }
  }
}
