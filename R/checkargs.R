# check forest argument for problems
checkForest <- function(forest) {
  if (typeof(forest) != "list") {
    stop("'forest' is not of the correct type")
  } else if (!any(c("randomForest", "ranger", "rfsrc", "quantregForest") %in% class(forest))) {
    stop("'forest' is not of the correct class")
  } else if (is.null(forest$inbag)) {
    stop("'forest' does not have record of which training observations are in bag for each tree. Re-fit the random forest with argument keep.inbag = TRUE")
  }
}

# check training and test covariate arguments for problems
checkXtrainXtest <- function(X.train, X.test) {
  if (length(dim(X.train)) != 2) {
    stop("'X.train' must be a matrix or data.frame of dimension 2")
  } else if (length(dim(X.test)) != 2) {
    stop("'X.test' must be a matrix or data.frame of dimension 2")
  } else if (ncol(X.train) != ncol(X.test)) {
    stop("'X.train' and 'X.test' must have the same predictor variables")
  }
}

# check training response argument for problems
checkYtrain <- function(forest, Y.train, n.train) {
  if ("ranger" %in% class(forest)) {
    if (is.null(Y.train)) {
      stop("You must supply the training responses (Y.train)")
    } else if (length(Y.train) != n.train) {
      stop("Number of training responses does not match number of training observations")
    }
  }
}

# check type-I error rate argument for problems
checkAlpha <- function(alpha) {
  if (typeof(alpha) != "double") {
    stop("'alpha' must be of type double")
  } else if (any(alpha <= 0 | alpha >= 1)) {
    stop("'alpha' must be in (0, 1)")
  }
}

# check index argument in perror and qerror functions for problems
checkxs <- function(xs, n.test) {
  if (max(xs) > n.test | min(xs) < 1) {
    stop("Test indices are out of bounds")
  } else if (any(xs %% 1 != 0)) {
    stop("Test indices must be whole numbers")
  }
}

# check probability argument in qerror for problems
checkps <- function(p) {
  if (max(p) > 1 | min(p) < 0) {
    stop("Probabilities must be between 0 and 1")
  }
}

# check core argument for problems
checkcores <- function(n.cores) {
  if (is.null(n.cores)) {
    stop("Number of cores must be specified")
  } else if (n.cores < 1) {
    stop("Number of cores must be at least 1")
  } else if (n.cores %% 1 != 0) {
    stop("Number of cores must be integer")
  }
}

# check requested parameters
checkwhat <- function(what, forest) {
  if (is.null(what)) {
    stop("Please specify the parameters to be estimated")
  } else if ("mcr" %in% what & length(what) > 2) {
    stop("Misclassification rate cannot be estimated for real-valued responses")
  } else if ("mcr" %in% what) {
    if ("quantregForest" %in% class(forest)) {
      if (forest$type != "classification") {
        stop("Misclassification rate can be estimated only for classification random forests")
      }
    } else if ("randomForest" %in% class(forest)) {
      if (forest$type != "classification") {
        stop("Misclassification rate can be estimated only for classification random forests")
      }
    } else if ("rfsrc" %in% class(forest)) {
      if (forest$family != "class") {
        stop("Misclassification rate can be estimated only for classification random forests")
      }
    } else if ("ranger" %in% class(forest)) {
      if (forest$treetype != "Classification") {
        stop("Misclassification rate can be estimated only for classification random forests")
      }
    }
  } else if (any(c("mspe", "bias", "interval", "p.error", "q.error") %in% what)) {
    if ("quantregForest" %in% class(forest)) {
      if (forest$type == "classification") {
        stop("Requested parameters cannot be estimated for classification random forests")
      }
    } else if ("randomForest" %in% class(forest)) {
      if (forest$type == "classification") {
        stop("Requested parameters cannot be estimated for classification random forests")
      }
    } else if ("rfsrc" %in% class(forest)) {
      if (forest$family == "class") {
        stop("Requested parameters cannot be estimated for classification random forests")
      }
    } else if ("ranger" %in% class(forest)) {
      if (forest$treetype == "Classification") {
        stop("Requested parameters cannot be estimated for classification random forests")
      }
    }
  }
}
