# forestError: A Unified Framework for Random Forest Prediction Error Estimation
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Version 1.1.0 Update

Version 1.1.0 makes two changes. First, it enables estimation of the conditional misclassification rate of predictions by classification random forests as proposed by Lu and Hardin (2021). Second, it compartmentalizes a costly step in the `quantForestError` algorithm: The identification of each training observation's out-of-bag terminal nodes.

### Conditional Misclassification Rate Estimation

The conditional misclassification rate of predictions by classification random forests can now be estimated. To estimate it, simply set the `what` argument in the `quantForestError` function to `"mcr"`. `what` will default to this if the provided `forest` is a classification random forest. See the example code below for a toy demonstration of the performance of this estimator.

### Compartmentalization

The identification of each training observation's out-of-bag terminal nodes is now compartmentalized from the main `quantForestError` function. By isolating this step from the main `quantForestError` function, Version 1.1.0 allows users to more efficiently iterate the algorithm. Users may wish to feed `quantForestError` batches of test observations iteratively if they have streaming data or a large test set that cannot be processed in one go due to memory constraints. In previous versions of this package, doing so would require the algorithm to recompute each training observation's out-of-bag terminal nodes in each iteration. This was redundant and costly. By separating this computation from the rest of the `quantForestError` algorithm, Version 1.1.0 allows the user to perform this computation only once.

As part of this modularization, the `quantForestError` function now has two additional arguments. If set to `TRUE`, `return_train_nodes` will return a `data.table` identifying each training observation's out-of-bag terminal nodes. This `data.table` can then be fed back into `quantForestError` via the argument `train_nodes` to avoid the redundant recomputation.

Version 1.1.0 also exports the function that produces the `data.table` identifying each training observation's out-of-bag terminal nodes. It is called `findOOBErrors`. Assuming the same inputs, `findOOBErrors` will produce the same output that is returned by setting `return_train_nodes` to `TRUE` in the `quantForestError` function.

See the documentation on `quantForestError` and `findOOBErrors` for examples.

Neither of these changes affects code that relied on Version 1.0.0 of this package, as the changes consist solely of a newly exported function, two optional arguments to `quantForestError` that by default do nothing new, and a new possible input for the `what` argument.

## Overview

The `forestError` package estimates conditional misclassification rates, conditional mean squared prediction errors, conditional biases, conditional prediction intervals, and conditional error distributions for random forest predictions using the plug-in method introduced in Lu and Hardin (2021). These estimates are conditional on the test observations' predictor values, accounting for possible response heterogeneity, random forest prediction bias, and random forest prediction variability across the predictor space.

In its current state, the main function in this package accepts regression random forests built using any of the following packages:

- `randomForest`,
- `randomForestSRC`,
- `ranger`, and
- `quantregForest`.

## Installation

Running the following line of code in `R` will install a stable version of this package from CRAN:

```{r}
install.packages("forestError")
```

To install the developer version of this package from Github, run the following lines of code in `R`:

```{r}
library(devtools)
devtools::install_github(repo = "benjilu/forestError")
```  

## Instructions
See the documentation for detailed information on how to use this package. A regression example and a classification example are given below.

```{r}
######################## REGRESSION ########################
# load data
data(airquality)

# remove observations with missing predictor variable values
airquality <- airquality[complete.cases(airquality), ]

# get number of observations and the response column index
n <- nrow(airquality)
response.col <- 1

# split data into training and test sets
train.ind <- sample(1:n, n * 0.9, replace = FALSE)
Xtrain <- airquality[train.ind, -response.col]
Ytrain <- airquality[train.ind, response.col]
Xtest <- airquality[-train.ind, -response.col]
Ytest <- airquality[-train.ind, response.col]

# fit random forest to the training data
rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 5,
                                 ntree = 500, keep.inbag = TRUE)

# estimate conditional mean squared prediction errors, conditional
# biases, conditional prediction intervals, and conditional error
# distribution functions for the test observations
output <- quantForestError(rf, Xtrain, Xtest)

######################## CLASSIFICATION ########################
# data-generating parameters
train_samp_size <- 10000
test_samp_size <- 5000
p <- 5

# generate binary data where the probability of success is a
# linear function of the first predictor variable
Xtrain <- data.frame(matrix(runif(train_samp_size * p),
                             ncol = p))
Xtest <- data.frame(matrix(runif(test_samp_size * p),
                            ncol = p))
Ytrain <- as.factor(rbinom(train_samp_size, 1, Xtrain$X1))

# fit random forest to training data
rf <- randomForest::randomForest(Xtrain, Ytrain, nodesize = 3,
                                 ntree = 1000, keep.inbag = TRUE)

# estimate conditional misclassification rate
output <- quantForestError(rf, Xtrain, Xtest)

# plot conditional misclassification rate against the signal
plot(Xtest$X1, output$mcr, xlab = "X1", ylab = "Estimated
     Misclassification Rate")
```

## License
See `DESCRIPTION` for information.

## Authors
Benjamin Lu and Johanna Hardin

## References
* Benjamin Lu and Johanna Hardin. A Unified Framework for Random Forest Prediction Error Estimation. Journal of Machine Learning Research, 22(8):1-41, 2021. [[Link](https://jmlr.org/papers/v22/18-558.html)]
