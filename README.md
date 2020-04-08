# forestError: A Unified Framework for Random Forest Prediction Error Estimation
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

### Version 0.2.0 Update

Thanks to John Sheffield ([Github Profile](https://github.com/sheffe)) for his extremely helpful improvements to the computational performance of this package. (See the [Issue Tracker](https://github.com/benjilu/forestError/issues/2) for details.) These changes, which substantially reduce the runtime and memory load of this package's `quantForestError`, `perror`, and `qerror` functions, have been implemented in Version 0.2.0, available for installation through both CRAN and this repository.

Version 0.2.0 also now allows the user to generate conditional prediction intervals with different type-I error rates in a single call of the `quantForestError` function.

In the future, we hope to implement a stochastic version of the `quantForestError` function, in which the parameters are estimated by random subsets of the training sample and/or the trees of the random forest.

### Overview

The `forestError` package estimates conditional mean squared prediction errors, conditional biases, conditional prediction intervals, and conditional error distributions for random forest predictions using the plug-in method introduced in Lu and Hardin (2019+). These estimates are conditional on the test observations' predictor values, accounting for possible response heterogeneity, random forest prediction bias, and random forest prediction variability across the predictor space.

In its current state, the main function in this package accepts regression random forests built using any of the following packages:

- `randomForest`,
- `randomForestSRC`,
- `ranger`, and
- `quantregForest`.

### Installation

Running the following line of code in `R` will install a stable version of this package from CRAN:

```{r}
install.packages("forestError")
```

To install the developer version of this package from Github, run the following lines of code in `R`:

```{r}
library(devtools)
devtools::install_github(repo = "benjilu/forestError")
```  

### Instructions
See the documentation for detailed information on how to use this package. A portion of the example given in the documentation is reproduced below for convenience.

```{r}
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
output <- quantForestError(rf, Xtrain, Xtest, alpha = 0.05)
```

### License
See `DESCRIPTION` for information.

### Authors
Benjamin Lu and Johanna Hardin

### References
* B. Lu and J. Hardin. A unified framework for random forest prediction error estimation. arXiv:1912.07435, 2019+. [[arXiv](https://arxiv.org/abs/1912.07435)]
