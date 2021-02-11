# forestError: A Unified Framework for Random Forest Prediction Error Estimation
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

### Version 1.0.0 Update

This package has been updated to reflect the conventional sign of bias (mean prediction minus mean response). Previous versions of the package returned negative bias (mean response minus mean prediction). The sign of any algebraic operations involving the bias outputted by this package must therefore be reversed to preserve their intended effect.

### Overview

The `forestError` package estimates conditional mean squared prediction errors, conditional biases, conditional prediction intervals, and conditional error distributions for random forest predictions using the plug-in method introduced in Lu and Hardin (2021). These estimates are conditional on the test observations' predictor values, accounting for possible response heterogeneity, random forest prediction bias, and random forest prediction variability across the predictor space.

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
* Benjamin Lu and Johanna Hardin. A Unified Framework for Random Forest Prediction Error Estimation. Journal of Machine Learning Research, 22(8):1-41, 2021. [[Link](https://jmlr.org/papers/v22/18-558.html)]
