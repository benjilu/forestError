# forestError: A Unified Framework for Random Forest Prediction Error Inference

### Overview
The `forestError` package estimates conditional mean squared prediction errors, conditional biases, conditional prediction intervals, and conditional error distributions for random forest predictions using methods introduced in Lu and Hardin (2019+) (in preparation). Because these estimates are conditional on the test observations' predictor values, each estimate is individualized. In other words, each squared error, bias, interval, and error distribution estimate is specific to each test observation, accounting for possible response heterogeneity, random forest prediction bias, and/or random forest prediction variability across the predictor space.

In its current state, the main function in this package accepts regression random forests built using any of the following packages:

- `randomForest`,
- `randomForestSRC`,
- `ranger`, and
- `quantregForest`.

### Installation

Running the following lines of code in `R` will install this package from Github:

```{r}
library(devtools)
devtools::install_github(repo = "benjilu/forestError")
```  

### Instructions
See `documentation.pdf` for information on how to use this package. A portion of the example given in the documentation is reproduced below for convenience.

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
rf <- randomForest(Xtrain, Ytrain, nodesize = 5,
                   ntree = 500, keep.inbag = TRUE)

# get conditional mean squared prediction errors, conditional biases,
# conditional prediction intervals, and conditional empirical error
# distribution functions for the test observations
test.errors <- quantForestError(rf, Xtrain, Xtest, alpha = 0.05)
```

### License
See `DESCRIPTION` for information.

### Authors
Benjamin Lu and Johanna Hardin

### References
* B. Lu and J. Hardin. A unified framework for random forest prediction error inference. In preparation, 2019+.
* J. Lei, M. G’Sell, A. Rinaldo, R.J. Tibshirani, and L. Wasserman. Distribution-free predictive inference for regression. Journal of the American Statistical Association, 113:1094-1111, 2018.
* N. Meinshausen. Quantile regression forests. Journal of Machine Learning Research, 7:983–999, 2006.
