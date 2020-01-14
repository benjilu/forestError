# forestError: A Unified Framework for Random Forest Prediction Error Estimation
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

### Overview
Update 1/14/2020: We are now on CRAN!

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
See `documentation.pdf` for detailed information on how to use this package. A portion of the example given in the documentation is reproduced below for convenience.

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

# do the same as above but this time in parallel
output <- quantForestError(rf, Xtrain, Xtest, alpha = 0.05,
                           n.cores = 2)
```

### License
See `DESCRIPTION` for information.

### Authors
Benjamin Lu and Johanna Hardin

### References
* B. Lu and J. Hardin. A unified framework for random forest prediction error estimation. arXiv:1912.07435, 2019+. [[arXiv](https://arxiv.org/abs/1912.07435)]
