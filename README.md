# forestError: A Unified Framework for Random Forest Prediction Error Estimation

### Overview
The `forestError` package estimates conditional mean squared prediction errors, conditional biases, conditional prediction intervals, and conditional error distributions for random forest predictions using the plug-in method introduced in Lu and Hardin (2019+). These estimates are conditional on the test observations' predictor values, accounting for possible response heterogeneity, random forest prediction bias, and random forest prediction variability across the predictor space.

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
* B. Lu and J. Hardin. A unified framework for random forest prediction error estimation. arXiv:1912.07435, 2019+. [[arXiv](https://arxiv.org/abs/1912.07435)]
