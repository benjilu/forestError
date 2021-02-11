### Version 1.0.0 Update

This package has been updated to reflect the conventional sign of bias (mean prediction minus mean response). Previous versions of the package returned negative bias (mean response minus mean prediction). The sign of any algebraic operations involving the bias outputted by this package must therefore be reversed to preserve their intended effect.

In the future, we hope to implement a stochastic version of the `quantForestError` function, in which the parameters are estimated by random subsets of the training sample and/or the trees of the random forest.

### Version 0.2.0 Update

Thanks to John Sheffield ([Github Profile](https://github.com/sheffe)) for his helpful improvements to the computational performance of this package. (See the [Issue Tracker](https://github.com/benjilu/forestError/issues/2) for details.) These changes, which substantially reduce the runtime and memory load of this package's `quantForestError`, `perror`, and `qerror` functions, have been implemented in Version 0.2.0, available for installation through this repository and pending approval at CRAN.

Version 0.2.0 also now allows the user to generate conditional prediction intervals with different type-I error rates in a single call of the `quantForestError` function.
