## Version 1.1.0 Update

Version 1.1.0 makes two changes. First, it enables estimation of the conditional misclassification rate of predictions by classification random forests as proposed by Lu and Hardin (2021). Second, it compartmentalizes a costly step in the `quantForestError` algorithm: The identification of each training observation's out-of-bag terminal nodes.

### Conditional Misclassification Rate Estimation

The conditional misclassification rate of predictions by classification random forests can now be estimated. To estimate it, simply set the `what` argument in the `quantForestError` function to `"mcr"`. `what` will default to this if the provided `forest` is a classification random forest. See the example code in the README for a toy demonstration of the performance of this estimator.

### Compartmentalization

The identification of each training observation's out-of-bag terminal nodes is now compartmentalized from the main `quantForestError` function. By isolating this step from the main `quantForestError` function, Version 1.1.0 allows users to more efficiently iterate the algorithm. Users may wish to feed `quantForestError` batches of test observations iteratively if they have streaming data or a large test set that cannot be processed in one go due to memory constraints. In previous versions of this package, doing so would require the algorithm to recompute each training observation's out-of-bag terminal nodes in each iteration. This was redundant and costly. By separating this computation from the rest of the `quantForestError` algorithm, Version 1.1.0 allows the user to perform this computation only once.

As part of this modularization, the `quantForestError` function now has two additional arguments. If set to `TRUE`, `return_train_nodes` will return a `data.table` identifying each training observation's out-of-bag terminal nodes. This `data.table` can then be fed back into `quantForestError` via the argument `train_nodes` to avoid the redundant recomputation.

Version 1.1.0 also exports the function that produces the `data.table` identifying each training observation's out-of-bag terminal nodes. It is called `findOOBErrors`. Assuming the same inputs, `findOOBErrors` will produce the same output that is returned by setting `return_train_nodes` to `TRUE` in the `quantForestError` function.

See the documentation on `quantForestError` and `findOOBErrors` for examples.

Neither of these changes affects code that relied on Version 1.0.0 of this package, as the changes consist solely of a newly exported function, two optional arguments to `quantForestError` that by default do nothing new, and a new possible input for the `what` argument.

## Version 1.0.0 Update

This package has been updated to reflect the conventional sign of bias (mean prediction minus mean response). Previous versions of the package returned negative bias (mean response minus mean prediction). The sign of any algebraic operations involving the bias outputted by this package must therefore be reversed to preserve their intended effect.

In the future, we hope to implement a stochastic version of the `quantForestError` function, in which the parameters are estimated by random subsets of the training sample and/or the trees of the random forest.

## Version 0.2.0 Update

Thanks to John Sheffield ([Github Profile](https://github.com/sheffe)) for his helpful improvements to the computational performance of this package. (See the [Issue Tracker](https://github.com/benjilu/forestError/issues/2) for details.) These changes, which substantially reduce the runtime and memory load of this package's `quantForestError`, `perror`, and `qerror` functions, have been implemented in Version 0.2.0.

Version 0.2.0 also now allows the user to generate conditional prediction intervals with different type-I error rates in a single call of the `quantForestError` function.
