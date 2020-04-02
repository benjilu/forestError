## Submission
This is an updated version of an existing package to improve performance.

## Changes from previous submission
Reduced runtime and memory usage by creating edgelists with the data.table
rather than computing the entire matrix of weights with Rcpp.

## Test environments
* local macOS R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies.
