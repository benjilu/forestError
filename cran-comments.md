## Submission
This is a resubmission. Submission last reviewed by Jelena Saf on January 8, 2020.

## Changes from previous submission
Thanks for the feedback. Change log is as follows.

The title is already exactly 65 characters long, including spaces, so I did not change it. Is this OK?

I added the arXiv identifier for the reference in the DESCRIPTION file.

The perror and qerror functions are empty because they are defined as outputs of the main function, quantForestError, but we would like to provide the user with documentation for these outputs. In particular, the perror and qerror functions are defined by the list ordered.oob.errors, which is computed from the inputs the user provides to the quantForestError function but is not an argument that the user should be able to modify or supply. I clarified this by adding stop calls to the perror and qerror functions. Is this OK?

I modified all examples so that they are executable and not wrapped in \donttest{}. To do so, I added the randomForest package to the Suggests field of the DESCRIPTION file.

I modified the examples for the unexported functions perror and qerror so that they are executable.

## Test environments
* local macOS R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies.