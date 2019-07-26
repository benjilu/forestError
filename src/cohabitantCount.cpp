#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix countOOBCohabitants(IntegerMatrix trainNodes,
                                  IntegerMatrix testNodes,
                                  double nTrain,
                                  double nTest) {

  // get number of trees
  int nTree = testNodes.ncol();

  // initialize matrix output
  IntegerMatrix output(nTest, nTrain);

  // for each test observation
  for (int i = 0; i < nTest; i++) {

    // for each train observation
    for (int j = 0; j < nTrain; j++) {

      // initialize cohabitance counter
      int counter = 0;

      // for each tree
      for (int k = 0; k < nTree; k++) {

        // if the training and test observation are cohabitants in that tree
        if (trainNodes(j, k) == testNodes(i, k)) {

          // increment the counter by one
          counter += 1;
        }
      }

      // record the counter in the output matrix
      output(i, j) = counter;
    }
  }

  return output;
}
