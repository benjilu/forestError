#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector countOOBCohabitantsTrainPar(IntegerVector trainNodes,
                                          IntegerMatrix testNodes,
                                          double nTest) {
  /* for each train-test pair, counts the frequency of cohabitation
  *
  * Args:
  *   trainNodes: vector indicating for each tree the terminal node in which
  *               the training observation is located
  *   testNodes: matrix indicating for each tree the terminal node in which
  *               the test observation is located
  *   nTest: number of test observations
  *
  * Returns:
  *   vector indicating for each train-test pair the frequency of cohabitation
  */

  // get number of trees
  int nTree = testNodes.ncol();

  // initialize vector output
  IntegerVector output(nTest);

  // for each test observation
  for (int i = 0; i < nTest; i++) {

    // initialize cohabitance counter
    int counter = 0;

    // for each tree
    for (int k = 0; k < nTree; k++) {

      // if the training and test observation are cohabitants in that tree
      if (trainNodes(k) == testNodes(i, k)) {

        // increment the counter by one
        counter += 1;
      }
    }

    // record the counter in the output vector
    output(i) = counter;
  }

  // return vector
  return output;
}
