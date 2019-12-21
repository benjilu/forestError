#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector countOOBCohabitantsTestPar(IntegerMatrix trainNodes,
                                         IntegerVector testNodes,
                                         double nTrain) {
  /* for each train-test pair, counts the frequency of cohabitation
  *
  * Args:
  *   trainNodes: matrix indicating for each tree the terminal node in which
  *               the training observation is located
  *   testNodes: vector indicating for each tree the terminal node in which
  *               the test observation is located
  *   nTrain: number of training observations
  *
  * Returns:
  *   vector indicating for each train-test pair the frequency of cohabitation
  */

  // get number of trees
  int nTree = trainNodes.ncol();

  // initialize vector output
  IntegerVector output(nTrain);

  // for each train observation
  for (int j = 0; j < nTrain; j++) {

    // initialize cohabitance counter
    int counter = 0;

    // for each tree
    for (int k = 0; k < nTree; k++) {

      // if the training and test observation are cohabitants in that tree
      if (trainNodes(j, k) == testNodes(k)) {

        // increment the counter by one
        counter += 1;
      }
    }

    // record the counter in the output vector
    output(j) = counter;
  }

  // return vector
  return output;
}
