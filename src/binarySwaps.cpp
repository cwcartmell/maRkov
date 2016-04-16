#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <iostream>
#include <Rmath.h>
#include <math.h>
#include <vector>
#include <algorithm>
#include <typeinfo>

//Enable C++11
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp ;

IntegerVector swap (IntegerVector binChain, int i, int j) ;
int rUnifInt (IntegerVector binChain) ;
float u1TestStat (IntegerVector binChain, int nChainUniques) ;
float u6TestStat (IntegerMatrix binChains, int nChainUniques, int nMultChains, int lengthOfChains) ;
float chiSqTestStat (IntegerVector binChain, int nChainUniques) ;
bool indicateRun (IntegerVector binChain, int p, int i) ;
int runTestStat (IntegerVector binChain, int p) ;
IntegerMatrix metropolis (IntegerVector binChain, int m, int b) ;
NumericVector u1TestStatArray (IntegerMatrix binChains, int nChainUniques) ;
NumericVector runTestStatArray (IntegerMatrix binChains, int p) ;
int vecGreaterThan (NumericVector testStats) ;
std::vector<std::vector<std::vector<int> > > nCounts (IntegerVector binChain, int nChainUniques) ;
int iDimSum (std::vector<std::vector<std::vector<int> > > n, int j, int k) ;
int jDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int k) ;
int kDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int j) ;
int ikDimSum (std::vector<std::vector<std::vector<int> > > n, int j) ;
int jkDimSum (std::vector<std::vector<std::vector<int> > > n, int i) ;
std::vector<std::vector<std::vector<int> > > nCountsMultiple (IntegerMatrix binChains, int nChainUniques) ;
std::vector<std::vector<std::vector<int> > > u6Metropolis (IntegerMatrix binChains, int m, int b) ;
std::vector<std::vector<int> > swapMult (std::vector<std::vector<int> > binChains, int m) ;
NumericVector multipleRunTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int p) ;
bool multipleIndicateRun (std::vector<int> binChain, int p, int i) ;
float multipleRunTestStat (std::vector<std::vector<int> > binChains, int p) ;
NumericVector multipleChiSqTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques) ;
float multipleChiSqTestStat (std::vector<std::vector<int> > binChains, int nChainUniques) ;





//' Swap elements of single binary chains
//'
//' \code{swap} is used to swap elements of a single binary chain if doing so
//' maintains the same number of transitions between the two states of that
//' chain.
//'
//' \code{swap} takes a R integer vector, as well as two integers, i and j,
//' which are valid indicies of that vector. It then checks if swapping the
//' values at indicies i and j would change the number of transitions between
//' states in the vector, and if the swap does not, it switches the two values,
//' returning a new vector with the values switched. If it cannot switch the
//' values, it returns the original vector.
//'
//' @param binChain A binary one dimensional integer vector.
//' @param i An integer which is a valid indice of the vector binChain.
//' @param j An integer which is a valid indice of the vector binChain.
// [[Rcpp::export]]
IntegerVector swap (IntegerVector binChain, int i, int j) {
  int minimum = std::min(i, j) ;
  if ((abs(i - j) > 1 and binChain[i - 1] + binChain[i + 1] == binChain[j - 1] + binChain[j + 1]))  {
    int xi = binChain[i] ;
    binChain[i] = binChain[j] ;
    binChain[j] =  xi ;
  }
  else if ((abs(i - j) == 1 and binChain[minimum - 1] == binChain[minimum + 2])) {
    int xi = binChain[i] ;
    binChain[i] = binChain[j] ;
    binChain[j] =  xi ;
  }

  return binChain ;
}

//' Swap elements of multiple binary chains
//'
//' \code{swapMult} is used to swap elements of multiple binary chains if doing
//' so maintains the same number of transitions between the two states of those
//' chains.
//'
//' \code{swapMult} works by taking a two dimensional integer vector
//' \code{binChains} and \code{m}, a number of times to attempt swaps. It
//' generates random integers which are valid indicies of the two dimensional
//' vector \code{binChains} and tries to swap the elements of the vector at
//' the indicies that it generates, only doing so if this preserves the total
//' number of transitions between states. After attempting \code{m} swaps,
//' \code{swapMult} returns the new, freshly swapped two dimensional vector of
//' binary chains.
//'
//' @param binChains A two dimensional integer vector with binary values.
//' @param m A positive nonzero integer value for the attempted number of swaps
//' to attempt on \code{binChains}.
// [[Rcpp::export]]
std::vector<std::vector<int> > swapMult (std::vector<std::vector<int> > binChains, int m) {
  int nCol = binChains[0].size() ;
  int nRow = binChains.size() ;
  for (int i = 0 ; i < m ; i++) {
    int caseVar = ceil(unif_rand() * 3) ;
    int a = ceil(unif_rand() * (nCol - 2)) ;
    int b = ceil(unif_rand() * (nCol - 2)) ;
    int c = floor(unif_rand() * nRow) ;
    int d = floor(unif_rand() * nRow) ;
    int maxi = std::max(a, b) ;
    int mini = std::min(a, b) ;
    if (caseVar == 1) {
      if (abs(a - b) > 1 and binChains[c][a - 1] + binChains[c][a + 1] == binChains[c][b - 1] + binChains[c][b + 1]) {
        int xac = binChains[c][a] ;
        binChains[c][a] = binChains[c][b] ;
        binChains[c][b] = xac ;
      }
      else if (abs(a - b) == 1 and binChains[c][mini - 1] == binChains[c][maxi + 1]) {
        int xac = binChains[c][a] ;
        binChains[c][a] = binChains[c][b] ;
        binChains[c][b] = xac ;
      }
    }
    else if (caseVar == 2) {
      if (binChains[c][a - 1] + binChains[c][a + 1] == binChains[d][b - 1] + binChains[d][b + 1]) {
        int xac = binChains[c][a] ;
        binChains[c][a] = binChains[d][b] ;
        binChains[d][b] = xac ;
      }
    }
    else if (caseVar == 3) {
      if (binChains[c][nCol - 1] == binChains[d][nCol - 1]) {
        int xnColc = binChains[c][nCol] ;
        binChains[c][nCol] = binChains[d][nCol] ;
        binChains[d][nCol] = xnColc ;
      }
    }
  }
  return binChains ;
}

//' Generate a random indice of a vector
//'
//' \code{rUnifInt} takes a one dimensional vector and randomly generates a
//' valid index of that vector from the discrete uniform distribution. It does
//' not return either the first or last indice.
//'
//' \code{rUnifInt} works by taking the length of a supplied R integer vector,
//' subtracting two from it, and multiplying it by a random number from the
//' uniform distribution on the interval (0, 1). it then rounds this number up
//' to the nearest integer and returns it.
//'
//' @param binChain A single binary chain in the form of an integer vector.
// [[Rcpp::export]]
int rUnifInt (IntegerVector binChain) {
  long double u ;
  long double n = binChain.size() ;
  u = ceil(unif_rand() * (n - 2)) ;
  return u ;
}

//' Calculates the liklihood ratio test statistic for a single binary chain.
//'
//' \code{u1TestStat} takes a binary chain of data and calculates the liklihood
//' ratio test statistic associated with it.
//'
//' @param binChain A binary chain of data in the form of a one dimensional
//' integer vector.
//' @param nChainUniques A integer value representing the number of unique
//' values in \code{binChain}.
// [[Rcpp::export]]
float u1TestStat (IntegerVector binChain, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCounts(binChain, nChainUniques) ;

  float testStat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; j < 2 ; j++) {
      for (int k = 0 ; k < 2 ; k++) {
        if (n[i][j][k] != 0) {
          testStat = testStat + (n[i][j][k] * (log(n[i][j][k]) - log(kDimSum(n, i, k)) - log(iDimSum(n, j, k)) + log(ikDimSum(n, j)))) ;
        }
        else {
          testStat = testStat ;
        }
      }
    }
  }

  testStat = testStat * 2 ;

  return testStat ;
}

//' Calculates the Pearson's chi square test statistic for a single binary chain
//' .
//'
//' \code{chiSqTestStat} takes a binary chain of data and calculates the
//' Pearson's chi square test statistic associated with it.
//'
//' @param binChain A single binary chain of data in the form of an integer
//' vector.
//' @param nChainUniques A integer value representing the number of unique
//' values in \code{binChain}.
// [[Rcpp::export]]
float chiSqTestStat (IntegerVector binChain, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCounts(binChain, nChainUniques) ;

  float testStat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; i < 2 ; i++) {
      for (int k = 0 ; i < 2 ; i++) {
        testStat = testStat + (pow((n[i][j][k] - ((kDimSum(n, i, j) * iDimSum(n, j, k)) / ikDimSum(n, j))), 2) / ((kDimSum(n, i, j) * iDimSum(n, j, k)) / (ikDimSum(n, j)))) ;
      }
    }
  }

  return testStat ;
}

//' Calculate the chi square test statistics for many single binary chains.
//'
//' \code{chiSqTestStatArray} takes a two dimensional matrix of many binary
//' chains of data and returns a numberic vector filled with a chi square test
//' statistic for each of them.
//'
//' @param binChains A integer matrix of binary chains of data, with each row
//' being a different chain.
//' @param nChainUniques A integer value representing the number of unique
//' values in \code{binChains}.
// [[Rcpp::export]]
NumericVector chiSqTestStatArray (IntegerMatrix binChains, int nChainUniques) {
  int nrow = binChains.nrow() ;
  int ncol = binChains.ncol() ;
  NumericVector out(nrow) ;
  for (int i = 0 ; i < nrow ; i++) {
    IntegerVector tempBinChain(ncol) ;
    for (int j = 0 ; j < ncol ; j++) {
      tempBinChain[j] = binChains(i, j) ;
    }
    out(i) = chiSqTestStat(tempBinChain, nChainUniques) ;
  }
  return out ;
}

//' Indicate whether or not there is a run at a point in a integer vector.
//'
//' \code{indicateRun} takes an integer vector \code{binChain}, and two
//' integers, \code{p} and \code{i}, and tells the user if a run of length
//' \code{p} starting and index \code{i} in the form of a boolean value.
//'
//' @param binChain A binary chain of data in the form of an integer vector.
//' @param p A integer value representing the length of the run to test for.
//' @param i A integer value representing the location in \code{binChain} to
//' test for a run starting at.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
bool indicateRun (IntegerVector binChain, int p, int i) {
  int first = binChain[i] ;
  bool run = TRUE ;
  for (int k = 0 ; k < p ; k++) {
    if (binChain[i + k] != first) {
      return FALSE ;
    }
  }
  return run ;
}

//' Calculate the run test statistic for a single binary chain.
//'
//' \code{runTestStat} takes an integer vector \code{binChain} of a chain of
//' binary data, and a integer \code{p} representing the length of run to test
//' for. It returns the run test stat for that chain of data.
//'
//' @param binChain A binary chain of data in the form of an integer vector.
//' @param p An integer greater than one representing the length of run to test
//' for.
// [[Rcpp::export]]
int runTestStat (IntegerVector binChain, int p) {
  int n = binChain.size() ;
  int testStat = 0 ;
  for (int i = 0 ; i < n - p ; i++) {
    if (indicateRun(binChain, p, i) == TRUE) {
      testStat = testStat + 1 ;
    }
  }
  return testStat ;
}

//' Generate independent data from a single binary chain.
//'
//' \code{metropolis} takes a single binary chain of data in the form of an
//' integer vector and generates \code{b} new independent chains of data,
//' placing all of them in an integer matrix with the original data in the first
//' row.
//'
//' \code{metropolis} works by taking the supplied \code{binChain}, and
//' attempting \code{m} swaps on it, only preforming a swap of elements if
//' doing so maintains the number of transitions between states in the resulting
//' chain. \code{metropolis} then takes the resulting chain, and attempts
//' \code{m} swaps on it again, then saving the resulting vector in a new row
//' of an output matrix. \code{metropolis} does this \code{b} times, each time
//' saving the resulting vector. Once all of the new data has been generated,
//' \code{metropolis} returns the newly built integer matrix, of which the first
//' row is the original chain of data \code{binChain}.
//'
//' @param binChain A single binary chain of data represented by an integer
//' vector.
//' @param m An integer representing the number of swaps to be attempted.
//' @param b An integer representing the number of new chains of data to be
//' generated.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
IntegerMatrix metropolis (IntegerVector binChain, int m, int b) {
  IntegerMatrix out(b + 1, binChain.size()) ;
  for (int i = 0 ; i < binChain.size() ; i++) {
    out(0, i) = binChain[i] ;
  }

  IntegerVector startBinChain(binChain.size()) ;
  startBinChain = binChain ;

  for (int l = 0 ; l < m ; l++) {
    startBinChain = swap(startBinChain, rUnifInt(binChain), rUnifInt(binChain)) ;
  }
  IntegerVector tempBinChain(binChain.size()) ;

  for (int k = 1 ; k <= b ; k++) {
    tempBinChain = startBinChain ;
    for (int j = 0 ; j < m ; j++) {
      tempBinChain = swap(tempBinChain, rUnifInt(binChain), rUnifInt(binChain)) ;
    }
    for (int o = 0 ; o < binChain.size() ; o++) {
      out(k, o) = tempBinChain[o] ;
    }
  }
  return out ;
 }

//' Calulate likihood ratio test statistics for many binary chains.
//'
//' \code{u1TestStatArray} takes an integer matrix with each row denoting a
//' binary chain of data and returns an integer vector with liklihood ratio test
//' statistics corresponding to each binary chain.
//'
//' @param binChains A two dimensional integer matrix with each row denoting a
//' individual binary chain of data.
//' @param nChainUniques An integer value representing the number of unique
//' values in the binary chains found in \code{binChains}.
// [[Rcpp::export]]
NumericVector u1TestStatArray (IntegerMatrix binChains, int nChainUniques) {
  int nrow = binChains.nrow() ;
  int ncol = binChains.ncol() ;
  NumericVector out(nrow) ;
  for (int i = 0 ; i < nrow ; i++) {
    IntegerVector tempBinChain(ncol) ;
    for (int k = 0 ; k < ncol ; k++) {
      tempBinChain[k] = binChains(i, k) ;
    }
    out[i] = u1TestStat(tempBinChain, nChainUniques) ;
  }
  return out ;
}

//' Calculate run test statistics for many binary chains.
//'
//' \code{runTestStatArray} takes an integer matrix with each row denoting a
//' binary chain of data and returns an integer vector with run test statistics
//' for runs of length \code{p} corresponding to each binary chain.
//'
//' @param binChains A two dimensional integer matrix with each row denoting a
//' individual binary chain of data.
//' @param p An integer value representing the length of run to test for.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
NumericVector runTestStatArray (IntegerMatrix binChains, int p) {
  int nrow = binChains.nrow() ;
  int ncol = binChains.ncol() ;
  NumericVector out(nrow) ;
  for (int i = 0 ; i < nrow ; i++) {
    IntegerVector tempBinChain(ncol) ;
    for (int k = 0 ; k < ncol ; k++) {
      tempBinChain[k] = binChains(i, k) ;
    }
    out[i] = runTestStat(tempBinChain, p) ;
  }
  return out ;
}

//' Find the number of entries in a vector greater or equal to the value of the
//' first entry.
//'
//' \code{vecGreaterThan} counts the number of entries in a numberic vector
//' \code{testStats} whose values are greater than that of the value of the
//' first element of the vector.
//'
//' @param testStats A one dimensional numeric vector.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
int vecGreaterThan (NumericVector testStats) {
  int n = testStats.size() ;
  int rank = 0 ;
  for (int i = 1 ; i < n ; i++) {
    if (testStats[0] <= testStats[i]) {
      rank = rank + 1 ;
    }
  }
  return rank ;
}

//' Second order transition counts for a single binary chain.
//'
//' \code{nCounts} counts the number of second order transitions in a binary
//' chain of data, then returns a three dimensional vector whose indicies
//' represent the type of transition, and whose values represent the number of
//' times that each transition occurs in the chain.
//'
//' @param binChain An integer vector representing a chain of data.
//' @param nChainUniques The number or unique values in the chain
//' \code{binChains}, represented as an integer value.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
std::vector<std::vector<std::vector<int> > > nCounts (IntegerVector binChain, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n ;

  n.resize(nChainUniques);
  for (int i = 0; i < nChainUniques ; ++i) {
    n[i].resize(nChainUniques) ;

    for (int j = 0; j < nChainUniques; ++j)
      n[i][j].resize(nChainUniques) ;
  }

  for (int i = 0 ; i < nChainUniques ; i++) {
    for (int j = 0 ; j < nChainUniques ; j++) {
      for (int k = 0 ; k < nChainUniques ; k++) {
        n[i][j][k] = 0 ;
        for (int l = 0 ; l < binChain.size() - 2 ; l++) {
          if (binChain[l] == i and binChain[l + 1] == j and binChain[l + 2] == k) {
            n[i][j][k] = n[i][j][k] + 1 ;
          }
        }
      }
    }
  }

  return n ;
}

//' Second order transition counts for multiple binary chains.
//'
//' \code{nCountsMultiple} counts the number of second order transitions in
//' a integer matrix whose rows represent individual binary chains. It returns
//' a three dimensional vector whose indicies represent the type of transition,
//' and whose values represent the number of times that each transition occurs
//' in the set of chains.
//'
//' @param binChains A two dimensional integer vector, each of whose rows
//' represents a single binary chain of data.
//' @param nChainUniques The number of unique values in the set of chains
//' \code{binChains}, represented as an integer value.
// [[Rcpp::export]]
std::vector<std::vector<std::vector<int> > > nCountsMultiple (std::vector<std::vector<int> > binChains, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n ;

  n.resize(nChainUniques) ;
  for (int i = 0; i < nChainUniques ; ++i) {
    n[i].resize(nChainUniques) ;

    for (int j = 0; j < nChainUniques; ++j)
      n[i][j].resize(binChains.size()) ;
  }

  for (int i = 0 ; i < (binChains.size()) ; i++) {
    for (int j = 0 ; j < (binChains[1].size() - 1) ; j++) {
      for (int k = 0 ; k < nChainUniques ; k++) {
        for (int l = 0 ; l < nChainUniques ; l++) {
          if (binChains[i][j] == k and binChains[i][j + 1] == l) {
            n[k][l][i] = n[k][l][i] + 1 ;
          }
        }
      }
    }
  }

  return n ;
}

//' Sum all the values in the first dimension of a three dimensional integer
//' vector.
//'
//' \code{iDimSum} takes a three dimensional integer vector, fixes the values
//' of the second and thrid dimensions, and then sums the values of all the
//' entires in the vector with those two values for its second and third
//' dimensions.
//'
//' @param n A three dimensional integer vector.
//' @param j An integer that is a valid index of the second dimension of
//' \code{n}.
//' @param k An integer that is a valid index of the third dimension of
//' \code{n}.
// [[Rcpp::export]]
int iDimSum (std::vector<std::vector<std::vector<int> > > n, int j, int k) {
  int sum = 0 ;
  int dimi = n.size() ;
  for (int i = 0 ; i < dimi ; i++) {
    sum = sum + n[i][j][k] ;
  }
  return sum ;
}

//' Sum all the values in the second dimension of a three dimensional integer
//' vector.
//'
//' \code{jDimSum} takes a three dimensional integer vector, fixes the values
//' of the first and thrid dimensions, and then sums the values of all the
//' entires in the vector with those two values for its first and third
//' dimensions.
//'
//' @param n A three dimensional integer vector.
//' @param i An integer that is a valid index of the first dimension of
//' \code{n}.
//' @param k An integer that is a valid index of the third dimension of
//' \code{n}.
// [[Rcpp::export]]
int jDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int k) {
  int sum = 0 ;
  int dimj = n[0].size() ;
  for (int j = 0 ; j < dimj ; j++) {
    sum = sum + n[i][j][k] ;
  }
  return sum ;
}

//' Sum all the values in the third dimension of a three dimensional integer
//' vector.
//'
//' \code{kDimSum} takes a three dimensional integer vector, fixes the values
//' of the first and second dimensions, and then sums the values of all the
//' entires in the vector with those two values for its first and second
//' dimensions.
//'
//' @param n A three dimensional integer vector.
//' @param i An integer that is a valid index of the first dimension of
//' \code{n}.
//' @param j An integer that is a valid index of the second dimension of
//' \code{n}.
// [[Rcpp::export]]
int kDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int j) {
  int sum = 0 ;
  int dimk = n[0][0].size() ;
  for (int k = 0 ; k < dimk ; k++) {
    sum = sum + n[i][j][k] ;
  }
  return sum ;
}

//' Sum all the values in the first and third dimensions of a three dimensional
//' integer vector.
//'
//' \code{ikDimSum} takes a three dimensional integer vector, fixes the value
//' of the second dimension, and then sums the values of all the
//' entires in the vector with that value for its second dimension.
//'
//' @param n A three dimensional integer vector.
//' @param j An integer that is a valid index of the second dimension of
//' \code{n}.
// [[Rcpp::export]]
int ikDimSum (std::vector<std::vector<std::vector<int> > > n, int j) {
  int sum = 0 ;
  int dimi = n.size() ;
  int dimk = n[0][0].size() ;
  for (int i = 0 ; i < dimi ; i++) {
    for (int k = 0 ; k < dimk ; k++) {
      sum = sum + n[i][j][k] ;
    }
  }
  return sum ;
}

//' Sum all the values in the second and third dimensions of a three dimensional
//' integer vector.
//'
//' \code{ikDimSum} takes a three dimensional integer vector, fixes the value
//' of the first dimension, and then sums the values of all the
//' entires in the vector with that value for its first dimension.
//'
//' @param n A three dimensional integer vector.
//' @param i An integer that is a valid index of the first dimension of
//' \code{n}.
// [[Rcpp::export]]
int jkDimSum (std::vector<std::vector<std::vector<int> > > n, int i) {
  int sum = 0 ;
  int dimj = n[0].size() ;
  int dimk = n[0][0].size() ;

  for (int j = 0 ; j < dimj ; j++) {
    for (int k = 0 ; k < dimk ; k++) {
      sum = sum + n[i][j][k] ;
    }
  }
  return sum ;
}

//' Calculate the liklihood ratio test statistic for a set of binary chains of
//' data.
//'
//' \code{u6TestStat} takes a two dimensional integer vector \code{binChains}
//' in which each row represents a single binary chain of data, and calculates
//' a liklihood ratio test statistic for the entire set.
//'
//' @param binChains A two dimensional integer vector where each row is a
//' seperate binary chain of data.
//' @param nChainUniques An integer value representing the number of unique
//' elements in the set of chains \code{binChains}.
// [[Rcpp::export]]
float u6TestStat (std::vector<std::vector<int> > binChains, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCountsMultiple(binChains, nChainUniques) ;
  float testStat = 0 ;

  int dimi = n.size() ;
  int dimj = n[0].size() ;
  int dimk = n[0][0].size() ;

  for (int i = 0 ; i < dimi ; i++) {
    for (int j = 0 ; j < dimj ; j++) {
      for (int k = 0 ; k < dimk  ; k++) {
        if (n[i][j][k] != 0) {
          testStat = testStat + (n[i][j][k] * (log(n[i][j][k]) - log(jDimSum(n, i, k)) - log(kDimSum(n, i, j)) + log(jkDimSum(n, i)))) ;
        }
        else {
          testStat = testStat ;
        }
      }
    }
  }
  testStat = testStat * 2 ;
  return testStat ;
}

//' Generate independent data from a set of binary chains.
//'
//' \code{u6Metropolis} takes a set of binary chains of data in the form of an
//' integer matrix and returns a three dimensional integer vector with
//' with the first entry of the first dimension filled with the original set
//' of binary chains and the rest filled with independent chains generated by
//' \code{u6Metropolis}.
//'
//' \code{u6Metropolis} works by taking a supplied set of binary chains
//' \code{binChains} and attempting a number \code{m} swaps on entries of those
//' chains, only swapping if doing so maintains the number of transitions
//' between states that existed in the initial set of chains \code{binChains}.
//' After it does this, it repeats the process on the newly generated set of
//' binary chains of data \code{b} times, each time saving the new set of chains
//' in a three dimensional vector of data. The first entry of the first
//' dimension of this vector is used to store the original set of binary chains
//' \code{binChains}.
//'
//' @param binChains An integer matrix whose rows represent seperate binary
//' chains of data.
//' @param m An integer value representing the number of swaps to be attempted.
//' @param b An integer value representing the number of new sets of data to be
//' generated.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
std::vector<std::vector<std::vector<int> > > u6Metropolis (IntegerMatrix binChains, int m, int b) {
  std::vector<std::vector<std::vector<int> > > metropolis ;
  std::vector<std::vector<int> > backwardsBinChain ;
  std::vector<std::vector<int> > tempBinChain ;
  std::vector<std::vector<int> > convertedBinChain ;
  int nRow = binChains.nrow() ;
  int nCol = binChains.ncol() ;
  metropolis.resize(b + 1) ;
  for (int i = 0 ; i < b + 1 ; i++) {
    metropolis[i].resize(nRow) ;
    for (int j = 0 ; j < nRow ; j++) {
      metropolis[i][j].resize(nCol) ;
    }
  }

  backwardsBinChain.resize(nRow) ;
  tempBinChain.resize(nRow) ;
  convertedBinChain.resize(nRow) ;
  for (int k = 0 ; k < nRow ; k++) {
    backwardsBinChain[k].resize(nCol) ;
    tempBinChain[k].resize(nCol) ;
    convertedBinChain[k].resize(nCol) ;
  }
  for (int l = 0 ; l < nRow ; l++) {
    for (int n = 0 ; n < nCol ; n++) {
      metropolis[0][l][n] = binChains(l, n) ;
      convertedBinChain[l][n] = binChains(l, n) ;
    }
  }
  backwardsBinChain = swapMult(convertedBinChain, m) ;

  for (int x = 1 ; x < b + 1 ; x++) {
    tempBinChain = swapMult(backwardsBinChain, m) ;
    for (int y = 0 ; y < nRow ; y++) {
      for (int z = 0 ; z < nCol ; z++) {
        metropolis[x][y][z] = tempBinChain[y][z] ;
      }
    }
  }
  return metropolis ;
}

//' Calculate liklihood ratio test statistics for many sets of binary chains of
//' data.
//'
//' \code{u6TestStatArray} takes a three dimensional vector containing multiple
//' sets of binary chains of data, and returns a numeric vector with entires
//' corresponding to the liklihood ratio test statistics of each set of binary
//' chains of data.
//'
//' @param binChains A three dimensional vector containing sets of chains of
//' binary data.
//' @param nChainUniques An integer value representing the number of unique
//' elements in the set of chains \code{binChains}.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
NumericVector u6TestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques) {
  int size = binChains.size() ;
  NumericVector out(size) ;
  for (int i = 0 ; i < size ; i++) {
    out[i] = u6TestStat(binChains[i], nChainUniques) ;
  }
  return out ;
}

//' Calculate the Pearson's chi square test statistic for a set of binary chains
//' of data.
//'
//' \code{multipleChiSqTestStat} takes a two dimensional integer vector
//' \code{binChains} in which each row represents a single binary chain of data,
//' and calculates a Pearson's chi square test statistic for the entire set.
//'
//' @param binChains A two dimensional integer vector where each row is a
//' seperate binary chain of data.
//' @param nChainUniques An integer value representing the number of unique
//' elements in the set of chains \code{binChains}.
// [[Rcpp::export]]
float multipleChiSqTestStat (std::vector<std::vector<int> > binChains, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCountsMultiple(binChains, nChainUniques) ;
  float testStat = 0 ;

  int dimi = n.size() ;
  int dimj = n[0].size() ;
  int dimk = n[0][0].size() ;

  for (int i = 0 ; i < dimi ; i++) {
    for (int j = 0 ; j < dimj ; j++) {
      for (int k = 0 ; k < dimk  ; k++) {
        if (n[i][j][k] != 0) {
          testStat = testStat + (pow((n[i][j][k] - ((kDimSum(n, i, j) * iDimSum(n, j, k)) / ikDimSum(n, j))), 2) / ((kDimSum(n, i, j) * iDimSum(n, j, k)) / (ikDimSum(n, j)))) ;
        }
        else {
          testStat = testStat ;
        }
      }
    }
  }
  return testStat ;
}

//' Calculate Pearson's chi square test statistics for many sets of binary
//' chains of data.
//'
//' \code{multipleChiSqTestStatArray} takes a three dimensional vector
//' containing multiple sets of binary chains of data, and returns a numeric
//' vector with entires corresponding to the Pearson's chi square test
//' statistics of each set of binary chains of data.
//'
//' @param binChains A three dimensional vector containing sets of chains of
//' binary data.
//' @param nChainUniques An integer value representing the number of unique
//' elements in the set of chains \code{binChains}.
//' @export
//' @useDynLib maRkov
//[[Rcpp::export]]
NumericVector multipleChiSqTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques) {
  int size = binChains.size() ;
  NumericVector out(size) ;
  for (int i = 0 ; i < size ; i++) {
    out[i] = multipleChiSqTestStat(binChains[i], nChainUniques) ;
  }
  return out ;
}

//' Calculate the run test statistic for a set of binary chains of data and a
//' run of a certain length.
//'
//' \code{multipleRunTestStat} takes a two dimensional integer vector
//' \code{binChains} in which each row represents a single binary chain of data,
//' and calculates a run test statistic for a run of length \code{p} for the
//' entire set.
//'
//' @param binChains A two dimensional integer vector where each row is a
//' seperate binary chain of data.
//' @param p An integer value representing the length of run to test for.
//[[Rcpp::export]]
float multipleRunTestStat (std::vector<std::vector<int> > binChains, int p) {
  int nRow = binChains.size() ;
  int nCol = binChains[0].size() ;
  int testStat = 0 ;
  for (int i = 0 ; i < nRow ; i++) {
    for (int j = 0 ; j < nCol - p ; j++) {
      if (multipleIndicateRun(binChains[i], p, j)) {
        testStat = testStat + 1 ;
      }
    }
  }
  return testStat ;
}

//' Indicate whether or not a run of a certain length exists starting at a
//' certain point.
//'
//' \code{multipleIndicateRun} takes a single binary chain \code{binChain}, a
//' valid index of that chain \code{i}, and a length of run \code{p} and tests
//' whether or not a run of that length starts at index \code{i}.
//'
//' @param binChain A one dimensional integer vector representing a binary chain
//' of data.
//' @param p An integer representing the length of run to test for.
//' @param i An integer representing a valid index of \code{binChain}.
//[[Rcpp::export]]
bool multipleIndicateRun (std::vector<int> binChain, int p, int i) {
  int first = binChain[i] ;
  bool run = TRUE ;
  for (int k = 0 ; k < p ; k++) {
    if (binChain[i + k] != first) {
      return FALSE ;
    }
  }
  return run ;
}

//' Calculate run test statistics for many sets of chains of binary data.
//'
//' \code{multipleRunTestStatArray} takes a three dimensional integer vector
//' containing multiple sets of binary chains of data, and returns a numeric
//' vector with entries corresponding to the run test statistics for runs of
//' length p for each set of binary chains of data.
//'
//' @param binChains A three dimensional integer vector containing sets of
//' chains of binary data.
//' @param p An integer representing the length of run to test for.
//' @export
//' @useDynLib maRkov
//[[Rcpp::export]]
NumericVector multipleRunTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int p) {
  int n = binChains.size() ;
  NumericVector out(n) ;
  for (int i = 0 ; i < n ; i++) {
    out[i] = multipleRunTestStat(binChains[i], p) ;
  }
  return out ;
}
