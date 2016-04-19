#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <iostream>
#include <Rmath.h>
#include <math.h>
#include <vector>
#include "general_resources.h"

//Enable C++11
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp ;

IntegerVector swap (IntegerVector binChain, int i, int j) ;
IntegerMatrix metropolis (IntegerVector binChain, int m, int b) ;
std::vector<std::vector<std::vector<int> > > nCounts (IntegerVector binChain, int nChainUniques) ;

float u1TestStat (IntegerVector binChain, int nChainUniques) ;
float chiSqTestStat (IntegerVector binChain, int nChainUniques) ;
bool indicateRun (IntegerVector binChain, int p, int i) ;
int runTestStat (IntegerVector binChain, int p) ;

NumericVector u1TestStatArray (IntegerMatrix binChains, int nChainUniques) ;
NumericVector chiSqTestStatArray (IntegerMatrix binChains, int nChainUniques) ;
NumericVector runTestStatArray (IntegerMatrix binChains, int p) ;

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