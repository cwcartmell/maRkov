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

std::vector<int> swap (std::vector<int> binChain, int m) ;
std::vector<std::vector<int> > metropolis (IntegerVector binChain, int m, int b) ;
std::vector<std::vector<std::vector<int> > > nCounts (std::vector<int> binChain, int nChainUniques) ;

long double u1TestStat (IntegerVector binChain, int nChainUniques) ;
long double chiSqTestStat (IntegerVector binChain, int nChainUniques) ;
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
//' \code{swap} takes a one dimensional vector of integers \code{binChain} and
//' an integer \code{m}. It attempts to swap elements of \code{binChain}
//' \code{m} times, each time only completing the swap if it does not affect the
//' number of transitions between states in the sequence.
//'
//' @param binChain A binary one dimensional integer vector.
//' @param m A integer value representing the number of swaps to attempt.
// [[Rcpp::export]]
std::vector<int> swap (std::vector<int> binChain, int m) {
  int a, b, min, xa ;
  for (int i = 0 ; i < m ; i++) {
    a = ceil(unif_rand() * (binChain.size() - 2)) ;
    b = ceil(unif_rand() * (binChain.size() - 2)) ;
    min = std::min(a, b) ;
    if ((abs(a - b) > 1 and binChain[a - 1] + binChain[a + 1] == binChain[b - 1] + binChain[b + 1]))  {
      xa = binChain[a] ;
      binChain[a] = binChain[b] ;
      binChain[b] =  xa ;
    }
    else if ((abs(a - b) == 1 and binChain[min - 1] == binChain[min + 2])) {
      xa = binChain[a] ;
      binChain[a] = binChain[b] ;
      binChain[b] =  xa ;
    }
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
std::vector<std::vector<int> > metropolis (IntegerVector binChain, int m, int b) {
  std::vector<std::vector<int> > out ;
  std::vector<int> startBinChain ;
  std::vector<int> backwardsBinChain ;
  std::vector<int> tempBinChain ;

  out.resize(b + 1) ;
  for (int i = 0 ; i < b + 1 ; i++) {
    out[i].resize(binChain.size()) ;
  }

  startBinChain.resize(binChain.size()) ;
  backwardsBinChain.resize(binChain.size()) ;
  tempBinChain.resize(binChain.size()) ;

  for (int j = 0 ; j < binChain.size() ; j++) {
    out[0][j] = binChain[j] ;
    startBinChain[j] = binChain[j] ;
  }

  backwardsBinChain = swap(startBinChain, m) ;

  for (int k = 1 ; k < b + 1 ; k++) {
    tempBinChain = swap(backwardsBinChain, m) ;
    for (int l = 0 ; l < binChain.size() ; l++) {
      out[k][l] = tempBinChain[l] ;
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
std::vector<std::vector<std::vector<int> > > nCounts (std::vector<int> binChain, int nChainUniques) {
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
long double u1TestStat (std::vector<int> binChain, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCounts(binChain, nChainUniques) ;
  long double testStat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; j < 2 ; j++) {
      for (int k = 0 ; k < 2 ; k++) {
        if (n[i][j][k] != 0) {
          testStat = testStat + (n[i][j][k] * (log(n[i][j][k]) - log(kDimSum(n, i, j)) - log(iDimSum(n, j, k)) + log(ikDimSum(n, j)))) ;
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
long double chiSqTestStat (std::vector<int> binChain, int nChainUniques) {
  std::vector<std::vector<std::vector<int> > > n = nCounts(binChain, nChainUniques) ;

  long double testStat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; i < 2 ; i++) {
      for (int k = 0 ; i < 2 ; i++) {
        testStat = testStat + (pow((n[i][j][k] - ((kDimSum(n, i, j) / 1.0 * iDimSum(n, j, k) / 1.0) / (ikDimSum(n, j) / 1.0 ))), 2) / ((kDimSum(n, i, j) / 1.0 * iDimSum(n, j, k) / 1.0) / (ikDimSum(n, j) / 1.0))) ;
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
bool indicateRun (std::vector<int> binChain, int p, int i) {
  int first = binChain[i] ;
  for (int k = 0 ; k < p ; k++) {
    if (binChain[i + k] != first) {
      return FALSE ;
    }
  }
  return TRUE ;
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
int runTestStat (std::vector<int> binChain, int p) {
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
NumericVector u1TestStatArray (std::vector<std::vector<int> >  binChains, int nChainUniques) {
  NumericVector out(binChains.size()) ;
  for (int i = 0 ; i < binChains.size() ; i++) {
    out[i] = u1TestStat(binChains[i], nChainUniques) ;
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
NumericVector chiSqTestStatArray (std::vector<std::vector<int> > binChains, int nChainUniques) {
  NumericVector out(binChains.size()) ;
  for (int i = 0 ; i < binChains.size() ; i++) {
    out(i) = chiSqTestStat(binChains[i], nChainUniques) ;
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
NumericVector runTestStatArray (std::vector<std::vector<int> > binChains, int p) {
  NumericVector out(binChains.size()) ;
  for (int i = 0 ; i < binChains.size() ; i++) {
    out[i] = runTestStat(binChains[i], p) ;
  }
  return out ;
}
