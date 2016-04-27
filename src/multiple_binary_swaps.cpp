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

std::vector<std::vector<int> > swapMult (std::vector<std::vector<int> > binChains, int m) ;
std::vector<std::vector<std::vector<int> > > u6Metropolis (IntegerMatrix binChains, int m, int b) ;
std::vector<std::vector<std::vector<int> > > nCountsMultiple (IntegerMatrix binChains, int nChainUniques) ;

float u6TestStat (IntegerMatrix binChains, int nChainUniques, int nMultChains, int lengthOfChains) ;
float multipleChiSqTestStat (std::vector<std::vector<int> > binChains, int nChainUniques) ;
bool multipleIndicateRun (std::vector<int> binChain, int p, int i) ;
float multipleRunTestStat (std::vector<std::vector<int> > binChains, int p) ;

NumericVector u6TestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques) ;
NumericVector multipleChiSqTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques) ;
NumericVector multipleRunTestStatArray (std::vector<std::vector<std::vector<int> > > binChains, int p) ;

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
        if (n[i][j][k] == 0 or jkDimSum(n, i) == 0 or jDimSum(n, i, k) == 0 or kDimSum(n, i, j) == 0) {
          testStat = testStat ;
        }
        else {
          testStat = testStat + (pow((n[i][j][k] - ((kDimSum(n, i, j) * jDimSum(n, i, k)) / jkDimSum(n, i))), 2) / ((kDimSum(n, i, j) * jDimSum(n, i, k)) / (jkDimSum(n, i)))) ;
        }
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
