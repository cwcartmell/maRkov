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

