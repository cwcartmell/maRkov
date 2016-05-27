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

std::vector<int> swap (std::vector<int> bin_chain, int m) ;
std::vector<std::vector<int> > metropolis (IntegerVector bin_chain, int m, int b) ;
std::vector<std::vector<std::vector<int> > > n_counts (std::vector<int> bin_chain, int n_chain_uniques) ;

long double u1_test_stat (IntegerVector bin_chain, int n_chain_uniques) ;
long double chi_sq_test_stat (IntegerVector bin_chain, int n_chain_uniques) ;
bool indicate_run (IntegerVector bin_chain, int p, int i) ;
int run_test_stat (IntegerVector bin_chain, int p) ;

NumericVector u1_test_stat_array (IntegerMatrix bin_chains, int n_chain_uniques) ;
NumericVector chi_sq_test_stat_array (IntegerMatrix bin_chains, int n_chain_uniques) ;
NumericVector run_test_stat_array (IntegerMatrix bin_chains, int p) ;

//' Swap elements of single binary chains
//'
//' \code{swap} is used to swap elements of a single binary chain if doing so
//' maintains the same number of transitions between the two states of that
//' chain.
//'
//' \code{swap} takes a one dimensional vector of integers \code{bin_chain} and
//' an integer \code{m}. It attempts to swap elements of \code{bin_chain}
//' \code{m} times, each time only completing the swap if it does not affect the
//' number of transitions between states in the sequence.
//'
//' @param bin_chain A binary one dimensional integer vector.
//' @param m A integer value representing the number of swaps to attempt.
// [[Rcpp::export]]
std::vector<int> swap (std::vector<int> bin_chain, int m) {
  int a, b, min, xa ;
  for (int i = 0 ; i < m ; i++) {
    a = ceil(unif_rand() * (bin_chain.size() - 2)) ;
    b = ceil(unif_rand() * (bin_chain.size() - 2)) ;
    min = std::min(a, b) ;
    if ((abs(a - b) > 1 and bin_chain[a - 1] + bin_chain[a + 1] == bin_chain[b - 1] + bin_chain[b + 1]))  {
      xa = bin_chain[a] ;
      bin_chain[a] = bin_chain[b] ;
      bin_chain[b] =  xa ;
    }
    else if ((abs(a - b) == 1 and bin_chain[min - 1] == bin_chain[min + 2])) {
      xa = bin_chain[a] ;
      bin_chain[a] = bin_chain[b] ;
      bin_chain[b] =  xa ;
    }
  }
  return bin_chain ;
}

//' Generate independent data from a single binary chain.
//'
//' \code{metropolis} takes a single binary chain of data in the form of an
//' integer vector and generates \code{b} new independent chains of data,
//' placing all of them in an integer matrix with the original data in the first
//' row.
//'
//' \code{metropolis} works by taking the supplied \code{bin_chain}, and
//' attempting \code{m} swaps on it, only preforming a swap of elements if
//' doing so maintains the number of transitions between states in the resulting
//' chain. \code{metropolis} then takes the resulting chain, and attempts
//' \code{m} swaps on it again, then saving the resulting vector in a new row
//' of an output matrix. \code{metropolis} does this \code{b} times, each time
//' saving the resulting vector. Once all of the new data has been generated,
//' \code{metropolis} returns the newly built integer matrix, of which the first
//' row is the original chain of data \code{bin_chain}.
//'
//' @param bin_chain A single binary chain of data represented by an integer
//' vector.
//' @param m An integer representing the number of swaps to be attempted.
//' @param b An integer representing the number of new chains of data to be
//' generated.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
std::vector<std::vector<int> > metropolis (IntegerVector bin_chain, int m, int b) {
  std::vector<std::vector<int> > out ;
  std::vector<int> start_bin_chain ;
  std::vector<int> backwards_bin_chain ;
  std::vector<int> temp_bin_chain ;

  out.resize(b + 1) ;
  for (int i = 0 ; i < b + 1 ; i++) {
    out[i].resize(bin_chain.size()) ;
  }

  start_bin_chain.resize(bin_chain.size()) ;
  backwards_bin_chain.resize(bin_chain.size()) ;
  temp_bin_chain.resize(bin_chain.size()) ;

  for (int j = 0 ; j < bin_chain.size() ; j++) {
    out[0][j] = bin_chain[j] ;
    start_bin_chain[j] = bin_chain[j] ;
  }

  backwards_bin_chain = swap(start_bin_chain, m) ;

  for (int k = 1 ; k < b + 1 ; k++) {
    temp_bin_chain = swap(backwards_bin_chain, m) ;
    for (int l = 0 ; l < bin_chain.size() ; l++) {
      out[k][l] = temp_bin_chain[l] ;
    }
  }
  return out ;
}

//' Second order transition counts for a single binary chain.
//'
//' \code{n_counts} counts the number of second order transitions in a binary
//' chain of data, then returns a three dimensional vector whose indices
//' represent the type of transition, and whose values represent the number of
//' times that each transition occurs in the chain.
//'
//' @param bin_chain An integer vector representing a chain of data.
//' @param n_chain_uniques The number or unique values in the chain
//' \code{bin_chains}, represented as an integer value.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
std::vector<std::vector<std::vector<int> > > n_counts (std::vector<int> bin_chain, int n_chain_uniques) {
  std::vector<std::vector<std::vector<int> > > n ;

  n.resize(n_chain_uniques);
  for (int i = 0; i < n_chain_uniques ; ++i) {
    n[i].resize(n_chain_uniques) ;
    for (int j = 0; j < n_chain_uniques; ++j)
      n[i][j].resize(n_chain_uniques) ;
  }

  for (int i = 0 ; i < n_chain_uniques ; i++) {
    for (int j = 0 ; j < n_chain_uniques ; j++) {
      for (int k = 0 ; k < n_chain_uniques ; k++) {
        n[i][j][k] = 0 ;
        for (int l = 0 ; l < bin_chain.size() - 2 ; l++) {
          if (bin_chain[l] == i and bin_chain[l + 1] == j and bin_chain[l + 2] == k) {
            n[i][j][k] = n[i][j][k] + 1 ;
          }
        }
      }
    }
  }
  return n ;
}

//' Calculates the likelihood ratio test statistic for a single binary chain.
//'
//' \code{u1_test_stat} takes a binary chain of data and calculates the likelihood
//' ratio test statistic associated with it.
//'
//' @param bin_chain A binary chain of data in the form of a one dimensional
//' integer vector.
//' @param n_chain_uniques A integer value representing the number of unique
//' values in \code{bin_chain}.
// [[Rcpp::export]]
long double u1_test_stat (std::vector<int> bin_chain, int n_chain_uniques) {
  std::vector<std::vector<std::vector<int> > > n = n_counts(bin_chain, n_chain_uniques) ;
  long double test_stat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; j < 2 ; j++) {
      for (int k = 0 ; k < 2 ; k++) {
        if (n[i][j][k] != 0) {
          test_stat = test_stat + (n[i][j][k] * (log(n[i][j][k]) - log(k_dim_sum(n, i, j)) - log(i_dim_sum(n, j, k)) + log(ik_dim_sum(n, j)))) ;
        }
        else {
          test_stat = test_stat ;
        }
      }
    }
  }
  test_stat = test_stat * 2 ;
  return test_stat ;
}

//' Calculates the Pearson's chi square test statistic for a single binary chain
//' .
//'
//' \code{chi_sq_test_stat} takes a binary chain of data and calculates the
//' Pearson's chi square test statistic associated with it.
//'
//' @param bin_chain A single binary chain of data in the form of an integer
//' vector.
//' @param n_chain_uniques A integer value representing the number of unique
//' values in \code{bin_chain}.
// [[Rcpp::export]]
long double chi_sq_test_stat (std::vector<int> bin_chain, int n_chain_uniques) {
  std::vector<std::vector<std::vector<int> > > n = n_counts(bin_chain, n_chain_uniques) ;

  long double test_stat = 0 ;

  for (int i = 0 ; i < 2 ; i++) {
    for (int j = 0 ; j < 2 ; j++) {
      for (int k = 0 ; k < 2 ; k++) {
        test_stat = test_stat + (pow((n[i][j][k] - ((k_dim_sum(n, i, j) / 1.0 * i_dim_sum(n, j, k) / 1.0) / (ik_dim_sum(n, j) / 1.0 ))), 2) / ((k_dim_sum(n, i, j) / 1.0 * i_dim_sum(n, j, k) / 1.0) / (ik_dim_sum(n, j) / 1.0))) ;
      }
    }
  }

  return test_stat ;
}

//' Indicate whether or not there is a run at a point in a integer vector.
//'
//' \code{indicate_run} takes an integer vector \code{bin_chain}, and two
//' integers, \code{p} and \code{i}, and tells the user if a run of length
//' \code{p} starting and index \code{i} in the form of a Boolean value.
//'
//' @param bin_chain A binary chain of data in the form of an integer vector.
//' @param p A integer value representing the length of the run to test for.
//' @param i A integer value representing the location in \code{bin_chain} to
//' test for a run starting at.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
bool indicate_run (std::vector<int> bin_chain, int p, int i) {
  for (int k = 0 ; k < p ; k++) {
    if (bin_chain[i + k] != 1) {
      return FALSE ;
    }
  }
  return TRUE ;
}

//' Calculate the run test statistic for a single binary chain.
//'
//' \code{run_test_stat} takes an integer vector \code{bin_chain} of a chain of
//' binary data, and a integer \code{p} representing the length of run to test
//' for. It returns the run test stat for that chain of data.
//'
//' @param bin_chain A binary chain of data in the form of an integer vector.
//' @param p An integer greater than one representing the length of run to test
//' for.
// [[Rcpp::export]]
int run_test_stat (std::vector<int> bin_chain, int p) {
  int n = bin_chain.size() ;
  int test_stat = 0 ;
  for (int i = 0 ; i < n - p ; i++) {
    if (indicate_run(bin_chain, p, i) == TRUE) {
      test_stat = test_stat + 1 ;
    }
  }
  return test_stat ;
}

//' Calculate likelihood ratio test statistics for many binary chains.
//'
//' \code{u1_test_stat_array} takes an integer matrix with each row denoting a
//' binary chain of data and returns an integer vector with likelihood ratio test
//' statistics corresponding to each binary chain.
//'
//' @param bin_chains A two dimensional integer matrix with each row denoting a
//' individual binary chain of data.
//' @param n_chain_uniques An integer value representing the number of unique
//' values in the binary chains found in \code{bin_chains}.
// [[Rcpp::export]]
NumericVector u1_test_stat_array (std::vector<std::vector<int> >  bin_chains, int n_chain_uniques) {
  NumericVector out(bin_chains.size()) ;
  for (int i = 0 ; i < bin_chains.size() ; i++) {
    out[i] = u1_test_stat(bin_chains[i], n_chain_uniques) ;
  }
  return out ;
}

//' Calculate the chi square test statistics for many single binary chains.
//'
//' \code{chi_sq_test_stat_array} takes a two dimensional matrix of many binary
//' chains of data and returns a numeric vector filled with a chi square test
//' statistic for each of them.
//'
//' @param bin_chains A integer matrix of binary chains of data, with each row
//' being a different chain.
//' @param n_chain_uniques A integer value representing the number of unique
//' values in \code{bin_chains}.
// [[Rcpp::export]]
NumericVector chi_sq_test_stat_array (std::vector<std::vector<int> > bin_chains, int n_chain_uniques) {
  NumericVector out(bin_chains.size()) ;
  for (int i = 0 ; i < bin_chains.size() ; i++) {
    out(i) = chi_sq_test_stat(bin_chains[i], n_chain_uniques) ;
  }
  return out ;
}

//' Calculate run test statistics for many binary chains.
//'
//' \code{run_test_stat_array} takes an integer matrix with each row denoting a
//' binary chain of data and returns an integer vector with run test statistics
//' for runs of length \code{p} corresponding to each binary chain.
//'
//' @param bin_chains A two dimensional integer matrix with each row denoting a
//' individual binary chain of data.
//' @param p An integer value representing the length of run to test for.
//' @export
//' @useDynLib maRkov
// [[Rcpp::export]]
NumericVector run_test_stat_array (std::vector<std::vector<int> > bin_chains, int p) {
  NumericVector out(bin_chains.size()) ;
  for (int i = 0 ; i < bin_chains.size() ; i++) {
    out[i] = run_test_stat(bin_chains[i], p) ;
  }
  return out ;
}
