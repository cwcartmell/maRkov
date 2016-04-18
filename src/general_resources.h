#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <iostream>
#include <Rmath.h>
#include <math.h>
#include <vector>


//Enable C++11
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp ;

int iDimSum (std::vector<std::vector<std::vector<int> > > n, int j, int k) ;
int jDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int k) ;
int kDimSum (std::vector<std::vector<std::vector<int> > > n, int i, int j) ;
int ikDimSum (std::vector<std::vector<std::vector<int> > > n, int j) ;
int jkDimSum (std::vector<std::vector<std::vector<int> > > n, int i) ;
int vecGreaterThan (NumericVector testStats) ;
int rUnifInt (IntegerVector binChain) ;
