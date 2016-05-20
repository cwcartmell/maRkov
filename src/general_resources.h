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

int i_dim_sum (std::vector<std::vector<std::vector<int> > > n, int j, int k) ;
int j_dim_sum (std::vector<std::vector<std::vector<int> > > n, int i, int k) ;
int k_dim_sum (std::vector<std::vector<std::vector<int> > > n, int i, int j) ;
int ik_dim_sum (std::vector<std::vector<std::vector<int> > > n, int j) ;
int jk_dim_sum (std::vector<std::vector<std::vector<int> > > n, int i) ;
int vec_greater_than (NumericVector testStats) ;
