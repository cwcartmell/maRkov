// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// swap
IntegerVector swap(IntegerVector binChain, int i, int j);
RcppExport SEXP maRkov_swap(SEXP binChainSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    __result = Rcpp::wrap(swap(binChain, i, j));
    return __result;
END_RCPP
}
// swapMult
std::vector<std::vector<int> > swapMult(std::vector<std::vector<int> > binChains, int m);
RcppExport SEXP maRkov_swapMult(SEXP binChainsSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    __result = Rcpp::wrap(swapMult(binChains, m));
    return __result;
END_RCPP
}
// rUnifInt
int rUnifInt(IntegerVector binChain);
RcppExport SEXP maRkov_rUnifInt(SEXP binChainSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    __result = Rcpp::wrap(rUnifInt(binChain));
    return __result;
END_RCPP
}
// u1TestStat
float u1TestStat(IntegerVector binChain, int nChainUniques);
RcppExport SEXP maRkov_u1TestStat(SEXP binChainSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(u1TestStat(binChain, nChainUniques));
    return __result;
END_RCPP
}
// chiSqTestStat
float chiSqTestStat(IntegerVector binChain, int nChainUniques);
RcppExport SEXP maRkov_chiSqTestStat(SEXP binChainSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(chiSqTestStat(binChain, nChainUniques));
    return __result;
END_RCPP
}
// chiSqTestStatArray
NumericVector chiSqTestStatArray(IntegerMatrix binChains, int nChainUniques);
RcppExport SEXP maRkov_chiSqTestStatArray(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerMatrix >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(chiSqTestStatArray(binChains, nChainUniques));
    return __result;
END_RCPP
}
// indicateRun
bool indicateRun(IntegerVector binChain, int p, int i);
RcppExport SEXP maRkov_indicateRun(SEXP binChainSEXP, SEXP pSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    __result = Rcpp::wrap(indicateRun(binChain, p, i));
    return __result;
END_RCPP
}
// runTestStat
int runTestStat(IntegerVector binChain, int p);
RcppExport SEXP maRkov_runTestStat(SEXP binChainSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    __result = Rcpp::wrap(runTestStat(binChain, p));
    return __result;
END_RCPP
}
// metropolis
IntegerMatrix metropolis(IntegerVector binChain, int m, int b);
RcppExport SEXP maRkov_metropolis(SEXP binChainSEXP, SEXP mSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    __result = Rcpp::wrap(metropolis(binChain, m, b));
    return __result;
END_RCPP
}
// u1TestStatArray
NumericVector u1TestStatArray(IntegerMatrix binChains, int nChainUniques);
RcppExport SEXP maRkov_u1TestStatArray(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerMatrix >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(u1TestStatArray(binChains, nChainUniques));
    return __result;
END_RCPP
}
// runTestStatArray
NumericVector runTestStatArray(IntegerMatrix binChains, int p);
RcppExport SEXP maRkov_runTestStatArray(SEXP binChainsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerMatrix >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    __result = Rcpp::wrap(runTestStatArray(binChains, p));
    return __result;
END_RCPP
}
// vecGreaterThan
int vecGreaterThan(NumericVector testStats);
RcppExport SEXP maRkov_vecGreaterThan(SEXP testStatsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type testStats(testStatsSEXP);
    __result = Rcpp::wrap(vecGreaterThan(testStats));
    return __result;
END_RCPP
}
// nCounts
std::vector<std::vector<std::vector<int> > > nCounts(IntegerVector binChain, int nChainUniques);
RcppExport SEXP maRkov_nCounts(SEXP binChainSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(nCounts(binChain, nChainUniques));
    return __result;
END_RCPP
}
// nCountsMultiple
std::vector<std::vector<std::vector<int> > > nCountsMultiple(std::vector<std::vector<int> > binChains, int nChainUniques);
RcppExport SEXP maRkov_nCountsMultiple(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(nCountsMultiple(binChains, nChainUniques));
    return __result;
END_RCPP
}
// iDimSum
int iDimSum(std::vector<std::vector<std::vector<int> > > n, int j, int k);
RcppExport SEXP maRkov_iDimSum(SEXP nSEXP, SEXP jSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    __result = Rcpp::wrap(iDimSum(n, j, k));
    return __result;
END_RCPP
}
// jDimSum
int jDimSum(std::vector<std::vector<std::vector<int> > > n, int i, int k);
RcppExport SEXP maRkov_jDimSum(SEXP nSEXP, SEXP iSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    __result = Rcpp::wrap(jDimSum(n, i, k));
    return __result;
END_RCPP
}
// kDimSum
int kDimSum(std::vector<std::vector<std::vector<int> > > n, int i, int j);
RcppExport SEXP maRkov_kDimSum(SEXP nSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    __result = Rcpp::wrap(kDimSum(n, i, j));
    return __result;
END_RCPP
}
// ikDimSum
int ikDimSum(std::vector<std::vector<std::vector<int> > > n, int j);
RcppExport SEXP maRkov_ikDimSum(SEXP nSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    __result = Rcpp::wrap(ikDimSum(n, j));
    return __result;
END_RCPP
}
// jkDimSum
int jkDimSum(std::vector<std::vector<std::vector<int> > > n, int i);
RcppExport SEXP maRkov_jkDimSum(SEXP nSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    __result = Rcpp::wrap(jkDimSum(n, i));
    return __result;
END_RCPP
}
// u6TestStat
float u6TestStat(std::vector<std::vector<int> > binChains, int nChainUniques);
RcppExport SEXP maRkov_u6TestStat(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(u6TestStat(binChains, nChainUniques));
    return __result;
END_RCPP
}
// u6Metropolis
std::vector<std::vector<std::vector<int> > > u6Metropolis(IntegerMatrix binChains, int m, int b);
RcppExport SEXP maRkov_u6Metropolis(SEXP binChainsSEXP, SEXP mSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerMatrix >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    __result = Rcpp::wrap(u6Metropolis(binChains, m, b));
    return __result;
END_RCPP
}
// u6TestStatArray
NumericVector u6TestStatArray(std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques);
RcppExport SEXP maRkov_u6TestStatArray(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(u6TestStatArray(binChains, nChainUniques));
    return __result;
END_RCPP
}
// multipleChiSqTestStat
float multipleChiSqTestStat(std::vector<std::vector<int> > binChains, int nChainUniques);
RcppExport SEXP maRkov_multipleChiSqTestStat(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(multipleChiSqTestStat(binChains, nChainUniques));
    return __result;
END_RCPP
}
// multipleChiSqTestStatArray
NumericVector multipleChiSqTestStatArray(std::vector<std::vector<std::vector<int> > > binChains, int nChainUniques);
RcppExport SEXP maRkov_multipleChiSqTestStatArray(SEXP binChainsSEXP, SEXP nChainUniquesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type nChainUniques(nChainUniquesSEXP);
    __result = Rcpp::wrap(multipleChiSqTestStatArray(binChains, nChainUniques));
    return __result;
END_RCPP
}
// multipleRunTestStat
float multipleRunTestStat(std::vector<std::vector<int> > binChains, int p);
RcppExport SEXP maRkov_multipleRunTestStat(SEXP binChainsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<int> > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    __result = Rcpp::wrap(multipleRunTestStat(binChains, p));
    return __result;
END_RCPP
}
// multipleIndicateRun
bool multipleIndicateRun(std::vector<int> binChain, int p, int i);
RcppExport SEXP maRkov_multipleIndicateRun(SEXP binChainSEXP, SEXP pSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<int> >::type binChain(binChainSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    __result = Rcpp::wrap(multipleIndicateRun(binChain, p, i));
    return __result;
END_RCPP
}
// multipleRunTestStatArray
NumericVector multipleRunTestStatArray(std::vector<std::vector<std::vector<int> > > binChains, int p);
RcppExport SEXP maRkov_multipleRunTestStatArray(SEXP binChainsSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<std::vector<std::vector<int> > > >::type binChains(binChainsSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    __result = Rcpp::wrap(multipleRunTestStatArray(binChains, p));
    return __result;
END_RCPP
}
