// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;


RcppExport SEXP _rcpp_module_boot_stan_fit4Bekk_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4SVM_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4Sarima_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4tBekk_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4tgarch_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4varma_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_stan_fit4Bekk_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4Bekk_mod, 0},
    {"_rcpp_module_boot_stan_fit4SVM_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4SVM_mod, 0},
    {"_rcpp_module_boot_stan_fit4Sarima_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4Sarima_mod, 0},
    {"_rcpp_module_boot_stan_fit4tBekk_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4tBekk_mod, 0},
    {"_rcpp_module_boot_stan_fit4tgarch_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4tgarch_mod, 0},
    {"_rcpp_module_boot_stan_fit4varma_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4varma_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_varstan(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
