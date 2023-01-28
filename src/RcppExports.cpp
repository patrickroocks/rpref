// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_hasse_impl
NumericVector get_hasse_impl(const DataFrame& scores, List serial_pref);
RcppExport SEXP _rPref_get_hasse_impl(SEXP scoresSEXP, SEXP serial_prefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame& >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< List >::type serial_pref(serial_prefSEXP);
    rcpp_result_gen = Rcpp::wrap(get_hasse_impl(scores, serial_pref));
    return rcpp_result_gen;
END_RCPP
}
// pref_select_top_impl
DataFrame pref_select_top_impl(const DataFrame& scores, const List& serial_pref, int N, double alpha, int top, int at_least, int toplevel, bool and_connected, bool show_levels);
RcppExport SEXP _rPref_pref_select_top_impl(SEXP scoresSEXP, SEXP serial_prefSEXP, SEXP NSEXP, SEXP alphaSEXP, SEXP topSEXP, SEXP at_leastSEXP, SEXP toplevelSEXP, SEXP and_connectedSEXP, SEXP show_levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame& >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< const List& >::type serial_pref(serial_prefSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type top(topSEXP);
    Rcpp::traits::input_parameter< int >::type at_least(at_leastSEXP);
    Rcpp::traits::input_parameter< int >::type toplevel(toplevelSEXP);
    Rcpp::traits::input_parameter< bool >::type and_connected(and_connectedSEXP);
    Rcpp::traits::input_parameter< bool >::type show_levels(show_levelsSEXP);
    rcpp_result_gen = Rcpp::wrap(pref_select_top_impl(scores, serial_pref, N, alpha, top, at_least, toplevel, and_connected, show_levels));
    return rcpp_result_gen;
END_RCPP
}
// grouped_pref_sel_top_impl
DataFrame grouped_pref_sel_top_impl(const List& indices, const DataFrame& scores, const List& serial_pref, int N, double alpha, int top, int at_least, int toplevel, bool and_connected, bool show_levels);
RcppExport SEXP _rPref_grouped_pref_sel_top_impl(SEXP indicesSEXP, SEXP scoresSEXP, SEXP serial_prefSEXP, SEXP NSEXP, SEXP alphaSEXP, SEXP topSEXP, SEXP at_leastSEXP, SEXP toplevelSEXP, SEXP and_connectedSEXP, SEXP show_levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< const DataFrame& >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< const List& >::type serial_pref(serial_prefSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type top(topSEXP);
    Rcpp::traits::input_parameter< int >::type at_least(at_leastSEXP);
    Rcpp::traits::input_parameter< int >::type toplevel(toplevelSEXP);
    Rcpp::traits::input_parameter< bool >::type and_connected(and_connectedSEXP);
    Rcpp::traits::input_parameter< bool >::type show_levels(show_levelsSEXP);
    rcpp_result_gen = Rcpp::wrap(grouped_pref_sel_top_impl(indices, scores, serial_pref, N, alpha, top, at_least, toplevel, and_connected, show_levels));
    return rcpp_result_gen;
END_RCPP
}
// pref_select_impl
NumericVector pref_select_impl(const DataFrame& scores, const List& serial_pref, int N, double alpha);
RcppExport SEXP _rPref_pref_select_impl(SEXP scoresSEXP, SEXP serial_prefSEXP, SEXP NSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame& >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< const List& >::type serial_pref(serial_prefSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(pref_select_impl(scores, serial_pref, N, alpha));
    return rcpp_result_gen;
END_RCPP
}
// grouped_pref_sel_impl
NumericVector grouped_pref_sel_impl(const List& indices, const DataFrame& scores, const List& serial_pref, int N, double alpha);
RcppExport SEXP _rPref_grouped_pref_sel_impl(SEXP indicesSEXP, SEXP scoresSEXP, SEXP serial_prefSEXP, SEXP NSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< const DataFrame& >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< const List& >::type serial_pref(serial_prefSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(grouped_pref_sel_impl(indices, scores, serial_pref, N, alpha));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rPref_get_hasse_impl", (DL_FUNC) &_rPref_get_hasse_impl, 2},
    {"_rPref_pref_select_top_impl", (DL_FUNC) &_rPref_pref_select_top_impl, 9},
    {"_rPref_grouped_pref_sel_top_impl", (DL_FUNC) &_rPref_grouped_pref_sel_top_impl, 10},
    {"_rPref_pref_select_impl", (DL_FUNC) &_rPref_pref_select_impl, 4},
    {"_rPref_grouped_pref_sel_impl", (DL_FUNC) &_rPref_grouped_pref_sel_impl, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_rPref(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
