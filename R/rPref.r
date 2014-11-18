#' Summary of the rPref package
#' 
#' rPref contains routines to select and visualize the maxima for a given strict
#' partial order. This especially includes the computation of the Pareto
#' frontier, also known as (Top-k) Skyline operator, and some
#' generalizations (database preferences).
#' 
#' @section Preference composition/selection:
#' 
#' \itemize{
#' \item Preferences are composed from base preference (see \code{\link{base_pref}}) and complex preferences (see \code{\link{complex_pref}}),
#' where especially the Pareto operator for Skylines is such a complex preferences. 
#' Additionally some base preference macros can be found in \code{\link{base_pref_macros}}.
#' \item The (Top(-Level)-k) preference selection \code{\link{psel}} allows to retrieve 
#'       the maxima of a preference (or Pareto frontier, Skyline), constructed with the functions above, on a given dataset.
#' }
#' 
#' @section Visualization/Conversion of preferences:
#' 
#' \itemize{
#' \item The visualization of the preference order in a Better-Than-Graph is possible via the \code{\link{get_btg}} function
#' in connection with the \code{\link{igraph}} package.
#' \item The adjacency list of the Better-Than-Graph (Hasse diagramm) can be accessed via \code{\link{get_hasse_diag}}.
#' \item The pareto frontier can be plotted using the \code{\link{plot_front}} function.
#' \item The preference query for some preference-supporting DBMS can be given by \code{\link{show.query}}.
#' }
#' 
#' @section Further information:
#' 
#' The rPref homepage is \url{http://www.p-roocks.de/rpref}. To submit bugs, feature requests or other comments, feel free to write a mail to me.
#' 
#' @author Patrick Roocks, \email{mail@@p-roocks.de}
#'
#' @docType package
#' @name rPref
#' @useDynLib rPref
#' @importFrom Rcpp cppFunction
#' @import igraph methods
NULL
