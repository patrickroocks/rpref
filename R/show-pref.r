#' Show preference in usual Query Languages
#' 
#' For a given preference this shows the \code{PREFERRING} clause of a database query in different SQL dialects which support preferences.
#' 
#' @param p A preference
#' @param dialect The preference query dialect, which determines the Syntax of the returned query. This has to be one of the following:
#' \describe{
#'    \item{\code{'EXASOL'}}{Syntax of the "Skyline" feature of the commercial database EXASOL EXASolution 5.}
#'     \item{\code{'Preference SQL'}}{Syntax of the Preference SQL system. 
#'      This is a research prototype developed at the Chair of Databases and Information Systems of the University of Augsburg. 
#'      See references for details.}
#' }
#'     
#' @details 
#' 
#' There are few database systems supporting Skyline queries. 
#' A Skyline query consists of a usual SQL query followed by a \code{PREFERRING}-clause (in some rarely used dialects also \code{SKYLINE OF}). 
#' For example consider a database table r(a,b). The preference selection \code{psel(r, low(a) * high(b))} can be expressed by (in the Exasol dialect):
#' 
#' \code{SELECT * FROM r PREFERRING LOW a PLUS HIGH b}
#' 
#' The \code{show.query} function generates just the PREFERRING-clause, i.e. \code{show.query(low(a) * high(b))} returns 
#' 
#' \code{PREFERRING LOW a PLUS HIGH b}
#' 
#' As usual in SQL queries all keywords are not case sensitive, i.e. \code{PLUS} or \code{plus} does not make any difference.
#' 
#' @references 
#' 
#' W. Kiessling, M. Endres, F. Wenzel (2011): The Preference SQL System - An Overview. IEEE Data Engineering Bulletin, Vol. 34 No. 3, pages 12-19.  
#' 
#' @examples
#' 
#' show.query(low(a) * high(b))
#' 
#' show.query(low(a) * high(b), dialect = 'Preference SQL')
#' 
#' @export
show.query <- function(p, dialect = 'EXASOL') {
 
  # Check dialect
  if (!(dialect %in% c(EXA, PSQL))) stop(paste0('The dialect "', dialect, '" is unknown.'))
  
  # Check for empty -> return empty string
  if (is.empty.pref(p)) return("")
  
  # Return PREFERRING-clause
  return(paste0('PREFERRING ', show_pref(p, dialect, 'PREF'))) # parent node "PREF" means root node
}

# Available Dialects
EXA <- 'EXASOL'
PSQL <- 'Preference SQL'

# Translation maps for operators
PSQL_PREF_MAP = list('*'        = 'AND', 
                     '&'        = 'PRIOR TO', 
                     '|'        = 'INTERSECT WITH',
                     '+'        = 'DISJOINT UNION',
                     'reverse'  = 'DUAL')

EXASOL_PREF_MAP = list('*'        = 'PLUS', 
                       '&'        = 'PRIOR TO', 
                       '|'        = NA,
                       '+'        = NA,
                       'reverse'  = 'INVERSE')



show_pref <- function(p, dialect, parent_op = '') {
  
  if (dialect == EXA) use_map <- EXASOL_PREF_MAP
  else                use_map <- PSQL_PREF_MAP
  
  if (is.basepref(p)) {
    return(show_base_pref(p, dialect))
  } else if (is.reversepref(p)) { 
    if (dialect == EXA) # INVERSE is notated as prefix in EXASOL
      return(paste0(use_map[['reverse']], ' (', show_pref(p$p, dialect), ')'))
    else  # "DUAL" is notated as suffix in Preference SQL!
      return(paste0('(', show_pref(p$p, dialect), ') ', use_map[['reverse']]))
      
  } else if (is.complexpref(p)) {
    # usual complex preference (not inverse!)
    opchr <- use_map[[p$op]]
    # Check if operator is available in the given dialect
    if (is.na(opchr)) stop('Operator "', p$op, '" is not available in the dialect "', dialect, '".')
    res <- paste0(show_pref(p$p1, dialect, opchr), ' ', opchr, ' ', show_pref(p$p2, dialect, opchr))
    if (parent_op != opchr && parent_op != 'PREF') res <- paste0('(', res, ')')
    return(res)
  }
}


# Translate Preference to PrefSQL
show_base_pref <- function(p, dialect) {
  if (dialect == EXA) {
    if (is.lowpref(p)) {
      return(paste0('LOW ', get_expr(p)))
    } else if (is.highpref(p)) {
      return(paste0('HIGH ', get_expr(p)))
    } else if (is.truepref(p)) {
      return(paste0(get_expr(p)))
    }
  } else {
    if (is.lowpref(p)) {
      return(paste0(get_expr(p), ' LOWEST'))
    } else if (is.highpref(p)) {
      return(paste0(get_expr(p), ' HIGHEST'))
    } else if (is.truepref(p)) {
      return(paste0(get_expr(p), ' = TRUE'))
    }
  }
}


# Get expression of base preference
get_expr <- function(p) {
  expr <- p$expr
  if (length(expr[[1]]) == 1) return(as.character(p$expr))
  else return(paste0('(', as.character(p$expr), ')')) # embrace complex expressions
}

