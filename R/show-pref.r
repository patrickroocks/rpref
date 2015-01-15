#' Show preference in usual Query Languages
#' 
#' For a given preference this shows the \code{PREFERRING} clause of a database query in different SQL dialects which support preferences.
#' 
#' @param p A preference
#' @param dialect The preference query dialect, which determines the Syntax of the returned query. This has to be one of the following:
#' \describe{
#'    \item{\code{'EXASOL'}}{Syntax of the "Skyline" feature of the commercial database EXASOL EXASolution 5.}
#'    \item{\code{'Preference SQL'}}{Syntax of the Preference SQL system. 
#'      This is a research prototype developed at the Chair of Databases and Information Systems of the University of Augsburg. 
#'      See references for details.}
#' }
#' @param df Optional to specifiy a dataframe on which the preference operates causing a partial evaluation. See \code{\link{show.pref}} for details.
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
show.query <- function(p, dialect = 'EXASOL', df = NULL) {
 
  # Check dialect
  if (!(dialect %in% c(EXA, PSQL))) stop(paste0('The dialect "', dialect, '" is unknown.'))
  
  # Check for empty -> return empty string
  if (is.empty.pref(p)) return("")
  
  # Return PREFERRING-clause
  return(paste0('PREFERRING ', show_pref_sql(p, dialect, 'PREF', get_static_terms(df)))) # parent node "PREF" means root node
}


#' Show a (partially evaluated) preference
#' 
#' @param p The preference to be shown.
#' @param df (Optional) A dataframe on which the preference operates.
#' 
#' @details If \code{df} is not given this function is identical to typing \code{p} on the console or calling \code{show(p)}.
#' This standard show function does not do any evaluation and just converts the expressions to characters.
#' A given dataframe causes that all expressions in \code{p} are evaluated except the attributes in \code{p}, 
#' i.e. the column names in \code{df}. The content of the dataframe \code{df} does not matter; 
#' only \code{colnames(df)} is taken to get the "free variables" in \code{p}
#' 
#' @examples
#' 
#' f <- function(x) 2*x
#' p <- true(cyl == f(1))
#' 
#' # prints 'cyl == f(x)'
#' p
#' 
#' # prints 'cyl == 2'
#' show.pref(p, mtcars)
#' 
#' @export
show.pref <- function(p, df = NULL) {

  # Output string representation
  return(cat(paste0('[Preference] ', unbrace(p$get_str(static_terms = get_static_terms(df))))))
}

# Get attributes of a data set as symbols
get_static_terms <- function(df) {
  # Get static terms (all attributes of dataset, if given)
  if (!is.null(df)) return(lapply(colnames(df), as.symbol))
  else return(NULL)  
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



show_pref_sql <- function(p, dialect, parent_op = '', static_terms = NULL) {
  
  if (dialect == EXA) use_map <- EXASOL_PREF_MAP
  else                use_map <- PSQL_PREF_MAP
  
  if (is.basepref(p)) {
    return(show_base_pref(p, dialect, static_terms))
  } else if (is.reversepref(p)) { 
    if (dialect == EXA) # INVERSE is notated as prefix in EXASOL
      return(paste0(use_map[['reverse']], ' (', show_pref_sql(p$p, dialect, static_terms = static_terms), ')'))
    else  # "DUAL" is notated as suffix in Preference SQL!
      return(paste0('(', show_pref_sql(p$p, dialect, static_terms = static_terms), ') ', use_map[['reverse']]))
      
  } else if (is.complexpref(p)) {
    # usual complex preference (not inverse!)
    opchr <- use_map[[p$op]]
    # Check if operator is available in the given dialect
    if (is.na(opchr)) stop('Operator "', p$op, '" is not available in the dialect "', dialect, '".')
    res <- paste0(show_pref_sql(p$p1, dialect, opchr, static_terms = static_terms), ' ', opchr, ' ', 
                  show_pref_sql(p$p2, dialect, opchr, static_terms = static_terms))
    if (parent_op != opchr && parent_op != 'PREF') res <- paste0('(', res, ')')
    return(res)
  }
}


# Translate Preference to PrefSQL
show_base_pref <- function(p, dialect, static_terms = NULL) {
  
  expr_str <- get_expr_sql(p, static_terms)
  
  if (dialect == EXA) {
    if (is.lowpref(p)) {
      return(paste0('LOW ', expr_str))
    } else if (is.highpref(p)) {
      return(paste0('HIGH ', expr_str))
    } else if (is.truepref(p)) {
      return(paste0(expr_str))
    }
  } else {
    if (is.lowpref(p)) {
      return(paste0(expr_str, ' LOWEST'))
    } else if (is.highpref(p)) {
      return(paste0(expr_str, ' HIGHEST'))
    } else if (is.truepref(p)) {
      return(paste0(expr_str, ' = TRUE'))
    }
  }
}


# Get (evaluated, if static_terms is given) expression of base preference
# Add braces for non-single term 
get_expr_sql <- function(p, static_terms = NULL) {
  expr_str <- p$get_expr_str(static_terms)
  if (length(p$expr[[1]]) != 1) return(paste0('(', expr_str, ')'))  # embrace complex expressions
  else return(expr_str) 
}
