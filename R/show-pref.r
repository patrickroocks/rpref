#' Show preference in usual Query Languages
#' 
#' For a given preference this shows the \code{PREFERRING} clause of a database query in different SQL dialects which support preferences.
#' 
#' @param p A preference
#' @param dialect The preference query dialect, which determines the Syntax of the returned query. This has to be one of the following:
#' \describe{
#'    \item{\code{'EXASOL'}}{Syntax of the "Skyline" feature of the commercial database EXASOL EXASolution 5.}
#'    \item{\code{'Preference SQL'} or \code{'PSQL'}}{Syntax of the Preference SQL system. 
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
#' As usual in SQL queries all keywords are not case sensitive, i.e., \code{PLUS} or \code{plus} does not make any difference.
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
 
  if (dialect == "PSQL" || dialect == "PreferenceSQL") dialect = PSQL
  
  # Check dialect
  if (!(dialect %in% c(EXA, PSQL))) stop(paste0('The dialect "', dialect, '" is unknown.'))
  
  # Check for empty -> return empty string
  if (is.empty.pref(p)) return("")
  
  # Return PREFERRING-clause
  return(paste0('PREFERRING ', show_pref_sql(p, dialect, 'PREF', get_static_terms(df)))) # parent node "PREF" means root node
}


#' Partial evaluation and string output of preferences
#' 
#' Functions to substitute variables and functions in preferences which can be calculated before the preference is evaluated on a dataframe.
#' This is especially used for string output of preferences.
#' 
#' @param p The preference to be shown.
#' @param df (Optional) A dataframe on which the preference operates.
#' 
#' @details The function \code{pref.str} (or \code{as.character(p)} for a preference \code{p}) returns the preference string 
#' while \code{show.pref} outputs it directly to the console, preceded by \code{'[Preference]'}.
#' If \code{df} is specified, then a partial evaluation of the preference is done before converting it to a string.
#' 
#' The function \code{eval.pref} (with given \code{df}) partially evaluates the internal preference expression and 
#' returns again a preference object.
#' 
#' The functions \code{show.pref} and \code{pref.str} have the optinal paramter \code{df}.
#' If this paramter is not given these functions are identical to \code{show(p)} and \code{as.character{p}}.
#' These functions do not do any evaluation and just convert the preference terms and expressions to characters.
#' If a dataframe \code{df} is given, then all expressions in \code{p} are evaluated in the environment
#' \code{where} p was defined. Except the the column names in \code{df} (which are potential attributes in \code{p}) 
#' are excluded from the evaluation. The content of the dataframe \code{df} does not matter; 
#' only \code{names(df)} is used to get the "free variables" in \code{p}.
#' 
#' This partial evaluation can be also done via \code{eval.pref}. The following equality hold:
#' 
#' \code{as.character(eval.pref(p, df)) == pref.str(p, df)}
#' 
#' Additionally \code{eval.pref(p, df)} produces the same output on the console as \code{show.pref(p, df)}.
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
#' eval.pref(p, mtcars)
#' 
#' @export
show.pref <- function(p, df = NULL) {
  # Output string representation
  return(cat(paste0('[Preference] ', pref.str(p, df))))
}

#' @export
#' @rdname show.pref
pref.str <- function(p, df = NULL) {
  # Generate string representation
  return(p$get_str(static_terms = get_static_terms(df)))
}

#' @export
#' @rdname show.pref
eval.pref <- function(p, df = NULL) {
  p <- p$copy()
  p$expr <- p$substitute_expr(static_terms = get_static_terms(df))
  return(p)
}

#' @export
#' @rdname show.pref
as.character.preference <- function(p) p$get_str()

#as.character.preference <- function(x, ...) x$get_str()

# Get attributes of a data set as symbols
get_static_terms <- function(df) {
  # Get static terms (all attributes of dataset, if given)
  if (!is.null(df)) return(lapply(names(df), as.symbol))
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
