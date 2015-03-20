
#' Complex preferences
#' 
#' Complex preferences are used to compose different preferences orders. For example the Pareto composition (via operator \code{*}) is the usual operator
#' to compose the preference for a Skyline query. The Skyline is also known as Pareto frontier.
#' All complex preferences are mathematically strict partial orders (irreflexive and transitive).
#' 
#' @name complex_pref
#' @param p,p1,p2,x Preferences (they can be either base preferences, see \code{\link{base_pref}}, or complex preferences)
#' 
#' @section Skylines:
#' 
#' The most important preference composition operator is the Pareto operator (\code{p1 * p2}) to formulate a Skyline query. 
#' A tuple t1 is better than t2 w.r.t. \code{p1 * p2} if it is strictly better w.r.t. one of the preferences p1, p2 and is better or equal w.r.t. the other preference.
#' 
#' The syntactical translation from other query languages supporting Skylines/Preferences to rPref is as follows:
#' 
#' \itemize{
#' \item A query in the syntax from Borzsonyi et. al (2001) like
#' 
#' "\code{... SKYLINE OF a MAX, b MIN, c MAX}" 
#' 
#' corresponds in rPref to the preference
#' 
#' \code{high(a) * low(b) * high(c)}.
#' 
#' 
#' \item A query in the syntax from Kiessling (2002) like 
#' 
#' "\code{... PREFERRING a LOWEST AND (b HIGHEST PRIOR TO c LOWEST)}" 
#' 
#' corresponds in rPref to 
#' 
#' \code{low(a) * (high(b) & low(c))}.
#' 
#' 
#' \item A query in the syntax of the "Skyline" feature of the commercial database "EXASOL EXASolution 5" like 
#' 
#' "\code{... PREFERRING LOW a PLUS (b = 1 PRIOR TO LOW c))}" 
#' 
#' corresponds in rPref to
#' 
#' \code{low(a) * (true(b == 1) & low(c))}.
#' 
#' }
#' 
#' Note that these query conversions can be done by \code{\link{show.query}}. 
#' 
#' @section Definition of additional preference operators:
#' 
#' Additionally, rPref supports the following preference composition operators:
#' 
#' \describe{
#'  \item{\code{p1 & p2}}{Prioritization (lexicographical order): A tuple t1 is better than t2 w.r.t. \code{p1 & p2} if it is 
#'   strictly better w.r.t. \code{p1} or is equal w.r.t. \code{p1} and is better w.r.t. \code{p2}.}
#'  \item{\code{p1 | p2}}{Intersection preference: A tuple t1 is better than t2 w.r.t. \code{p1 | p2} if it is strictly better w.r.t. both preferences. 
#'        This is a stricter variant of the Pareto operator. The evaluation of \code{psel(df, p1 | p2)} is always a subset of \code{psel(df, p1 * p2)}.}
#'  \item{\code{p1 + p2}}{Union preference: A tuple t1 is better than t2 w.r.t. \code{p1 + p2} if it is strictly better w.r.t. to one of the preferences. 
#'  Note that this can violate the strict partial order property, if the domains (the tuples on which \code{p1} and \code{p2} define better-than-relationships) 
#'  of the preferences are not disjoint.}
#'  \item{\code{reverse(p1)} or \code{-p1}}{Reverse preference (converse relation): 
#'  A tuple t1 is better than t2 w.r.t. \code{-p1} if t2 is better than t1 w.r.t. \code{p1}. 
#'  The unary minus operator, i.e. \code{-p1}, is a short notation for \code{reverse(p1)}.}
#'  \item{\code{empty()}}{Empty preference, i.e. a neutral element for all complex preference compositions. 
#'  It holds that \code{op(empty(), p)} is equal to \code{p} for all preference operators \code{op} and all preferences \code{p}.}
#' }
#' 
#' @section Preference term length:
#' 
#' The function \code{length(p)} gives the term length of the preference term \code{p} which is defined as the number of base preferences
#' in a complex preference term.
#'
#' @seealso See \code{\link{base_pref}} for the construction of base preferences. See \code{\link{psel}} for the evaluation of preferences. 
#' 
#' @keywords skyline
#' 
#' @references 
#' 
#' S. Borzsonyi, D. Kossmann, K. Stocker (2001): The Skyline Operator. In Data Engineering (ICDE '01), pages 421-430.
#' 
#' W. Kiessling (2002): Foundations of Preferences in Database Systems. In Very Large Data Bases (VLDB '02), pages 311-322.
#' 
#' S. Mandl, O. Kozachuk, M. Endres, W. Kiessling (2015): Preference Analytics in EXASolution. 
#' 16th Conference on Database Systems for Business, Technology, and Web.
#' 
#' @examples
#' # Define preference for cars with low consumption (high mpg-value) 
#' # and simultanously high horsepower
#' p1 <- high(mpg) * high(hp)  
#' 
#' # Perform the preference search
#' psel(mtcars, p1)
NULL


# Complex preference Construtors
# ------------------------------

# Score functions are NOT needed for C++ algorithms but for igraph, etc.

# Infix Pareto-Constructor
#' @rdname complex_pref
#' @export
"*.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  return(paretopref(p1, p2))
}

# Infix Prioritization-Constructor (special constructor as we have to consider prior-chains!)
#' @rdname complex_pref
#' @export
"&.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  return(priorpref(p1, p2))
}


# Infix Intersection-Constructor
#' @rdname complex_pref
#' @export
"|.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  return(intersectionpref(p1, p2))
}

# Infix-Disjount-Union-Constructor
#' @rdname complex_pref
#' @export
"+.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  return(unionpref(p1, p2))
}


# Reverse preference
#' @rdname complex_pref
#' @export
reverse <- function(p) {
  check_pref(p)
  if (is.empty.pref(p)) return(p)
  return(reversepref(p))
}

# This entry will be deleted in the final man-files as "-" is just used unary!
#' @rdname complex_pref
#' @export
"-.preference" <- function(p1, p2) {
  if (nargs() == 1) return(reverse(p1))
  else stop("Operation not defined.")
}

# Neutral element
#' @rdname complex_pref
#' @export
empty <- function() empty.pref()

# Length of a preference term (number of base preferences)
#' @export
#' @rdname complex_pref
length.preference <- function(x) x$get_length()

 
# Helper functions
# ----------------
  
# Helper to check if all given arguments are preferences
check_pref <- function(p1, p2) {
  if (!is.preference(p1) || (nargs() == 2 && !is.preference(p2)))
    stop("This operator requires preference objects as input.")
}

# Check if one (or perhaps both) preference is empty
check_empty <- function(p1, p2) (is.empty.pref(p1) || is.empty.pref(p2))

# Get the result of a complex operation with an empty pref
get_empty <- function(p1, p2) {
  if (is.empty.pref(p1))
    return(p2) # perhaps empty
  else
    return(p1)
}    
