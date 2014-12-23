
#' Complex preferences
#' 
#' Complex preferences are used to compose different preferences orders. For example the Pareto composition (via operator \code{*}) is the usual operator
#' to compose the preference for a Skyline query. The Skyline is also known as Pareto frontier.
#' All complex preferences are mathematically strict partial orders (irreflexive and transitive).
#' 
#' @name complex_pref
#' @param p1,p2 Preferences to be composed (either base preferences via \code{\link{base_pref}} or also complex preferences)
#' 
#' @section Skylines:
#' 
#' The most important preference composition operator is the Pareto operator (\code{p1 * p2}) to formulate a Skyline query. 
#' A tuple t1 is better than t2 w.r.t. \code{p1 * p2} if it is strictly better w.r.t. one of the preferences p1, p2 and is better or equal w.r.t. the other preference.
#' 
#' The syntactical translation from other query languages supporting skylines/Preferences to rPref is as follows:
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

  # Create new preference function according to the Pareto Definition
  return(complexpref(p1, p2, '*',
                     (p1$cmp | p1$eq) & p2$cmp |
                     (p2$cmp | p2$eq) & p1$cmp,
                      p1$eq & p2$eq))
}

# Infix Prioritization-Constructor (special constructor as we have to consider prior-chains!)
#' @rdname complex_pref
#' @export
"&.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  
  return(priorpref(p1, p2, '&',
                   p1$cmp | p1$eq & p2$cmp,
                   p1$eq & p2$eq))
}


# Infix Intersection-Constructor
#' @rdname complex_pref
#' @export
"|.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  
  return(complexpref(p1, p2, '|',
                     p1$cmp & p2$cmp,
                     p1$eq  & p2$eq))
}

# Infix-Disjount-Union-Constructor
#' @rdname complex_pref
#' @export
"+.preference" <- function(p1, p2) {
  check_pref(p1, p2)
  if (check_empty(p1, p2)) return(get_empty(p1, p2))
  
  return(complexpref(p1, p2, '+',
                     p1$cmp | p2$cmp,
                     p1$eq  & p2$eq))
}


# Reverse preference
#' @rdname complex_pref
#' @export
reverse <- function(p1) {
  check_pref(p1)
  if (is.empty.pref(p1)) return(p1)
  return(reversepref(p1))
}

#' @rdname complex_pref
#' @export
"-.preference" <- function(p1, p2) {
  if (nargs() == 1) return(reverse(p1))
  else stop("Operation not defined")
}

# Neutral element
#' @rdname complex_pref
#' @export
empty <- function() empty.pref()
 
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


# Overload arithmetic operators for arbitrary functions - used for the composition of complex preferences
"Ops.function" = function(e1, e2) {
  res <- function(...) .Primitive(.Generic)(e1(...), e2(...))
  # Equip this function with the function class attribute (to ensure that Ops.function cares about it!) and return it
  class(res) <- "function"
  return(res)
}
