
#' Utility Functions for Preferences
#' 
#' Collection of some useful functions which are applicable to all preference objects.
#' 
#' @name general_pref
#' 
#' @param x A preference, or, for \code{is.preference}, an object to be tested if it is an (empty) preference.
#' @param df A data frame to associate with a preference object.
#' @param ... Optional arguments passed to \code{as.character} and \code{as.expression}.
#' 
#' @details 
#' 
#' The empty preference \code{empty()} is a neutral element for the complex preference compositions \code{{*, &, +}}. 
#' It holds that \code{empty() * p} and \code{empty() & p} is equal to \code{p} for all preferences \code{p}.
#' 
#' The function \code{length(p)} returns the term length of the preference term \code{p}
#' which is defined as the number of base preferences
#' in a complex preference term. The empty preference \code{empty()} has length 0, 
#' and all base preferences have length 1.
#' 
#' With \code{as.expression(p)} for a preference \code{p} the call to the preference is constructed. 
#' This means, \code{eval(as.expression(p))} returns the preference \code{p}, evaluated in the current environment.
#' 
#' The function \code{is.empty_pref} returns \code{TRUE} if \code{x} is the empty preference object 
#' \code{empty()} and \code{FALSE} otherwise.
#' 
#' With \code{set.df} a preference is associated with a data frame.
#' This means, a partial evaluation based on this data frame is done. 
#' See \code{\link{show.pref}} for details on partial evaluation of preferences.
#' Next, the preference is linked to that data frame, such that \code{\link{peval}(p)} can be used instead of \code{psel(df, p)}.
#' The function \code{get.df} is the inverse to \code{set.df} and returns the associated data frame.
#' It returns \code{NULL} if no data frame is associated.
#' 
#' @seealso See \code{\link{base_pref}} for the construction of base preferences,
#' and \code{\link{complex_pref}} for the construction of complex preferences. 
#' See \code{\link{show.pref}} for a partial evaluation of preference terms.
#' 
#' @examples 
#' 
#' # Same as low(a) * low(b)
#' p <- low(a) * low(b) * empty()
#' 
#' # returns 2, as empty() does not count
#' length(p)
#' 
#' # the preference expression (without empty())
#' as.expression(p)
#' 
NULL



# Neutral element
#' @rdname general_pref
#' @export
empty <- function() emptypref$new()

# Check for empty preference
#' @rdname general_pref
#' @export
is.empty_pref <- function(x) {
  inherits(x, "emptypref")
}

# Length of a preference term (number of base preferences)
#' @export
#' @rdname general_pref
length.preference <- function(x) {
  x$get_length()
}

#' @rdname general_pref
#' @export
is.preference <- function(x) {
  inherits(x, "preference")
}
  
#' @export
#' @rdname general_pref
as.expression.preference <- function(x, ...) {
  methods::callNextMethod()
}

#' @export
#' @rdname general_pref
as.character.preference <- function(x, ...) {
  x$get_str()
}



#' @export
#' @rdname general_pref
set.df <- function(x, df) {
  x$set_df_src(dataframe_src$new(df, substitute(df)))
}

#' @export
#' @rdname general_pref
get.df <- function(x) {
  x$df_src$df
}

