
#' Utility Functions for Preferences
#' 
#' Collection of some useful functions which are applicable to all preference objects.
#' 
#' @name general_pref
#' 
#' @param x A preference, or, for \code{is.preference}, an object to be tested if it is an (empty) preference.
#' @param value A data frame to associate with a preference object.
#' @param ... Optional arguments passed to \code{as.expression}.
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
#' With \code{assoc.df} the associated data frame of a preference can be retrieved or set.
#' Setting the associated data frame means that a partial evaluation based on this data frame is done. 
#' See \code{\link{show.pref}} for details on partial evaluation of preferences.
#' Next, the preference is linked to that data frame, such that \code{\link{peval}(p)} can be used instead of \code{psel(df, p)}.
#' It returns \code{NULL} if no data frame is associated.
#' Use \code{set.assoc.df(NULL)} to delete an associated data frame.
#' 
#' @seealso See \code{\link{base_pref}} for the construction of base preferences,
#' and \code{\link{complex_pref}} for the construction of complex preferences. 
#' See \code{\link{show.pref}} for string output and partial evaluation of preference terms.
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
empty <- function() methods::new("emptypref")

# Check for empty preference
#' @rdname general_pref
#' @export
is.empty_pref <- function(x) {
  inherits(x, "emptypref")
}

# Length of a preference term (number of base preferences)
#' @export
#' @rdname general_pref
#' @aliases length,preference-method length,basepref-method length,emptypref-method length,complexpref-method length,reversepref-method
#' @docType methods
setMethod("length", signature(x = "preference"),
  function(x) {
    methods::callNextMethod()
  }
)

#' @rdname general_pref
#' @export
is.preference <- function(x) {
  # intentionally not a generic "is" method
  inherits(x, "preference")
}

#' @export
#' @rdname general_pref
#' @aliases as.expression,preference-method as.expression,basepref-method as.expression,emptypref-method as.expression,complexpref-method as.expression,reversepref-method
#' @docType methods
setMethod("as.expression", signature(x = "preference"),
  function(x, ...) {
    methods::callNextMethod()
  }
)

setGeneric("assoc.df<-", function(x, value) standardGeneric("assoc.df<-"))
setGeneric("assoc.df",   function(x)        standardGeneric("assoc.df"))

# how to document s4 methods:
# http://stackoverflow.com/questions/4396768/how-to-properly-document-s4-and-methods-using-roxygen


#' @export
#' @rdname general_pref
#' @name assoc.df
#' @aliases assoc.df,preference-method
#' @docType methods
setMethod("assoc.df", signature = "preference",
  function(x) {
    return(x@df_src$df)
})


#' @export
#' @rdname general_pref
#' @name assoc.df<-
#' @aliases assoc.df<-,preference-method
#' @docType methods
setReplaceMethod("assoc.df", signature = "preference",
  function(x, value) {
    # this does not modify the parent frame variable (like init_pred_succ does)
    # instead a new (evaled!) preference object is returned
    # assocComposedDf and compose.df are defined in base_pref
    x <- assoc.composed.df(x, compose.df(value, substitute(value)))
    return(x)
})

