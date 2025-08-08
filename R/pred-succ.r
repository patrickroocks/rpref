#' Predecessor and Successor Functions
#'
#' Functions for traversing the BTG (Better-Than-Graph or Hasse diagram) of a preference.
#'
#' @name pred_succ
#' @param df (Optional) A data frame or data frame extension (e.g., a tibble) characterizing the set wherein predecessors/successors are searched.
#'        If \code{df} is \code{NULL} then the data frame associated with the preference is used.
#'        Causes an error if \code{df == NULL} and no data frame is associated.
#' @param v A numeric vector of indices in \code{df}. This represents the set of tuples for which predecessors/successors are searched.
#' @param p A preference. Worse tuples in the induced order are successors and better tuples are predecessors.
#' @param intersect (Optional) Logical value.
#'        If this is \code{FALSE} (by default) the union of all predecessors/successors of \code{v} is returned.
#'        For \code{intersect = TRUE} the intersection of those values is returned.
#'
#' @details
#'
#' These functions return the predecessors and successors in the Better-Than-Graph of a preference.
#' Note that the successors/predecessors can can be plotted via \code{\link{get_btg}}.
#' Before any of the successor/predecessor functions can be used the initialization has to be called as follows:
#'
#' \code{init_pred_succ(p, df)}
#'
#' There \code{p} is a preference object and \code{df} a data frame.
#' When this done, the data frame \code{df} is associated with \code{p}, i.e.,
#' implicitly \code{\link{assoc.df}} is called.
#' If the preference has already an associated data frame, \code{df} can be omitted. For example
#'
#' \code{p <- low(mpg, df = mtcars)} \cr
#' \code{init_pred_succ(p)}
#'
#' does the initialization of the preference \code{low(mpg)} on the data set \code{mtcars}.
#'
#' The \code{init_pred_succ} function calculates the Better-Than-Relation on \code{df} w.r.t. \code{p}.
#' Afterwards the predecessor and successor functions, as subsequently described, can be called.
#' The value of \code{v} is a numeric vector within \code{1:nrow(df)}
#' and characterizes a subset of tuples in \code{df}.
#' The return value of these functions is again a numeric vector referring to the row numbers in \code{df}
#' and it is always ordered ascending, independently of the order of the indices in \code{v}.
#'
#'
#' \describe{
#'   \item{\code{all_pred(p, v)}}{Returns all predecessors of \code{v}, i.e., indices of better tuples than \code{v}.}
#'   \item{\code{all_succ(p, v)}}{Returns all successors of \code{v}, i.e., indices of worse tuples than \code{v}.}
#'   \item{\code{hasse_pred(p, v)}}{Returns the direct predecessors of \code{v},
#'         i.e., indices of better tuples than \code{v} where the better-than-relation is contained in the transitive reduction.}
#'   \item{\code{hasse_succ(p, v)}}{Returns the direct successors of \code{v},
#'         i.e., indices of worse tuples than \code{v} where the better-than-relation is contained in the transitive reduction.}
#' }
#'
#' If \code{v} has length 1, then the value of \code{intersect} does not matter, as there is nothing to intersect or join.
#' For scalar values \code{x} and \code{y} the following identities hold, where \code{f} is one of the predecessor/successor functions:
#'
#' \code{f(p, c(x, y), intersect = FALSE) == union(f(p, x), f(p, y))}
#'
#' \code{f(p, c(x, y), intersect = TRUE) == intersect(f(p, x), f(p, y))}
#'
#'
#' @examples
#'
#' # Preference on mtcars for high mpg and low weight
#' p <- high(mpg) * low(wt)
#' init_pred_succ(p, mtcars)
#'
#' # Helper to show mpg/hp values
#' show_vals <- function(x) mtcars[x, c("mpg", "wt")]
#'
#' # Pick some tuple "in the middle":
#' show_vals(10)
#'
#' # Show (direct) predecessors/successors of tuple 10:
#' show_vals(hasse_pred(p, 10)) # Next better car
#' show_vals(hasse_succ(p, 10)) # Next worse car
#' show_vals(all_pred(p, 10)) # All better cars
#' show_vals(all_succ(p, 10)) # All worse cars
NULL


#' @rdname pred_succ
#' @export
init_pred_succ <- function(p, df = NULL) {
  # For compatibility with rPref <= 1.0 where we had init_pred_succ(df, p)
  if (is.preference(df) && !is.preference(p)) {
    warning('Wrong order of arguments in "init_pred_succ(p, df)". This has changed in rPref >= 1.1')
    parent_var <- substitute(df)
    p <- init_pred_succ_internal(df, p, substitute(p))
  } else {
    # For the usual call of this function in rPref >= 1.1
    parent_var <- substitute(p)
    p <- init_pred_succ_internal(p, df, substitute(df))
  }
  # Returns nothing but modifies parent preference object
  # One of the ugly hacks which were needed since we changed from reference classes to S4
  if (!is.symbol(parent_var)) {
    stop.syscall("init_pred_succ has to called on a preference stored in a variable.")
  }
  assign(as.character(parent_var), p, parent.frame())
}


#' @rdname pred_succ
#' @export
hasse_pred <- function(p, v, intersect = FALSE) {
  h_predsucc(p, v, do_intersect = intersect, succ = FALSE)
}

#' @rdname pred_succ
#' @export
hasse_succ <- function(p, v, intersect = FALSE) {
  h_predsucc(p, v, do_intersect = intersect, succ = TRUE)
}

#' @rdname pred_succ
#' @export
all_pred <- function(p, v, intersect = FALSE) {
  all_predsucc(p, v, do_intersect = intersect, succ = FALSE)
}

#' @rdname pred_succ
#' @export
all_succ <- function(p, v, intersect = FALSE) {
  all_predsucc(p, v, do_intersect = intersect, succ = TRUE)
}


# Internal implementation
# -----------------------

init_pred_succ_internal <- function(p, df, df_call) {
  pref.df.check(p, df)

  # Overwrite associated data frame, do partial evaluation
  if (!is.null(df)) {
    p@df_src <- compose.df(df, df_call)
    p <- evaluate(p, get_static_terms(p@df_src$df))
  } else if (length(p@df_src) == 0) stop("No data source was specified!")

  # Use specified data.frame from now on
  res <- get_scores(p, 1, p@df_src$df)
  # We need the modified preference object for cmp/eq
  p <- res$p
  # Cached scorevals
  p@scorevals <- res$scores
  serialized <- pserialize(p)
  # Get Hasse Matrix from C++ function, add 1 for R indices (starting at 1)
  p@hasse_mtx <- t(get_hasse_impl(p@scorevals, serialized)) + 1
  p@cache_available <- TRUE

  # returns modified preference
  return(p)
}

# Hasse diagram successors/predecessors
h_predsucc <- function(p, inds, do_intersect, succ) {
  pref.df.check(p)
  check_cache(p)

  # Select indices predecessors/successors
  if (succ) {
    l_index <- 1
    r_index <- 2
  } else {
    l_index <- 2
    r_index <- 1
  }

  # Selection aggregation method, if inds is a vector (and not a single value)
  if (do_intersect) {
    agg <- intersect
  } else {
    agg <- union
  }

  # Do the calculation
  if (length(inds) == 0) {
    return(numeric(0))
  } else if (length(inds) == 1) {
    return(p@hasse_mtx[p@hasse_mtx[, l_index] == inds, r_index])
  } else {
    res_inds <- lapply(inds, function(x) p@hasse_mtx[p@hasse_mtx[, l_index] == x, r_index])
    return(sort(Reduce(agg, res_inds[-1], res_inds[[1]])))
  }
}

# All succesors/predecessors (sorting not necessary because of "which")
all_predsucc <- function(p, inds, do_intersect, succ) {
  pref.df.check(p)
  check_cache(p)
  # does not need hasse diagram, but needs p@scorevals

  # Select indices predecessors/successors
  if (succ) {
    cmpfun <- function(x, y) cmp(p, x, y, p@scorevals)
  } else {
    cmpfun <- function(x, y) cmp(p, y, x, p@scorevals)
  }

  # Selection aggregation method, if inds is a vector (and not a single value)
  if (do_intersect) {
    agg <- pmin
  } # min is logically equivalent to intersection
  else {
    agg <- pmax
  } # max is logically equivalent to union

  all_inds <- 1:nrow(p@scorevals)
  if (length(inds) == 0) {
    return(numeric(0))
  } else if (length(inds) == 1) {
    return(which(cmpfun(inds, all_inds)))
  } else {
    return(which(as.logical(do.call(agg, lapply(inds, function(x) cmpfun(x, all_inds))))))
  }
}

check_cache <- function(p) {
  if (!p@cache_available) {
    stop.syscall(paste0(
      "In calculation of predecessors/successors : No data set avaible. ",
      "Run `init_succ_pref(df)` or `assoc.df(p) <- df` first."
    ))
  }
}

pref.df.check <- function(pref, df = NULL) {
  # Note that also data.table fulfills is.data.frame(.) = TRUE
  if (!is.actual.preference(pref)) stop.syscall("First argument has to be a preference.")
  if (!is.null(df) && !is.data.frame(df)) stop.syscall("Second argument has to be a data.frame or NULL.")
}
