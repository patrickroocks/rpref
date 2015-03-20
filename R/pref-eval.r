

#' Preference selection
#' 
#' Evaluates a preference on a given dataset, i.e., return the maximal elements of a dataset for a given preference order.
#' 
#' @param df A dataframe or, for a grouped preference selection, a grouped dataframe. See below for details.
#' @param pref The preference order constructed via \code{\link{complex_pref}} and \code{\link{base_pref}}. 
#'             All variables occuring in the definition of \code{pref} must be either columns of the dataframe \code{df} 
#'             or variables/functions of the environment where \code{pref} was defined.
#' @param ... Additional parameters for Top(-Level)-k selections:
#'  \describe{
#'    \item{\code{top}}{A \code{top} value of k means that the k-best tuples of the dataset are returned. 
#'     This may be non-deterministic, see below for details.}
#'     \item{\code{at_least}}{A \code{at_least} value of k returns the Top-k tuples and additionally all tuples which are 
#'     not dominated by the worst tuple (i.e. the minima) of the Top-k set. The number of tuples returned is greater or equal than
#'      \code{at_least}. In contrast to top-k, this is deterministic.}
#'     \item{\code{top_level}}{A \code{top_level} value of k returns all tuples from the k-best levels. See below for the definition of a level.}
#'     \item{\code{and_connected}}{Logical value, which is only relevant if more than one of the above \{\code{top}, \code{at_least}, \code{top_level}\} 
#'     values are given. Then \code{and_connected = TRUE} (which is the default) means that all top-conditions must hold for the returned tuples: 
#'     Let \code{cond1} and \code{cond2} be top-conditions like \code{top=2} or \code{top_level=3}, then
#'     \code{psel([...], cond1, cond2)} is equivalent to the intersection of \code{psel([...], cond1)} and \code{psel([...], cond2)}. If we have
#'     \code{and_connected = FALSE} these conditions are or-connected. 
#'     This corresponds to the union of \code{psel([...], cond1)} and \code{psel([...], cond2)}.}
#'     \item{\code{show_level}}{Logical value. If \code{TRUE}, a column \code{.level} 
#'     is added to the returned data frame, containing all level values (see below for details). 
#'     This is only relevant if at least one of the \{\code{top}, \code{at_least}, \code{top_level}\} values are given. 
#'     For \code{psel} this is \code{TRUE} by default, for \code{psel.indices} this is \code{FALSE} by default.}
#' }
#' 
#' @details
#' 
#' The difference between the two variants of the preference selection is:
#' 
#' \itemize{
#' \item The \code{psel} function returns a subset of the dataset which are the maxima according to the given preference. 
#' \item The function \code{psel.indices} returns just the row indices of the maxima 
#' (except Top-k queries with \code{show_level = TRUE}, see Top-k preference selection).
#' Hence \code{psel(df, pref)} is equivalent to \code{df[psel.indices(df, pref),]} for non-grouped dataframes. 
#' }
#' 
#' @section Top-k preference selection:
#' 
#' For a given \code{top} value "k" the k best elements and their level values are returned. The level values are determined as follows:
#' 
#' \itemize{
#' \item{All the maxima of a dataset w.r.t. a preference have level 1.}
#' \item{The maxima of the remainder, i.e. the dataset without the level 1 maxima, have level 2.}
#' \item{The n-th iteration of "Take the maxima from the remainder" leads to tuples of level n.}
#' }
#' 
#' By default, \code{psel.indices} does not return the level values. By setting \code{show_level = TRUE} this function
#' returns a dataframe with the columns '.indices' and '.level'. 
#' 
#' By definition, a top-k preference selection is non-deterministic. A top-1 query of two equivalent tuples (equivalence according to \code{pref})
#' can return on both of these tuples. E.g., for tuples {(a=1,b=1),(a=1,b=2)} a \code{top=1} selection with a \code{pref=low(a)} preference can return
#' either the 'b=1' or the 'b=2' tuple. 
#' 
#' On the contrary a preference selection with \code{at_least} is deterministic by adding all tuples having the same level as the worst level 
#' of the corresponding top-k query, i.e., which are incomparable to the minimum of the top-k result. 
#' A preference selection with top-level-k returns all tuples having level k or better. 
#'  
#' If the \code{top} or \code{at_least} value is greater than the number of elements in \code{df} 
#' (i.e. \code{nrow(df)}), or \code{top_level} is greater than the highest level in \code{df},
#' then all elements of \code{df} will be returned without further warning.
#' 
#' @section Grouped preference selection:
#' 
#' With \code{psel} it is also possible to perform a preference selection where the maxima are calculated for every group seperately. 
#' The groups have to be created with \code{\link{group_by}} from the dplyr package. The preference selection preserves the grouping, i.e.,
#' the groups are restored after the preference selection.
#' 
#' For example, if the \code{summarize} function from dplyr is applied to
#' \code{psel(group_by(...), pref)}, the summarizing is done for the set of maxima of each group. 
#' This can be used to e.g. calculate the number of maxima in each group, see examples below.
#' 
#' A \{\code{top}, \code{at_least}, \code{top_level}\} preference selection
#' is applied to each group seperately.
#' A \code{top=k} selection returns the k best tuples for each group. 
#' Hence if there are 3 groups in \code{df}, each containing at least 2 elements, 
#' and we have \code{top = 2}, then 6 tuples will be returned.
#' 
#' @section Parallel computation:
#' 
#' On multicore machines the preference selection runs in parellel using a divide-and-conquer approach. 
#' If you prefer a single-threaded computation,
#' use the following code to deactivate parallel compuation within rPref:
#' 
#' \code{options(rPref.parallel = FALSE)}
#' 
#' If this option is not set, rPref will use parallel computation by default.
#' 
#' @seealso See \code{\link{complex_pref}} on how to construct a Skyline preference. 
#' See \code{\link{plot_front}} on how to plot the pareto front of a Skyline.
#' 
#' 
#' @name psel
#' @importFrom dplyr is.grouped_df is.grouped_dt group_by_
#' @export
#' 
#' @examples
#' 
#' # Skyline and Top-K/At-least skyline
#' psel(mtcars, low(mpg) * low(hp))
#' psel(mtcars, low(mpg) * low(hp), top = 5)
#' psel(mtcars, low(mpg) * low(hp), at_least = 5)
#' 
#' # Visualize the skyline in a plot
#' sky1 <- psel(mtcars, high(mpg) * high(hp))
#' plot(mtcars$mpg, mtcars$hp)
#' points(sky1$mpg, sky1$hp, lwd=3)
#' 
#' # Grouped preference with dplyr
#' library(dplyr)
#' psel(group_by(mtcars, cyl), low(mpg))
#' 
#' # Return size of each maxima group
#' summarise(psel(group_by(mtcars, cyl), low(mpg)), n())
#' 
psel <- function(df, pref, ...) {  
  
  vars = list(...)
  
  # Check if top-k query
  is_top <- any(names(vars) %in% c('top', 'at_least', 'top_level'))
  
  # Store actual show_level value
  show_level <- is_top && (is.null(vars$show_level) || isTRUE(vars$show_level)) # FALSE by Default
  vars$show_level <- show_level
  
  # Call psel.indices with all parameters from ...
  tres <- psel.indices(df, pref, .dots = vars)
  
  # Extract indices
  if (!show_level) indices <- tres # psel.indices.top returned just indices when show_level = FALSE
  else indices <- tres[['.indices']]
  
  # Preference subset
  if (!dplyr::is.grouped_df(df)) res <- df[indices,,drop=FALSE] # Usual preference selection
  else res <- dplyr::group_by_(df[indices,,drop=FALSE], .dots = attr(df, 'vars')) # Grouped preference selection - regroup!
  
  # Add levels if show_level is true
  if (!show_level) return(res)
  else {
    res[['.level']] <- tres[['.level']]
    return(res)
  }
  
  # Do the preference selection
  if (!dplyr::is.grouped_df(df)) return(df[res,]) # Usual preference selection
  else return(dplyr::group_by_(df[res,], .dots = attr(df, 'vars'))) # Grouped preference selection - regroup!
}



#' @export
#' @rdname psel
#' @importFrom dplyr is.grouped_df is.grouped_dt group_by_
#' @importFrom RcppParallel defaultNumThreads
psel.indices <- function(df, pref, ...) {
  
  if (!is.data.frame(df)) stop("Preference selection needs a data.frame as input.")
  
  # Check if grouped data.table (currently not implemented, only grouped dataframes work fine)
  if (dplyr::is.grouped_dt(df)) {
    df <- group_by_(as.data.frame(df), .dots = attr(df, 'vars'))
    warning("Grouped preference selection is only possible for grouped dataframes. The input was converted to a grouped dataframe.")
  }
    
  # ** Get all special arguments for top-k selections
  
  vars <- list(...)
  
  # Get all variables from '...' in psel
  if ('.dots' %in% names(vars)) vars <- vars[['.dots']] 
    
  is_top <- any(c('top', 'at_least', 'top_level') %in% names(vars))
  
  if (is_top) { 
    
    # Logical options
    and_connected <- isTRUE(vars[['and_connected']]) || is.null(vars[['and_connected']])  # TRUE by default
    show_level    <- isTRUE(vars[['show_level']]) # FALSE by Default (for psel.indices)
    
    # No defaults, but value -1 if null
    top       <- check.singleint.null2minus(vars[['top']],       'top')
    top_level <- check.singleint.null2minus(vars[['top_level']], 'top_level')
    at_least  <- check.singleint.null2minus(vars[['at_least']],  'at_least')
    
    # Check if all "-1" (were NULL before) - no valid top-k selection!
    if (all(c(top, show_level, at_least) == -1)) stop('The parameters top, top_level, at_least must not be all NULL!')
  }
  
  # ** Check for additional (wrong) arguments
  
  unused_names = setdiff(names(vars), c('top', 'at_least', 'top_level', 'and_connected', 'show_level', '.dots', ''))
  
  if (!is.null(unused_names) && length(unused_names) > 0)
    warning(paste0('The following arguments passed to psel are no preference selection parameters and will be ignored: ', paste(unused_names, collapse = ', ')))

  if ((length(vars) > 0 && is.null(names(vars))) || ('' %in% names(vars)))
    warning("Unnamed arguments were passed to '...' in psel. They will be ignored.")
  
  # ** Calculcalate score/serial pref
 
  # Precalculate score values for given preference, get_corevals must be called before serialize!
  scores <- pref$get_scorevals(1, df)$scores
  pref_serial <- pref$serialize()
  
  # ** Get options
  
  # Get alpha value, default is 10
  alpha <- getOption("rPref.scalagon.alpha", default = 10)
  
  # Use parallel computation? Default is TRUE!
  if (isTRUE(getOption("rPref.parallel", default = TRUE)))
    # Get number of threads vom RcppParallel for parallelization
    Npar <- RcppParallel::defaultNumThreads()
  else
    Npar <- 1
  
  # ** Finally do the (top-k) preference selection
  
  if (!is_top) {
    # Do the preference selection - not-top-k
    if (!dplyr::is.grouped_df(df)) { # Usual preference selection (not grouped)
      res <- pref_select_impl(scores, pref_serial, Npar, alpha) # non parallel for Npar=1
    } else { # Grouped preference selection
      res <- grouped_pref_sel_impl(df, scores, pref_serial, Npar, alpha) 
    }
    
    # All C indices start at 0, and all R indices start at 1
    return(res + 1)
    
  } else {
    
    # Do the top-k preference selection
    if (!is.grouped_df(df)) { # Usual preference selection (not grouped)
      res <- pref_select_top_impl(scores, pref_serial, Npar, alpha, 
                                  top, at_least, top_level, and_connected, show_level) 
    } else { # Grouped preference selection
      res <- grouped_pref_sel_top_impl(df, scores, pref_serial, Npar, alpha,
                                       top, at_least, top_level, and_connected, show_level) 
    }
    
    # All C indices start at 0, and all R indices start at 1
    res[['.indices']] <- res[['.indices']] + 1
    
    if (!show_level) return(res[['.indices']]) # Return just indices
    return(res) # Return Data.Frame(".indices", ".level")
  }
}


# Helper
check.singleint.null2minus <- function(val, name) {
  if (is.null(val)) 
    return(-1)
  else if (!(is.numeric(val) && length(val) == 1 && round(val) == val && val >= 0))
    stop(paste0('Error in preference selection: Parameter ', name, ' must be a positive single integer value'), call. = FALSE)
  else
    return(val)
}

