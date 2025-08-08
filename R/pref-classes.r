
# All functions are internal!

# General preference (only for internal use)
preference <- setClass("preference",
  slots = c(
    # list containing "df" and "info_str", list() means "not set"
    # accessed via constructor and assoc.df
    df_src = "list", 
    # this is set in get_scores
    score_id = "numeric",
    # These are used by init_pred_succ in pred-succ.r
    cache_available = "logical",
    hasse_mtx = "matrix",
    scorevals = "data.frame"
  ),
  prototype = list(
    # also used for string output (as.character)
    cache_available = FALSE
  ),
  contains = "VIRTUAL"
)

setMethod("show", signature(object = "preference"),
  # not explicitly exported - called implicitly
  # show.pref(p, df) is exported, see show-pref
  function(object) {
    cat('[Preference] ', as.character(object), '\n', sep = "")
    if (length(object@df_src) > 0)
      cat('  * associated data source: ', object@df_src$info_str, '\n', sep = "")
    if (object@cache_available)
      cat('  * Hasse matrix for predecessors/successors available')
  }  
)

setGeneric(name = "cmp",         def = function(object, i, j, score_df) {standardGeneric("cmp")} )
setGeneric(name = "eq",          def = function(object, i, j, score_df) {standardGeneric("eq")} )
setGeneric(name = "pserialize",  def = function(object)                 {standardGeneric("pserialize")} )
setGeneric(name = "evaluate",    def = function(object, static_terms)   {standardGeneric("evaluate")} )
setGeneric(name = "calc_scores", def = function(object, df, frm)        {standardGeneric("calc_scores")} )
setGeneric(name = "get_scores",  def = function(object, next_id, df)    {standardGeneric("get_scores")} )

is.preference <- function(x) inherits(x, "preference")

# Empty preference
# ----------------

# Special empty preference class. All scores are 0
emptypref <- setClass("emptypref",
  slots = character(0),
  prototype = list(),
  contains = "preference"
)

setMethod("as.character", signature(x = "emptypref"),
  function(x, parent_op = "") {
    return("(empty)")
  }
)

#' @export
setMethod("as.expression", signature(x = "emptypref"),
  function(x, ...) {
    return(expression(empty()))
  }
)

setMethod("get_scores", signature(object = "emptypref"),
  function(object, next_id, df) {
    return(list(p = object, next_id = next_id + 1, scores = as.data.frame(rep(0, nrow(df)))))
  }
)

setMethod("length", signature(x = "emptypref"),
  function(x) {
    return(0)
  }
)

setMethod("cmp", signature(object = "emptypref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return(FALSE)
  }
)

setMethod("eq", signature(object = "emptypref"),
  function(object, i, j, score_df) { # TRUE if i is equal to j
    return(TRUE)
  }
)

setMethod("pserialize", signature(object = "emptypref"),
  function(object) {
    return(list(kind = 's'))
  }
)

# Nothing to do for empty preference
setMethod("evaluate", signature(object = "emptypref"),
  function(object, static_terms) {
    return(object)
  } 
)

## @importClassesFrom lazyeval lazy

# cmp/eq functions are not needed for C++ BNL algorithms, but for igraph, etc.

# Base preferences 
setOldClass("lazy")
basepref <- setClass("basepref",
  slots = c(
    lazy_expr = "lazy",
    op = "character"
  ),
  prototype = list(),
  contains = "preference"
)

setMethod("initialize", "basepref",
  function(.Object, lazy_expr) {
    .Object@lazy_expr <- lazy_expr
    return(.Object)
  }
)

setMethod("get_scores", signature(object = "basepref"),
  function(object, next_id, df) {
    object@score_id <- next_id
    # Add data.frame to the environment without modifying original environment
    frm <- new.env(parent = object@lazy_expr$env)
    assign("df__", df, pos = frm)
    
    # Calc score of base preference
    # calc_score is defined in low/high/true preferences
    scores <- calc_scores(object, df, frm)
    
    # Check if length of score vector is ok 
    # (length 1 might happen; has to be appropriately extended)
    if (length(scores) != nrow(df)) {
      if (length(scores) == 1) scores <- rep(scores, nrow(df))
      else stop(paste0("Evaluation of base preference ", as.character(object), 
                       " does not have the same length as the data frame!"))
    }
    # Increase next_id after base preference
    return(list(p = object, next_id = next_id + 1, scores = as.data.frame(scores)))
  }
)

setMethod("length", signature(x = "basepref"),
  function(x) {
    return(1)
  }
)

setMethod("cmp", signature(object = "basepref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return(score_df[i, object@score_id] < score_df[j, object@score_id])
  }
)

setMethod("eq", signature(object = "basepref"),
  function(object, i, j, score_df) { # TRUE if i is equal to j
    return(score_df[i, object@score_id] == score_df[j, object@score_id])
  }
)


setMethod("as.character", signature(x = "basepref"),
  function(x, parent_op = "") {
    return(paste0(x@op, '(', as.character(as.expression(x@lazy_expr$expr)), ')'))
  }
)
    
setMethod("pserialize", signature(object = "basepref"),
  function(object) {
    return(list(kind = 's'))
  }
)

setMethod("evaluate", signature(object = "basepref"),
  function(object, static_terms) {
    # get_inner_expr is not generic, only used for baseprefs!
    object@lazy_expr$expr <- get_inner_expr(object, static_terms)
    return(object)
  }
)    


#' @export
setMethod("as.expression", signature(x = "basepref"),
  function(x, ...) {
    # x is a base preference object, where the constructor name is given by x@op
    args <- list(...)
    # get_expr_evaled returns a Call
    if (!is.null(args[['static_terms']])) {
      expr <- get_inner_expr(x, args[['static_terms']])
    } else {
      expr <- x@lazy_expr$expr
    }
    # this has to return an expression!
    return(as.expression(call(x@op, expr))) 
  }
)

lowpref <- setClass("lowpref",
  prototype = list(op = "low"),
  contains = "basepref"
)

setMethod("calc_scores", signature(object = "lowpref"),
  function(object, df, frm) {
    # frm is from lazy_eval object, augmented by df__, do not use lazy_eval
    res <- eval(object@lazy_expr$expr, df, frm) 
    if (!is.numeric(res)) stop("For the low preference ", as.character(object), " the expression must be numeric!")
    return(res)        
  }
)


is.lowpref <- function(x) inherits(x, "lowpref")

highpref <- setClass("highpref",
  prototype = list(op = "high"),
  contains = "basepref"
)

setMethod("calc_scores", signature(object = "highpref"),
  function(object, df, frm) {
    res <- eval(object@lazy_expr$expr, df, frm) 
    if (!is.numeric(res)) stop("For the high preference ", as.character(object), " the expression must be numeric!")
    return(-res)       
  }
)

is.highpref <- function(x) inherits(x, "highpref")

truepref <- setClass("truepref",
  prototype = list(op = "true"),
  contains = "basepref"
)

setMethod("initialize", "truepref",
  function(.Object, ...) {
    .Object <- methods::callNextMethod(.Object, ...)
    if (isTRUE(getOption("rPref.checkLogicalAndOr", default = TRUE))) {
      chr_expr <- as.character(as.expression(.Object@lazy_expr$expr))
      if (length(chr_expr > 0) && grepl(" \\|\\||\\&\\& ", chr_expr))
        warning(paste0("In true(...) :\n", 
                 "Detected logical operator '&&' or '||' in logical expreesion -- possibly unexpected behavior.\n",
                 "Probably you intended '&' or '|'?\n",
                 "Set \n'options(rPref.checkLogicalAndOr = FALSE)' \nto disable this warning."),
                 call. = FALSE)
    }
    return(.Object)
  }
)

setMethod("calc_scores", signature(object = "truepref"),
  function(object, df, frm) {
    res <- eval(object@lazy_expr$expr, df, frm) 
    if (!is.logical(res)) stop("For the true preference ", as.character(object), " the expression must be logical!")
    return(1 - as.numeric(res))
  }
)

is.truepref <- function(x) inherits(x, "truepref")

# Complex preferences
# -------------------

# they get df_src as argument (propagated from baseprefs or sub-preferences)

# Unary complex preference: Reverse preference (reevrsing the order)
reversepref <- setClass("reversepref",
  prototype = list(),
  slots = c(
    p = "preference"
  ),
  contains = "preference"
)

setMethod("initialize", "reversepref",
  function(.Object, p) {
    .Object@p <- p
    return(.Object)
  }
)

setMethod("get_scores", signature(object = "reversepref"),
  function(object, next_id, df) {
    res <- get_scores(object@p, next_id, df)
    object@p <- res$p
    return(list(p = object, next_id = res$next_id, scores = res$scores))
  }
)

setMethod("as.character", signature(x = "reversepref"),
  function(x, parent_op = "") {
    return(paste0('-', as.character(x@p, parent_op = parent_op)))
  }
)

setMethod("cmp", signature(object = "reversepref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return(cmp(object@p, j, i, score_df)) # swap i/j for realising reverse
  }
)

setMethod("eq", signature(object = "reversepref"),
  function(object, i, j, score_df) { # TRUE if i is equal to j
    eq(object@p, i, j, score_df)
  }
)

setMethod("pserialize", signature(object = "reversepref"),
  function(object) {
    return(list(kind = '-', p = pserialize(object@p)))
  }
)

setMethod("length", signature(x = "reversepref"),
  function(x) {
    return(length(x@p))
  }
)

setMethod("evaluate", signature(object = "reversepref"),
  function(object, static_terms) {
    object@p <- evaluate(object@p, static_terms)
    return(object)
  }
)

is.reversepref <- function(x) inherits(x, "reversepref")

#' @export
setMethod("as.expression", signature(x = "reversepref"),
  function(x, ...) {
    return(as.expression(call('-', as.expression(x@p, ...)[[1]])))
  }
)

# Binary complex preferences
complexpref <- setClass("complexpref",
  prototype = list(),
  slots = c(
    p1 = "preference",
    p2 = "preference",
    op = "character"
  ),
  contains = "preference"
)

setMethod("initialize", "complexpref",
  function(.Object, p1, p2) {
    .Object@p1 <- p1
    .Object@p2 <- p2
    return(.Object)
  }
)

setMethod("get_scores", signature(object = "complexpref"),
  function(object, next_id, df) {
    # ** Usual complex preference
    res1 <- get_scores(object@p1, next_id, df)
    res2 <- get_scores(object@p2, res1$next_id, df)
    object@p1 <- res1$p
    object@p2 <- res2$p
    return(list(p = object, next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
  }
)

setMethod("as.character", signature(x = "complexpref"),
  function(x, parent_op = "") {
    res <- paste0(as.character(x@p1, parent_op = x@op), ' ', x@op, ' ', as.character(x@p2, parent_op = x@op))
    if (parent_op != "" && x@op != parent_op) res <- embrace(res)
    return(res)
  }
)

setMethod("pserialize", signature(object = "complexpref"),
  function(object) {
    return(list(kind = object@op, p1 = pserialize(object@p1), p2 = pserialize(object@p2)))
  }
)

setMethod("length", signature(x = "complexpref"),
  function(x) {
    return(length(x@p1) + length(x@p2))
  }
)

# Equivalence composition for ALL complex preferences
setMethod("eq", signature(object = "complexpref"),
  function(object, i, j, score_df) { # TRUE if i is equal to j
    return(eq(object@p1, i, j, score_df) & eq(object@p2, i, j, score_df))
  }
)

setMethod("evaluate", signature(object = "complexpref"),
  function(object, static_terms) {
    object@p1 <- evaluate(object@p1, static_terms)
    object@p2 <- evaluate(object@p2, static_terms)
    return(object)
  }
)
    
is.binarycomplexpref <- function(x) inherits(x, "complexpref")

#' @export
setMethod("as.expression", signature(x = "complexpref"),
  function(x, ...) {
    # x is a base preference OR complex preference
    return(as.expression(call(x@op, as.expression(x@p1, ...)[[1]], as.expression(x@p2, ...)[[1]])))
  }
)

paretopref <- setClass("paretopref",
  prototype = list(op = "*"),
  contains = "complexpref"
)

setMethod("cmp", signature(object = "paretopref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return ( (cmp(object@p1, i, j, score_df) | eq(object@p1, i, j, score_df)) & cmp(object@p2, i, j, score_df) |
             (cmp(object@p2, i, j, score_df) | eq(object@p2, i, j, score_df)) & cmp(object@p1, i, j, score_df)   )
  }
)

unionpref <- setClass("unionpref",
  prototype = list(op = "+"),
  contains = "complexpref"
)

setMethod("cmp", signature(object = "unionpref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return (cmp(object@p1, i, j, score_df) | cmp(object@p2, i, j, score_df))
  }
)

intersectionpref <- setClass("intersectionpref",
  prototype = list(op = "|"),
  contains = "complexpref"
)

setMethod("cmp", signature(object = "intersectionpref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    return (cmp(object@p1, i, j, score_df) & cmp(object@p2, i, j, score_df))
  }
)


# Prioritization preferences
# --------------------------

# double has 52 bits significand, thus the largest double number d which surely fulfills "d \neq d+1" is 2^52
MAX_CHAIN_LENGTH <- 52 


priorpref <- setClass("priorpref",
  slots = c(
    chain_size = "numeric",
    prior_chain = "logical",
    prior_score_id = "numeric"
  ),
  prototype = list(op = "&",
                   chain_size = numeric(0),
                   prior_chain = FALSE,
                   prior_score_id = numeric(0)),
  contains = "complexpref"
)

# not generic, prior-specific. calculate recursively the length of a prior chain
# returning NULL means, that the given preference is not a prior chain
get_prior_length <- function(object) {
  # Calculate sum 
  len <- 0
  for (p in c('p1', 'p2')) {
    if (is.priorpref(methods::slot(object, p))) {
      res <- get_prior_length(methods::slot(object, p))
      if (is.null(res)) return(NULL)
      methods::slot(object, p) <- res$p
      len <- len + res$len
    } else if (is.truepref(methods::slot(object, p))) {
      len <- len + 1
    } else {
      return(NULL)
    }
    
    # Store chain_size for get_scores(..., get_subtree = TRUE)
    object@chain_size <- len
  }
  # has to return p as this was modified
  return(list(p = object, len = len))
}
    
# Calculate prior-chain score, not generic, prior-specific
# does not modify the object, hence does not return it!
get_priorchain_scores <- function(object, next_id, prior_length, df) {
  scores <- 0
  for (p in c('p1', 'p2')) {
    if (is.priorpref(methods::slot(object, p))) {
      res <- get_priorchain_scores(methods::slot(object, p), next_id, prior_length, df)
      scores <- scores + res$scores
      next_id <- res$next_id
    } else if (is.truepref(methods::slot(object, p))) { # p is not prioritization, use generic!
      res <- get_scores(methods::slot(object, p), 1, df)
      scores <- scores + 2 ^ (prior_length - next_id) * res$scores
      # Increase next_id after true preference
      next_id <- next_id + 1
    }
  }
  
  return(list(next_id = next_id, scores = scores))
}

setMethod("get_scores", signature(object = "priorpref"),
  function(object, next_id, df) {
    # call specific function for prior-pref
    return(get_scores_prior(object, next_id, df))
  }
)
    
# Get scorevals, specially consider prior-chains. Called *before* serialize! Not generic, prior-specific
get_scores_prior <- function(object, next_id, df, get_subtree = FALSE) {
  
  # By default false
  object@prior_chain = FALSE
  
  if (get_subtree) { # It's a prior-chain, but perhaps to large
    
    if (object@chain_size <= MAX_CHAIN_LENGTH) { # subtree which is sufficiently small
      # Set prior-chain (pseudo score preference)
      object@prior_chain = TRUE
      object@prior_score_id = next_id
      res <- get_priorchain_scores(object, 1, object@chain_size, df) # does not return pref!
      return(list(p = object, next_id = next_id + 1, scores = res$scores))
    } # -> else: see block "** All else-paths"
  
  } else {
  
    # ** Prior chains
    res_prior <- get_prior_length(object)
    # Check if len > 1 (a real chain) 
    if (!is.null(res_prior) && (res_prior$len > 1)) {
      prior_len <- res_prior$len
      object@chain_size <- res_prior$p@chain_size
      # check if maxval is suffcient small to get distinctive score vals (double precision!)
      if (prior_len <= MAX_CHAIN_LENGTH) { 
        # Set prior-chain (pseudo score preference)
        object@prior_chain = TRUE
        object@prior_score_id = next_id
        res <- get_priorchain_scores(object, 1, prior_len, df) # does not return pref!
        return(list(p = object, next_id = next_id + 1, scores = res$scores))
      } else {
        # Limit reached - stop the recursive search for chains
        
        # Give a warning for all unbalanced trees
        if (is.priorpref(object@p1) && object@p1$chain_size > MAX_CHAIN_LENGTH && is.truepref(object@p2) ||
            is.priorpref(object@p2) && object@p2$chain_size > MAX_CHAIN_LENGTH && is.truepref(object@p1))
          warning(paste0("Prioritization chain of true-preferences exceeded maximal size (", MAX_CHAIN_LENGTH, ") ",
                         "and the operator tree is unbalanced. ",
                          "This is a performance issue. It is recommended to build chains like ",
                          "(P1 & ... & P", MAX_CHAIN_LENGTH, ") & ... & (P1 & ... & P", MAX_CHAIN_LENGTH, ") ",
                          "for better performance of the preference evaluation."))
        
        # Change to search with known chain_sizes and use flag get_subtree = TRUE
        res <- get_scores_prior(object, next_id, df, get_subtree = TRUE)
        return(list(p = res$object, next_id = res$next_id, scores = res$scores))
      }
    }
  }
  
  # ** All else-paths: Handle like usual complex preference, but propagate get_subtree
  res1 <- if (is.priorpref(object@p1)) get_scores_prior(object@p1, next_id,      df, get_subtree) else get_scores(object@p1, next_id,      df)
  res2 <- if (is.priorpref(object@p2)) get_scores_prior(object@p2, res1$next_id, df, get_subtree) else get_scores(object@p2, res1$next_id, df)
  object@p1 <- res1$p
  object@p2 <- res2$p
  return(list(p = object, next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
}

setMethod("pserialize", signature(object = "priorpref"),
  function(object) {
    if (object@prior_chain)  # Prior chain is like a numerical base preference
      return(list(kind = 's'))
    else
      return(methods::callNextMethod(object))
  }
)

setMethod("cmp", signature(object = "priorpref"),
  function(object, i, j, score_df) { # TRUE if i is better than j
    if (object@prior_chain) # Prior-Chain => Score Pref
          return(score_df[i, object@prior_score_id] < score_df[j, object@prior_score_id])
        else # Usual composition function for prior-preference
          return( cmp(object@p1, i, j, score_df) | 
                 (eq( object@p1, i, j, score_df) & cmp(object@p2, i, j, score_df)) ) 
  }
)

setMethod("eq", signature(object = "priorpref"),
  function(object, i, j, score_df) { # TRUE if i is equal to j
      if (object@prior_chain) # Prior-Chain => Score Pref
        return(score_df[i, object@prior_score_id] == score_df[j, object@prior_score_id])
      else
        return(methods::callNextMethod(object, i, j, score_df))
  }
)
  
is.priorpref <- function(x) inherits(x, "priorpref")

# Non-abstract preference?
is.actual.preference <- function(x) (is.base_pref(x) || is.complex_pref(x) || is.empty_pref(x))
  


# Helper functions
# ----------------

# Expression (as a CALL!) of preference with partial evaluation
# only called by as.expression.basepref, NOT generic
get_inner_expr <- function(object, static_terms = NULL) {
  if (is.null(static_terms)) return(object@lazy_expr$expr)
  
  get_expr_evaled = function(lexpr) {
    
    # Helper for transforming an evaluated object into expression
    get_expr <- function(val) {
      if (is.preference(val)) {
        # propagete preference -> expression 
        # (even it is unlikely that a base-pref expression contains a preference)
        return(as.expression(val, static_terms = static_terms))
      } else if (length(val) <= 1) {
        # atomic objects
        return(as.expression(val))
      } else {
        # non-atomic objects, we assume lists or vectors
        if (is.list(val)) op <- quote(list)
        else if (is.vector(val)) op <- quote(c)
        else stop(paste0("Cannot evaluate the object ", as.character(val)))
        return(as.expression(as.call(c(list(op), as.list(val)))))
      }
    }
    
    lexpr <- lexpr[[1]] # unwrap expression
    if (is.symbol(lexpr)) { 
      lexpr_chr <- as.character(lexpr)
      if (lexpr_chr == '...') # ... cannot be eval'd directly!
        return(list(expr = lexpr, final = FALSE)) # ... is never final, will be evaluated above if possible
      else if (any(vapply(static_terms, function(x) identical(lexpr, x), TRUE)))
        return(list(expr = lexpr, final = TRUE)) # x is a static_term => final
      else if (lexpr_chr == 'df__')
        return(list(expr = lexpr, final = TRUE)) # special variable df__ => final
      else # eval a symbol (e.g. a number)
        return(list(expr = get_expr(eval(lexpr, object@lazy_expr$env)), final = FALSE))
      
    } else if (length(lexpr) == 1) { # Not a symbol => not final
      return(list(expr = lexpr, final = FALSE))
      
    } else if (length(lexpr[1]) == 1 && as.character(lexpr[1]) == '$') { # object$attribute operator
      
      # first evaluate the object
      expr_eval_first <- get_expr_evaled(lexpr[2]) 
      
      if (expr_eval_first$final == FALSE) # not final => re-eval object$attribute
        return(list(expr = get_expr(eval(lexpr, object@lazy_expr$env)), final = FALSE))
      else # final => return the object_new$attribute
        return(list(expr = call('$', expr_eval_first$expr, lexpr[3][[1]]), final = TRUE))
      
      
    } else { # n-ary function/operator
      
      # Go into recursion and generate new term
      lexpr_lst <- as.list(lexpr) # New expression as list
      lexpr_names <- if (is.null(names(lexpr_lst))) rep("", length(lexpr)) else names(lexpr_lst) # Preserve names
      
      # Collect operands and names
      lexpr_lst <- lexpr_lst[1]
      names(lexpr_lst) <- ""
      expr_evals <- list()
      for (i in 2:length(lexpr)) {
        
        # catch empty symbol as occuring in, e.g., df[,1]. 
        if (is.symbol(lexpr[i][[1]]) && as.character(lexpr[i][[1]]) == '') {
          # Must not be stored in a variable, hence calling get_expr_evaled CANNOT work!
          # -> save to evaled, not final
          lexpr_lst <- c(lexpr_lst, lexpr[i][[1]])
          expr_evals[[i-1]] <- list(final = FALSE) # add final = FALSE manually to emulate usual structure
        } else { # All other (non-empty!) sub-expression
          # ** Recursion call!
          expr_evals[[i-1]] <- get_expr_evaled(lexpr[i]) 
          # Catch abstract function argument "..."
          if (identical(expr_evals[[i-1]]$expr, quote(...))) {
            lexpr_lst <- c(lexpr_lst, eval(expression(list(...)), object@lazy_expr$env)) # Re-Evaulation!
          } else {
            # Usual recursive call result
            lexpr_lst <- c(lexpr_lst, as.expression(expr_evals[[i-1]]$expr)[[1]]) # Put terms together
            names(lexpr_lst)[length(lexpr_lst)] <- lexpr_names[i]
          }
        }
      }
      lexpr_new <- as.call(lexpr_lst)
      
      # Check if not final
      if (all(vapply(expr_evals, function(x) x$final, TRUE) == FALSE)) { 
        # ** re-eval
        # Especially eval ... at that level where ... is an operand
        # Evaluate the entire term if possible (no final symbol occurs!)
        return(list(expr = get_expr(eval(lexpr, object@lazy_expr$env)), final = FALSE)) # not final
      } else {
        return(list(expr = lexpr_new, final = TRUE)) # already final!
      }
    } 
  }  # End get_expr_evaled
  
  # Call get_expr_evaled with my own expression and extract result 
  return(get_expr_evaled(as.expression(object@lazy_expr$expr))$expr)
}


# Remove brackets from string if existing
unbrace <- function(x) {
  if (substr(x, 1, 1) == "(" && substr(x, nchar(x), nchar(x)) == ")") return(substr(x, 2, nchar(x)-1))
  else return(x)
}

# Add brackets to string
embrace <- function(x) {
  return(paste0('(', x, ')'))
}
  
  