

# Preference classes
# ------------------

# All functions are internal!

# General preference (only for internal use)
preference <- setRefClass("preference",
  fields = list(expr = "expression", score_id = "numeric", eval_frame = "environment", 
                # cache for Hasse diagram / scorevals
                hasse_mtx = "matrix", scorevals = "data.frame", cache_available = "logical"), 
  methods = list(
    
    # Default show function
    show = function() {
      return(cat(paste0('[Preference] ', .self$get_str())))
    },
    
    # Get string representation
    get_str = function(parent_op = "", static_terms = NULL) {
      return("(empty)") 
    },
    
    
    # Predecessor/Successor functions
    # -------------------------------
    
    # Initialize cache for Hasse diagram / Scorevals
    init_pred_succ = function(df) {
      .self$scorevals <- .self$get_scorevals(1, df)$scores
      serialized <- .self$serialize()
      .self$hasse_mtx <- t(get_hasse_impl(.self$scorevals, serialized)) + 1
      .self$cache_available <- TRUE
      # returns nothing
    },
    
    check_cache = function() {
      if (!isTRUE(.self$cache_available)) 
        stop("In calculation of predecessors/successors : No data set avaible. Run `init_succ_pref(df)` first.", call. = FALSE)
    },
    
    # Hasse diagramm successors/predecessors
    h_predsucc = function(inds, do_intersect, succ) {
      .self$check_cache()
      
      # Select indices predecessors/successors
      if (succ) {
        l_index <- 1
        r_index <- 2
      } else {
        l_index <- 2
        r_index <- 1
      }
      
      # Selection aggregation method, if inds is a vector (and not a single value)
      if (do_intersect) agg <- intersect
      else              agg <- union
      
      # Do the calculation
      if (length(inds) == 0) {
        return(numeric(0))
      } else if (length(inds) == 1) { 
        return(.self$hasse_mtx[.self$hasse_mtx[,l_index]==inds,r_index])
      } else {
        res_inds <- lapply(inds, function(x) .self$hasse_mtx[.self$hasse_mtx[,l_index]==x,r_index])
        return(sort(Reduce(agg, res_inds[-1], res_inds[[1]])))
      }
    },
    
    # All succesors/predecessors (sorting not necessary because of "which")
    all_predsucc = function(inds, do_intersect, succ) {
      .self$check_cache()
      
      # Select indices predecessors/successors
      if (succ) {
        cmpfun <- function(x, y) .self$cmp(x, y, .self$scorevals)
      } else {
        cmpfun <- function(x, y) .self$cmp(y, x, .self$scorevals)
      }
      
      # Selection aggregation method, if inds is a vector (and not a single value)
      if (do_intersect) agg <- pmin  # min is logically equivalent to intersection
      else              agg <- pmax  # max is logically equivalent to union
      
      .self$check_cache()
      all_inds <- 1:nrow(.self$scorevals)
      if (length(inds) == 0) {
        return(numeric(0))
      } else if (length(inds) == 1) {
        return(which(cmpfun(inds, all_inds)))
      } else {
        return(which(as.logical(do.call(agg, lapply(inds, function(x) cmpfun(x, all_inds) )))))
      }
    }
    
  )
)
is.preference <- function(x) inherits(x, "preference")

# Special empty preference class. All scores are 0
empty.pref <- setRefClass("empty.pref",
  contains = "preference",
  methods = list(
    
    get_scorevals = function(next_id, df) {
      return(list(next_id = next_id + 1, scores = as.data.frame(rep(0, nrow(df)))))
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(0)
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(FALSE)
    },
    
    eq = function(i, j, score_df) { # TRUE if i is equal to j
      return(TRUE)
    },
    
    serialize = function() {
      return(list(kind = 's'));
    }
  )    
)

is.empty.pref <- function(x) inherits(x, "empty.pref")

# cmp/eq functions are not needed for C++ BNL algorithms, but for igraph, etc.

# Base preferences 
basepref <- setRefClass("basepref", 
  contains = "preference",
  fields = list(expr = "expression", score_id = "numeric", eval_frame = "environment"),
  methods = list(
    initialize = function(expr_ = expression(), eval_frame_ = environment()) {
      .self$expr <- expr_
      .self$eval_frame <- eval_frame_
      return(.self)
    },
    
    # Calculate scorevals as dataframe, increment score id
    get_scorevals = function(next_id, df) {
      .self$score_id = next_id
      # Calc score of base preference
      scores <- .self$calc_scores(df, .self$eval_frame)
      # Check if length ok
      if (length(scores) != nrow(df)) {
        if (length(scores) == 1) scores <- rep(scores, nrow(df))
        else stop(paste0("Evaluation of base preference ", .self$get_str(), " does not have the same length as the dataset!"))
      }
      # Increase next_id after base preference
      return(list(next_id = next_id + 1, scores = as.data.frame(scores)))
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(1)
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(score_df[i, .self$score_id] < score_df[j, .self$score_id])
    },
    
    eq = function(i, j, score_df) { # TRUE if i is equal to j
      return(score_df[i, .self$score_id] == score_df[j, .self$score_id])
    },
    
    # Get expression string, with partial evaluation if static_terms is not NULL (they are not evaluated)
    substitute_expr = function(static_terms = NULL) {
      if (!is.null(static_terms)) {
        get_expr_evaled <- function(lexpr) {
          lexpr <- lexpr[[1]]
          if (is.symbol(lexpr)) { 
            if (as.character(lexpr) == '...') # ... cannot be eval'd directly!
              return(list(expr = lexpr, final = FALSE)) # ... is never final, will be evaluated above if possible
            else if (any(vapply(static_terms, function(x) identical(lexpr, x), TRUE)))
              return(list(expr = lexpr, final = TRUE)) # static_term => final
            else
              return(list(expr = as.expression(list(eval(lexpr, .self$eval_frame))), final = FALSE))
          
          } else if (length(lexpr) == 1) { # Not a symbol => not final
            return(list(expr = lexpr, final = FALSE))
            
          } else { # n-ary function/operator
            
            # Go into recursion
            expr_evals <- list()
            for (i in 2:length(lexpr)) {
              expr_evals[[i-1]] <- get_expr_evaled(lexpr[i])
              lexpr[i] <- as.expression(expr_evals[[i-1]]$expr)
            }
            
            # Check if not final
            if (all(vapply(expr_evals, function(x) x$final, TRUE) == FALSE)) { 
              # ** re-eval
              # Especially eval ... at that level where ... is an operand
              return(list(expr = as.expression(list(eval(lexpr, .self$eval_frame))), final = FALSE)) # still not final
            } else {
              return(list(expr = lexpr, final = TRUE)) # already final!
            }
          }
        }
        .self$expr <- as.expression(get_expr_evaled(.self$expr)$expr)
      }
      return(NULL) # nothing to return
    },
    
    # Get string representation
    get_str = function(parent_op = "") {
      return(paste0(.self$op(), '(', as.character(.self$expr), ')'))
    },
    
    serialize = function() {
      return(list(kind = 's'));
    }
  )
)
is.basepref <- function(x) inherits(x, "basepref")

lowpref <- setRefClass("lowpref", 
  contains = "basepref",
  methods = list(
    op = function() 'low',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.numeric(res)) stop("For the low preference ", .self$get_str(), " the expression must be numeric!")
      return(res)        
    }
  )
)
is.lowpref <- function(x) inherits(x, "lowpref")


highpref <- setRefClass("highpref", 
  contains = "basepref",
  methods = list(
    op = function() 'high',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.numeric(res)) stop("For the high preference ", .self$get_str(), " the expression must be numeric!")
      return(-res)
    }
  )
)
is.highpref <- function(x) inherits(x, "highpref")


truepref <- setRefClass("truepref", 
  contains = "basepref",
  methods = list(
    
    initialize = function(...) {
      callSuper(...)
      if (isTRUE(getOption("rPref.checkLogicalAndOr", default = TRUE))) {
        if (length(as.character(.self$expr) > 0) && grepl(" \\|\\||\\&\\& ", as.character(.self$expr)))
          warning(paste0("In true(...) :\n", 
                   "Detected logical operator '&&' or '||' in logical expreesion -- possibly unexpected behavior. ",
                   "Probably you intended '&' or '|'? Set \n'options(rPref.checkLogicalAndOr = FALSE)' \nto disable this warning."),
                   call. = FALSE)
      }
      return(.self)
    },
    
    op = function() 'true',
    
    calc_scores = function(df, frm) {
      res = eval(.self$expr, df, frm)
      if (!is.logical(res)) stop("For the true preference ", .self$get_str(), " the expression must be logical!")
      return(1 - as.numeric(res))
    }
  )
)
is.truepref <- function(x) inherits(x, "truepref")


# Reverse preference (revrsing the order)
reversepref <- setRefClass("reversepref",
  contains = "preference",
  fields = list(p = "preference"),
  methods = list(
    initialize = function(p_ = preference()) {
      .self$p <- p_
      return(.self)
    },
    
    get_scorevals = function(next_id, df) {
      res <- p$get_scorevals(next_id, df)
      return(list(next_id = res$next_id + 1, scores = res$scores))
    },
    
    # Get string representation
    get_str = function(parent_op = "") {
      return(paste0('-', .self$p$get_str(parent_op)))
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(p$cmp(j, i, score_df))
    },
    
    eq = function(...) p$eq(...),
    
    serialize = function() {
      return(list(kind = '-', p = .self$p$serialize()));
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(.self$p$get_length())
    },
    
    substitute_expr = function(static_terms = NULL) {
      .self$p$substitute_expr(static_terms)
      return(NULL)
    }
  )
)
is.reversepref <- function(x) inherits(x, "reversepref")


# Binary complex preferences
# cmpcom and eqcomp functions are set via constructor and are just compositions (refering to $cmp and $eq)
complexpref <- setRefClass("complexpref",
  contains = "preference",
  fields = list(p1 = "preference", p2 = "preference", op = "character", 
                cmpcomp = "function", eqcomp = "function", prior_chain = "logical"),
  methods = list(
    initialize = function(p1_ = preference(), p2_ = preference(), op_ = '', cmp_ = function() NULL, eq_ = function() NULL) {
      .self$p1      <- p1_
      .self$p2      <- p2_
      .self$op      <- op_
      .self$cmpcomp <- cmp_ # composition of cmp function
      .self$eqcomp  <- eq_
      .self$prior_chain <- FALSE
      return(.self)
    },
      
    # Get Scorevals / check for prior-chain. Called *before* serialize!
    get_scorevals = function(next_id, df) {
      # ** Usual complex preference
      res1 <- .self$p1$get_scorevals(next_id, df)
      res2 <- .self$p2$get_scorevals(res1$next_id, df)
      return(list(next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
    },
    
    # Get string representation
    get_str = function(parent_op = "") {
      res <- paste0(.self$p1$get_str(.self$op), ' ', .self$op, ' ', .self$p2$get_str(.self$op))
      if (parent_op != "" && .self$op != parent_op) res <- embrace(res)
      return(res)
    },
    
    # Serialization for C++ Interface
    serialize = function() {
      return(list(kind = .self$op, p1 = .self$p1$serialize(), p2 = .self$p2$serialize()))
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(.self$p1$get_length() + .self$p2$get_length())
    },
    
    # Substitute all evaluatable parts of expression by their evaluations
    substitute_expr = function(static_terms = NULL) {
      .self$p1$substitute_expr(static_terms)
      .self$p2$substitute_expr(static_terms)
      return(NULL)
    },
    
    # Simple wrapper for cmp/eq compositions (overwritten by prioritization)
    cmp = function(...) .self$cmpcomp(...),
    eq  = function(...) .self$eqcomp(...)
    
  )
)
is.complexpref <- function(x) inherits(x, "complexpref")

# double has 52 bits significand, thus the largest double number d which surely fulfills "d \neq d+1" is 2^52
MAX_CHAIN_LENGTH <- 52 

priorpref <- setRefClass("priorpref",
  contains = "complexpref",
  fields = list(chain_size = "numeric", prior_chain = "logical", prior_score_id = "numeric"),
  methods = list(   
    
    # Check if we have a purely prioritization chain - calculate highest value
    get_prior_length = function() {
      # Calculate sum 
      len <- 0
      for (p in list(.self$p1, .self$p2)) {
        if (is.priorpref(p)) {
          tres <- p$get_prior_length()
          if (is.null(tres)) return(NULL)
          len <- len + tres
        } else if (is.truepref(p)) {
          len <- len + 1
        } else {
          return(NULL)
        }
        
        # Store chain_size for get_scorevals(..., get_greatest_subtree = TRUE)
        .self$chain_size <- len
      }
      return(len)
    },
    
    # Calculcate prior-chain score
    get_prior_scores = function(next_id, prior_length, df) {
      score_vals <- 0
      for (p in list(.self$p1, .self$p2)) {
        if (is.complexpref(p)) {
          res <- p$get_prior_scores(next_id, prior_length, df) 
          score_vals <- score_vals + res$score_vals
          next_id <- res$next_id
        } else if (is.truepref(p)) {
          score_vals <- score_vals + 2 ^ (prior_length - next_id) * p$get_scorevals(1, df)$scores
          # Increase next_id after true preference
          next_id <- next_id + 1
        }
      }
      
      return(list(next_id = next_id, score_vals = score_vals))
    },
    
    # Get scorevals, specially consider prior-chains. Called *before* serialize!
    get_scorevals = function(next_id, df, get_greatest_subtree = FALSE) {
      
      # By default false
      .self$prior_chain = FALSE
      
      if (get_greatest_subtree) { # It's a prior-chain, but perhaps to large
        
        if (.self$chain_size <= MAX_CHAIN_LENGTH) { # subtree which is sufficiently small
          # Set prior-chain (pseudo score preference)
          .self$prior_chain = TRUE
          .self$prior_score_id = next_id
          scores <- .self$get_prior_scores(1, .self$chain_size, df)$score_vals
          return(list(next_id = next_id + 1, scores = scores))
        } # -> else: see block "** All else-paths"
      
      } else {
      
        # ** Prior chains
        prior_res_len <- .self$get_prior_length()
        # Check if len > 1 (a real chain) 
        if (!is.null(prior_res_len) && (prior_res_len > 1)) {
          # check if maxval is suffcient small to get distinctive score vals (double precision!)
          if (prior_res_len <= MAX_CHAIN_LENGTH) { 
            # Set prior-chain (pseudo score preference)
            .self$prior_chain = TRUE
            .self$prior_score_id = next_id
            scores <- .self$get_prior_scores(1, prior_res_len, df)$score_vals
            return(list(next_id = next_id + 1, scores = scores))
          } else {
            # Limit reached - stop the recursive search for chains
            
            # Give a warning for all unbalanced trees
            if (is.priorpref(.self$p1) && .self$p1$chain_size > MAX_CHAIN_LENGTH && is.truepref(.self$p2) ||
                is.priorpref(.self$p2) && .self$p2$chain_size > MAX_CHAIN_LENGTH && is.truepref(.self$p1))
              warning(paste0("Prioritization chain exceeded maximal size (", MAX_CHAIN_LENGTH, ") and the operator tree is unbalanced. ",
                              "This is a performance issue. It is recommended to build chains like ",
                              "(P1 & ... & P", MAX_CHAIN_LENGTH, ") & ... & (P1 & ... & P", MAX_CHAIN_LENGTH, ") ",
                              "for better performance of the preference evaluation."))
            
            # Change to search with known chain_sizes and use flag get_greatest_subtree = TRUE
            return(.self$get_scorevals(next_id, df, get_greatest_subtree = TRUE))
          }
        }
      }
      
      # ** All else-paths: Handle like usual complex preference, but propagete get_greatest_subtree
      res1 <- if (is.priorpref(.self$p1)) .self$p1$get_scorevals(next_id,      df, get_greatest_subtree) else .self$p1$get_scorevals(next_id,      df)
      res2 <- if (is.priorpref(.self$p2)) .self$p2$get_scorevals(res1$next_id, df, get_greatest_subtree) else .self$p2$get_scorevals(res1$next_id, df)
      return(list(next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
    },
    
    # Serialization for C++ Interface
    serialize = function() {
      if (.self$prior_chain)  # Prior chain is like a numerical base preference
        return(list(kind = 's'))
      else
        return(callSuper())
    },
    
    # Wrapper for Compare/Prioritization
    cmp = function(i, j, score_df) {
      if (.self$prior_chain) # Prior-Chain => Score Pref
        return(score_df[i, .self$prior_score_id] < score_df[j, .self$prior_score_id])
      else
        return(.self$cmpcomp(i, j, score_df)) # Usual composition function
    },
    
    # Wrapper for Equal/Prioritization
    eq = function(i, j, score_df) {
      if (.self$prior_chain) # Prior-Chain => Score Pref
        return(score_df[i, .self$prior_score_id] == score_df[j, .self$prior_score_id])
      else
        return(.self$eqcomp(i, j, score_df)) # Usual composition function
    }  
  )
)
  
is.priorpref <- function(x) inherits(x, "priorpref")
  
# Some helper functions
# ---------------------

# Remove brackets from string if existing
unbrace <- function(x) {
  if (substr(x, 1, 1) == "(" && substr(x, nchar(x), nchar(x)) == ")") return(substr(x, 2, nchar(x)-1))
  else return(x)
}

# Add brackets to string
embrace <- function(x) {
  return(paste0('(', x, ')'))
}
  
  