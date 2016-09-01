

# Wrapper class for associcated dataframes
# ----------------------------------------

# Avoids deep copies of the dataframe
#' @importFrom R6 R6Class
dataframe_src <- R6::R6Class("dataframe_src",
  public = list(
    df = NULL,
    print = function() {
      cat(self$info_str, '\n', sep = '')
    },
    initialize = function(df, df_call) {
      self$df <- df
      df_name <- as.character(as.expression(df_call))
      self$info_str <- paste0(class(df)[1], ' "', df_name, '" [', nrow(df), ' x ', ncol(df), ']')
    },
    info_str = ''
 )
)


# Preference classes
# ------------------

# All functions are internal!

# General preference (only for internal use)
#' @importFrom R6 R6Class
preference <- R6::R6Class("preference",
  public = list(
    
    # associated data.frame
    df_src = NULL,
    
    # Default show function
    print = function() {
      cat('[Preference] ', self$get_str(), '\n', sep = "")
      if (!is.null(self$df_src))
        cat('  * associated data source: ', self$df_src$info_str, '\n', sep = "")
      if (private$cache_available)
        cat('  * Hasse matrix for predecessors/successors available')
    },
    
    # Get string representation
    get_str = function(parent_op = "", static_terms = NULL) {
      return("[abstract preference superclass]") 
    },
    
    # Default term length (to be overwritten!)
    get_length = function() {
      return(0)
    },
    
    # Set new data set source (to be overwritten!)
    set_df_src = function() {},
    
    
    # Predecessor/Successor functions
    # -------------------------------
    
    # Initialize cache for Hasse diagram / Scorevals
    init_pred_succ = function(df = NULL, df_call = NULL) {
      # Overwrite associcated data frame, do partial evaluation
      if (!is.null(df)) self$set_df_src(dataframe_src$new(df, df_call))
      else if (is.null(self$df_src)) stop("No data source was specified!")
      
      # Use specified data.frame from now on
      private$scorevals <- self$get_scorevals(1, self$df_src$df)$scores
      serialized <- self$serialize()
      # Get Hasse Matrix from C++ function, add 1 for R indices (starting at 1)
      private$hasse_mtx <- t(get_hasse_impl(private$scorevals, serialized)) + 1
      private$cache_available <- TRUE
      # returns nothing
    },
    
    # Hasse diagram successors/predecessors
    h_predsucc = function(inds, do_intersect, succ) {
      private$check_cache()
      
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
        return(private$hasse_mtx[private$hasse_mtx[,l_index] == inds, r_index])
      } else {
        res_inds <- lapply(inds, function(x) private$hasse_mtx[private$hasse_mtx[,l_index] == x, r_index])
        return(sort(Reduce(agg, res_inds[-1], res_inds[[1]])))
      }
    },
    
    # All succesors/predecessors (sorting not necessary because of "which")
    all_predsucc = function(inds, do_intersect, succ) {
      private$check_cache()
      # does not need hasse diagram, but needs self$scorevals
      
      # Select indices predecessors/successors
      if (succ) cmpfun <- function(x, y) self$cmp(x, y, private$scorevals)
      else      cmpfun <- function(x, y) self$cmp(y, x, private$scorevals)
      
      # Selection aggregation method, if inds is a vector (and not a single value)
      if (do_intersect) agg <- pmin  # min is logically equivalent to intersection
      else              agg <- pmax  # max is logically equivalent to union
      
      all_inds <- 1:nrow(private$scorevals)
      if (length(inds) == 0) {
        return(numeric(0))
      } else if (length(inds) == 1) {
        return(which(cmpfun(inds, all_inds)))
      } else {
        return(which(as.logical(do.call(agg, lapply(inds, function(x) cmpfun(x, all_inds) )))))
      }
    }
    
  ),
  private = list(
    score_id = 0, 
    
    # cache for Hasse diagram / scorevals
    hasse_mtx = NULL, 
    scorevals = NULL,
    cache_available = FALSE,
    check_cache = function() {
      if (!private$cache_available)
        stop(paste0("In calculation of predecessors/successors : No data set avaible. ",
                    "Run `init_succ_pref(df)` first."), call. = FALSE)
    },
    deep_clone = function(name, value) {
      if (name == "p" || name == "p1" || name == "p2") # preference -> clone
        return(value$clone(deep = TRUE))
      else
        # data frame source or usual variable
        return(value)
    })
)


is.preference <- function(x) inherits(x, "preference")


# Special empty preference class. All scores are 0
#' @importFrom R6 R6Class
emptypref <- R6::R6Class("emptypref",
  inherit = preference,
  public = list(
    
    # Empty preference hast "0" for all score vals
    get_scorevals = function(next_id, df) {
      return(list(next_id = next_id + 1, scores = as.data.frame(rep(0, nrow(df)))))
    },
    
    # Get string representation
    get_str = function(parent_op = "", static_terms = NULL) {
      return("(empty)") 
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
    },
    
    evaluate = function(static_terms) {
      return(empty())
    }
  )    
)

#' @export
as.expression.emptypref <- function(x, ...) expression(empty())


# cmp/eq functions are not needed for C++ BNL algorithms, but for igraph, etc.

# Base preferences 
#' @importFrom R6 R6Class
basepref <- R6::R6Class("basepref", 
  inherit = preference,
  public = list(
    lazy_expr = NULL,
    initialize = function(lazy_expr, df = NULL, df_call = NULL) {
      self$lazy_expr <- lazy_expr
      # Set df, evaluate lazy_expr partially
      if (!is.null(df)) self$set_df_src(dataframe_src$new(df, df_call))
    },
    
    # Set new data frame source and do the partial evaluation
    set_df_src = function(df_src) {
      if (!identical(self$df_src, df_src)) {
        self$df_src <- df_src
        self$evaluate(get_static_terms(self$df_src$df))
      } 
    },
    
    # Calculate scorevals as data frame, increment score id
    get_scorevals = function(next_id, df) {
      private$score_id <- next_id
      # Add data.frame to the environment without modifying original environment
      frm <- new.env(parent = self$lazy_expr$env)
      assign("df__", df, pos = frm)
      
      # Calc score of base preference
      # calc_score is defined in low/high/true preferences
      scores <- self$calc_scores(df, frm)
      
      # Check if length of score vector is ok 
      # (length 1 might happen; has to be appropriately extended)
      if (length(scores) != nrow(df)) {
        if (length(scores) == 1) scores <- rep(scores, nrow(df))
        else stop(paste0("Evaluation of base preference ", self$get_str(), 
                         " does not have the same length as the data frame!"))
      }
      # Increase next_id after base preference
      return(list(next_id = next_id + 1, scores = as.data.frame(scores)))
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(1)
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(score_df[i, private$score_id] < score_df[j, private$score_id])
    },
    
    eq = function(i, j, score_df) { # TRUE if i is equal to j
      return(score_df[i, private$score_id] == score_df[j, private$score_id])
    },
  
    
    # Expression (as a CALL!) of preference with partial evaluation
    # only called by as.expression.basepref, expects an expression
    get_inner_expr = function(static_terms = NULL) {
      if (is.null(static_terms)) return(self$lazy_expr$expr)
      
      get_expr_evaled = function(lexpr) {
        
        # Helper for transforming an evaluated object into expression
        get_expr <- function(val) {
          if (is.preference(val)) {
            # propage preference -> expression 
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
            return(list(expr = get_expr(eval(lexpr, self$lazy_expr$env)), final = FALSE))
          
        } else if (length(lexpr) == 1) { # Not a symbol => not final
          return(list(expr = lexpr, final = FALSE))
          
        } else if (length(lexpr[1]) == 1 && as.character(lexpr[1]) == '$') { # object$attribute operator
          
          # first evaluate the object
          expr_eval_first <- get_expr_evaled(lexpr[2]) 
          
          if (expr_eval_first$final == FALSE) # not final => re-eval object$attribute
            return(list(expr = get_expr(eval(lexpr, self$lazy_expr$env)), final = FALSE))
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
                lexpr_lst <- c(lexpr_lst, eval(expression(list(...)), self$lazy_expr$env)) # Re-Evaulation!
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
            return(list(expr = get_expr(eval(lexpr, self$lazy_expr$env)), final = FALSE)) # not final
          } else {
            return(list(expr = lexpr_new, final = TRUE)) # already final!
          }
        } 
      }  # End get_expr_evaled
      
      # Call get_expr_evaled with my own expression and extract result 
      return(get_expr_evaled(as.expression(self$lazy_expr$expr))$expr)
    },
    
    
    # Get string representation
    get_str = function(parent_op = "") {
      return(paste0(self$op(), '(', as.character(as.expression(self$lazy_expr$expr)), ')'))
    },
    
    serialize = function() {
      return(list(kind = 's'))
    },
    
    # Partial evaluation: Substitute expression ("in place" evaluation)
    evaluate = function(static_terms) {
      self$lazy_expr$expr <- self$get_inner_expr(static_terms)
    }
  )
)

#' @export
as.expression.basepref <- function(x, ...) { 
  # x is a base preference object, where the constructor name is given by x$op()
  args <- list(...)
  # get_expr_evaled returns a Call
  if (!is.null(args[['static_terms']])) {
    expr <- x$get_inner_expr(args[['static_terms']])
  } else {
    expr <- x$lazy_expr$expr
  }
  # this has to return an expression!
  return(as.expression(call(x$op(), expr))) 
}

#' @importFrom R6 R6Class
lowpref <- R6::R6Class("lowpref", 
  inherit = basepref,
  public = list(
    op = function() 'low',
    
    calc_scores = function(df, frm) {
      # frm is from lazy_eval object, augmented by df__, do not use lazy_eval
      res <- eval(self$lazy_expr$expr, df, frm) 
      if (!is.numeric(res)) stop("For the low preference ", self$get_str(), " the expression must be numeric!")
      return(res)        
    }
  )
)
is.lowpref <- function(x) inherits(x, "lowpref")


#' @importFrom R6 R6Class
highpref <- R6::R6Class("highpref", 
  inherit = basepref,
  public = list(
    op = function() 'high',
    
    calc_scores = function(df, frm) {
      res <- eval(self$lazy_expr$expr, df, frm) 
      if (!is.numeric(res)) stop("For the high preference ", self$get_str(), " the expression must be numeric!")
      return(-res)
    }
  )
)
is.highpref <- function(x) inherits(x, "highpref")

#' @importFrom R6 R6Class
truepref <- R6::R6Class("truepref", 
  inherit = basepref,
  public = list(
    
    # true gets a special constructor for checking the expression
    initialize = function(...) {
      super$initialize(...)
      if (isTRUE(getOption("rPref.checkLogicalAndOr", default = TRUE))) {
        chr_expr <- as.character(as.expression(self$lazy_expr$expr))
        if (length(chr_expr > 0) && grepl(" \\|\\||\\&\\& ", chr_expr))
          warning(paste0("In true(...) :\n", 
                   "Detected logical operator '&&' or '||' in logical expreesion -- possibly unexpected behavior. ",
                   "Probably you intended '&' or '|'? ",
                   "Set \n'options(rPref.checkLogicalAndOr = FALSE)' \nto disable this warning."),
                   call. = FALSE)
      }
    },
    
    op = function() 'true',
    
    calc_scores = function(df, frm) {
      res <- eval(self$lazy_expr$expr, df, frm) 
      if (!is.logical(res)) stop("For the true preference ", self$get_str(), " the expression must be logical!")
      return(1 - as.numeric(res))
    }
  )
)
is.truepref <- function(x) inherits(x, "truepref")

# Complex preferences
# -------------------

# they get df_src as argument (propagated from baseprefs or sub-preferences)

# Unary complex preference: Reverse preference (revrsing the order)
#' @importFrom R6 R6Class
reversepref <- R6::R6Class("reversepref",
  inherit = preference,
  public = list(
    p = NULL,
    initialize = function(p, df_src) {
      self$p <- p
      self$set_df_src(df_src)
    },
    
    # Set new data frame source and propagate
    set_df_src = function(df_src) {
      if (!identical(self$df_src, df_src)) {
        self$df_src <- df_src
        self$p$set_df_src(df_src)
      }
    },
    
    get_scorevals = function(next_id, df) {
      res <- self$p$get_scorevals(next_id, df)
      return(list(next_id = res$next_id, scores = res$scores))
    },
    
    # Get string representation
    get_str = function(parent_op = "") {
      return(paste0('-', self$p$get_str(parent_op)))
    },
    
    cmp = function(i, j, score_df) { # TRUE if i is better than j
      return(self$p$cmp(j, i, score_df))
    },
    
    eq = function(...) self$p$eq(...),
    
    serialize = function() {
      return(list(kind = '-', p = self$p$serialize()));
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(self$p$get_length())
    },
    
    evaluate = function(static_terms) {
      self$p$evaluate(static_terms)
    }
  )
)
is.reversepref <- function(x) inherits(x, "reversepref")

#' @export
as.expression.reversepref <- function(x, ...) {
  # x is a base preference OR complex preference
  return(as.expression(call('-', as.expression(x$p, ...)[[1]])))
}

# Binary complex preferences
# cmpcom and eqcomp functions are set via constructor and are just compositions (refering to $cmp and $eq)
#' @importFrom R6 R6Class
complexpref <- R6::R6Class("complexpref",
  inherit = preference,
  public = list(
    p1 = NULL, 
    p2 = NULL, 
    op = NULL,
    
    initialize = function(p1, p2, op = '', df_src) {
      self$p1 <- p1
      self$p2 <- p2
      self$op <- op
      self$set_df_src(df_src)
    },
    
    # Set new data frame source and propagate
    set_df_src = function(df_src) {
      if (!identical(self$df_src, df_src)) {
        self$df_src <- df_src
        self$p1$set_df_src(df_src)
        self$p2$set_df_src(df_src)
      }
    },
      
    # Get Scorevals / check for prior-chain. Called *before* serialize!
    get_scorevals = function(next_id, df) {
      # ** Usual complex preference
      res1 <- self$p1$get_scorevals(next_id, df)
      res2 <- self$p2$get_scorevals(res1$next_id, df)
      return(list(next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
    },
    
    # Get string representation
    get_str = function(parent_op = "") {
      res <- paste0(self$p1$get_str(self$op), ' ', self$op, ' ', self$p2$get_str(self$op))
      if (parent_op != "" && self$op != parent_op) res <- embrace(res)
      return(res)
    },
    
    # Serialization for C++ Interface
    serialize = function() {
      return(list(kind = self$op, p1 = self$p1$serialize(), p2 = self$p2$serialize()))
    },
    
    # Term length (Number of base preferences)
    get_length = function() {
      return(self$p1$get_length() + self$p2$get_length())
    },
    
    # Equivalence composition for all complex preferences
    eq = function(...) (self$p1$eq(...) & self$p2$eq(...)),
    
    # Evaluate sub-expressions in place
    evaluate = function(static_terms) {
      self$p1$evaluate(static_terms)
      self$p2$evaluate(static_terms)
    }
  )
)
is.binarycomplexpref <- function(x) inherits(x, "complexpref")

#' @export
as.expression.complexpref <- function(x, ...) {
    # x is a base preference OR complex preference
  return(as.expression(call(x$op, as.expression(x$p1, ...)[[1]], as.expression(x$p2, ...)[[1]])))
}

#' @importFrom R6 R6Class
paretopref <- R6::R6Class("paretopref",
  inherit = complexpref,
  public = list(   
    
    initialize = function(p1, p2, df_src) {
      super$initialize(p1, p2, '*', df_src)
    },

    cmp = function(...) 
      ( (self$p1$cmp(...) | self$p1$eq(...)) & self$p2$cmp(...) |
        (self$p2$cmp(...) | self$p2$eq(...)) & self$p1$cmp(...)   )
  )
)

#' @importFrom R6 R6Class
unionpref <- R6::R6Class("unionpref",
  inherit = complexpref,
  public = list(   
    
    initialize = function(p1, p2, df_src) {
      super$initialize(p1, p2, '+', df_src)
    },

    cmp = function(...) 
      (self$p1$cmp(...) | self$p2$cmp(...))
  )
)

#' @importFrom R6 R6Class
intersectionpref <- R6::R6Class("intersectionpref",
  inherit = complexpref,
  public = list(   
    
    initialize = function(p1, p2, df_src) {
      super$initialize(p1, p2, '|', df_src)
    },

    cmp = function(...) 
      (self$p1$cmp(...) & self$p2$cmp(...))
  )
)


# double has 52 bits significand, thus the largest double number d which surely fulfills "d \neq d+1" is 2^52
MAX_CHAIN_LENGTH <- 52 

#' @importFrom R6 R6Class
priorpref <- R6::R6Class("priorpref",
  inherit = complexpref,
  public = list(  
    chain_size = NULL,
    prior_chain = FALSE,
    prior_score_id = NULL,
    
    initialize = function(p1, p2, df_src) {
      super$initialize(p1, p2, '&', df_src)
    },
    
    # Check if we have a purely prioritization chain - calculate highest value
    get_prior_length = function() {
      # Calculate sum 
      len <- 0
      for (p in list(self$p1, self$p2)) {
        if (is.priorpref(p)) {
          tres <- p$get_prior_length()
          if (is.null(tres)) return(NULL)
          len <- len + tres
        } else if (is.truepref(p)) {
          len <- len + 1
        } else {
          return(NULL)
        }
        
        # Store chain_size for get_scorevals(..., get_subtree = TRUE)
        self$chain_size <- len
      }
      return(len)
    },
    
    # Calculate prior-chain score
    get_prior_scores = function(next_id, prior_length, df) {
      score_vals <- 0
      for (p in list(self$p1, self$p2)) {
        if (is.priorpref(p)) {
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
    get_scorevals = function(next_id, df, get_subtree = FALSE) {
      
      # By default false
      self$prior_chain = FALSE
      
      if (get_subtree) { # It's a prior-chain, but perhaps to large
        
        if (self$chain_size <= MAX_CHAIN_LENGTH) { # subtree which is sufficiently small
          # Set prior-chain (pseudo score preference)
          self$prior_chain = TRUE
          self$prior_score_id = next_id
          scores <- self$get_prior_scores(1, self$chain_size, df)$score_vals
          return(list(next_id = next_id + 1, scores = scores))
        } # -> else: see block "** All else-paths"
      
      } else {
      
        # ** Prior chains
        prior_res_len <- self$get_prior_length()
        # Check if len > 1 (a real chain) 
        if (!is.null(prior_res_len) && (prior_res_len > 1)) {
          # check if maxval is suffcient small to get distinctive score vals (double precision!)
          if (prior_res_len <= MAX_CHAIN_LENGTH) { 
            # Set prior-chain (pseudo score preference)
            self$prior_chain = TRUE
            self$prior_score_id = next_id
            scores <- self$get_prior_scores(1, prior_res_len, df)$score_vals
            return(list(next_id = next_id + 1, scores = scores))
          } else {
            # Limit reached - stop the recursive search for chains
            
            # Give a warning for all unbalanced trees
            if (is.priorpref(self$p1) && self$p1$chain_size > MAX_CHAIN_LENGTH && is.truepref(self$p2) ||
                is.priorpref(self$p2) && self$p2$chain_size > MAX_CHAIN_LENGTH && is.truepref(self$p1))
              warning(paste0("Prioritization chain of true-preferences exceeded maximal size (", MAX_CHAIN_LENGTH, ") ",
                             "and the operator tree is unbalanced. ",
                              "This is a performance issue. It is recommended to build chains like ",
                              "(P1 & ... & P", MAX_CHAIN_LENGTH, ") & ... & (P1 & ... & P", MAX_CHAIN_LENGTH, ") ",
                              "for better performance of the preference evaluation."))
            
            # Change to search with known chain_sizes and use flag get_subtree = TRUE
            return(self$get_scorevals(next_id, df, get_subtree = TRUE))
          }
        }
      }
      
      # ** All else-paths: Handle like usual complex preference, but propagate get_subtree
      res1 <- if (is.priorpref(self$p1)) self$p1$get_scorevals(next_id,      df, get_subtree) else self$p1$get_scorevals(next_id,      df)
      res2 <- if (is.priorpref(self$p2)) self$p2$get_scorevals(res1$next_id, df, get_subtree) else self$p2$get_scorevals(res1$next_id, df)
      return(list(next_id = res2$next_id, scores = cbind(res1$scores, res2$scores)))
    },
    
    # Serialization for C++ Interface
    serialize = function() {
      if (self$prior_chain)  # Prior chain is like a numerical base preference
        return(list(kind = 's'))
      else
        return(super$serialize())
    },
    
    # Compare/Prioritization
    cmp = function(i, j, score_df) {
      if (self$prior_chain) # Prior-Chain => Score Pref
        return(score_df[i, self$prior_score_id] < score_df[j, self$prior_score_id])
      else # Usual composition function for prior-preference
        return( self$p1$cmp(i, j, score_df) | 
               (self$p1$eq( i, j, score_df) & self$p2$cmp(i, j, score_df)) ) 
    },
    
    # Equal/Prioritization
    eq = function(i, j, score_df) {
      if (self$prior_chain) # Prior-Chain => Score Pref
        return(score_df[i, self$prior_score_id] == score_df[j, self$prior_score_id])
      else
        return(super$eq(i, j, score_df)) 
    }  
  )
)
  
is.priorpref <- function(x) inherits(x, "priorpref")

# Non-abstract preference?
is.actual.preference <- function(x) (is.base_pref(x) || is.complex_pref(x) || is.empty_pref(x))
  
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
  
  