

#' Better-Than-Graphs
#' 
#' Returns/plots a Hasse diagram of a preference order (also called the Better-Than-Graph, short BTG) on a given data set. 
#' Ploting within R relies on the igraph package or the Rgraphviz package.
#' Alternatively, a dot file for an external graphviz/dot interpreter can be generated.
#' 
#' @param df A data frame.
#' @param pref A preference on the columns of \code{df}, see \code{\link{psel}} for details.
#' @param flip.edges (optional) Flips the orientation of edges,
#'        if \code{TRUE} than arrows point from worse nodes to better nodes.
#' @param labels (optional) Labels for the vertices. Default values are the row indices.
#' @param levelwise (optional) Only relevant is the dot layouter is used. 
#'        If \code{TRUE}, all tuples from the same level are placed on one row.
#'        If \code{FALSE}, the row arrangement is subject to the dot layouter.
#' @param use_dot (optional) If \code{TRUE}, the dot layouter from Rgraphviz is used. 
#'        If \code{FALSE}, igraph is used.
#'        By default this is \code{TRUE} iff Rgraphviz is available.
#' @param file (optional) If specified, then \code{get_btg_dot} writes the graph specification to 
#'        given file path. If not specified, the graph specification is returned as a string.
#'        
#' @details
#' 
#' The Hasse diagram of a preference visualizes all the better-than-relationships on a given data set.
#' All edges which can be retrieved by transitivity of the order are omitted in the graph.
#' 
#' The functions \code{get_btg} and \code{plot_btg} either use the \link{igraph}
#' package (if \code{use_dot = FALSE}) or the dot layouter from the Rgraphviz package 
#' (if \code{use_dot = TRUE}). 
#' If Rgraphviz is available it is used by default, otherwise the igraph Package is used.
#' Note that Rgraphviz is only available on BioConductor and not on CRAN.
#' 
#' The dot layouter from Rgraphviz is more appropriate for Better-Than-Graphs than the igraph layouter,
#' as all edges will be directed in the same direction (rank based ordering). 
#' Using \code{levelwise = TRUE} (the default), all tuples of the same level are placed on the same row.
#' 
#' @section BTGs with igraph:
#' 
#' If used with \code{use_dot = FALSE}, 
#' the function \code{get_btg} returns a list \code{l} with the following list entries:
#' 
#' \describe{
#'   \item{\code{l$graph}}{An igraph object, created with the \code{\link{igraph}} package.}
#'   \item{\code{l$layout}}{A typical Hasse diagram layout for plotting the graph, also created with igraph.}
#' }
#' 
#' To plot the resulting graph returned from \code{get_btg}, use the \code{plot} function as follows: 
#' 
#' \code{plot(l$graph, layout = l$layout)} 
#' 
#' For more details, see \code{\link{igraph.plotting}}.
#' 
#' 
#' @section BTGs with Rgraphviz:
#' 
#' If used with \code{use_dot = FALSE}, the function \code{get_btg} returns a \code{graphNEL} object from 
#' the graph-package (Rgraphviz is build on top of that package). 
#' This object can also be plotted using \code{plot(...)}.
#' 
#' @section Direct Plotting:
#' 
#' In both cases (wheter Rgraphviz is used or not), 
#' the function \code{plot_btg} directly plots the Better-Than-Graph.
#' There is an additional parameter \code{labels}, specifying the node labels. 
#' The default are the row numbers (not the \code{rownames} of the data frame),
#' ranging from \code{"1"} to \code{as.character(nrow(df))}.
#' 
#' @section Dot (Graphviz) String Output:
#' 
#' The function \code{get_btg_dot} produces the source code of the Better-Than-Graph in the dot language
#' of the Graphviz software. This is useful for an external dot interpreter. 
#' Depending on the \code{file} parameter the output is either written to a file
#' (if a file path is given) or returned as a string (if \code{file = NULL}).
#' 
#' @section Additional Parameters:
#' 
#' By default, the directed edges in the diagram point from better to worse nodes w.r.t. the preference. 
#' This means an arrow can be read as "is better than". If \code{flip.edges = TRUE} is set, 
#' then the arrows point from worse nodes to better nodes ("is worse than"). 
#' In any case, the better nodes are plotted at the top and the worse nodes at the bottom of the diagram.
#' 
#' If Rgraphviz is used for \code{plot_btg} and for \code{get_btg_dot}, 
#' the option \code{levelwise} controls if all nodes of the same level are placed in one row.
#' If this parameter is \code{FALSE}, then the vertical arrangement is subject to the dot layouter.
#' 
#' 
#' @examples
#' 
#' # pick a small data set and create preference and BTG 
#' df <- mtcars[1:10,]
#' pref <- high(mpg) * low(wt)
#' 
#' # directly plot the BTG with row numbers as labels
#' # uses Rgraphviz if available and igraph otherwise
#' plot_btg(df, pref) 
#' 
#' # plot the graph with labels with relevant values
#' labels <- paste0(df$mpg, "; ", df$wt)
#' plot_btg(df, pref, labels)
#'      
#' # show lattice structure of 3-dimensional Pareto preference
#' df <- merge(merge(data.frame(x = 1:3), data.frame(y = 1:3)), data.frame(z = 1:2))
#' labels <- paste0(df$x, ",", df$y, ",", df$z)
#' plot_btg(df, low(x) * low(y) * low(z), labels)
#'      
#' # Create a graph with external Graphviz (requires installed Graphviz)
#' \dontrun{
#' # creates tmpgraph.dot in the current working directoy
#' get_btg_dot(df, pref, labels, file = "tmpgraph.dot")
#' # convert to diagram tmpgraph.png using Graphviz
#' shell(paste0('"C:/Program Files (x86)/Graphviz2.38/bin/dot.exe"',
#'              ' -Tpng tmpgraph.dot -o tmpgraph.png'))
#' # open resulting image
#' shell("tmpgraph.png")}
#' 
#' @importFrom utils installed.packages
#' @export
plot_btg <- function(df, pref, labels = 1:nrow(df), flip.edges = FALSE, 
                     levelwise = TRUE,
                     use_dot = "Rgraphviz" %in% rownames(installed.packages())) {
  
  # check 
  check.plot.base(df, pref, labels)
  
  # Get BTG (including checks for df/pref)
  btg <- get_btg(df, pref, flip.edges, use_dot)
  
  if (use_dot) {
    
    # ** Layers
    
    if (levelwise) {
      # Each level is one layer independent of flip.edges!
      
      # Get levels
      df_lev <- psel.indices(df, pref, top = nrow(df), show_level = TRUE)
      
      # Generate subgraphs
      subgraphs <- lapply(1:max(df_lev$.level), function(l) {
        nodes <- as.character(df_lev[df_lev$.level == l, '.indices'])
        list(graph = graph::subGraph(nodes, btg))
      })
    } else {
      
      # Only maxima are one layer
      max_nodes <- as.character(psel.indices(df, pref))
      subgraphs <- list(list(graph = graph::subGraph(max_nodes, btg)))
    }
    
    # ** Labels/Plotting
    
    # Lables of nodes
    attr(labels, "names") <- as.character(1:nrow(df))
    nAttrs <- list(label = labels)
    
    # Graph attributes
    att <- list(node = list(shape = "ellipse", fixedsize = FALSE))
    
    # Flip edges? (Arrows point to max)
    if (flip.edges) att$graph <- list(rankdir = "BT")
    
    Rgraphviz::plot(btg, attrs = att, nodeAttrs = nAttrs, subGList = subgraphs)
    
  } else { # use igraph
    
    # Plot igraph BTG using some reasonable defaults
    igraph::plot.igraph(btg$graph, layout = btg$layout, vertex.label = as.character(labels), 
                        vertex.color = 'white', vertex.size = 20, vertex.label.cex = 1.5,
                        edge.color = 'black', vertex.label.color = 'black')
  }
}

#' @rdname plot_btg
#' @importFrom utils installed.packages
#' @export
get_btg <- function(df, pref, flip.edges = FALSE, 
                    use_dot = "Rgraphviz" %in% rownames(installed.packages())) {
  
  check.plot.base(df, pref)
  
  # Arrows from worse to better?
  if (flip.edges) pref <- -pref
  
  # calculate Hasse diagram
  links <- get_hasse_diag(df, pref)
  nodes <- as.character(1:nrow(df))
  
  if (use_dot) {
    
    g <- graph::graphNEL(nodes = nodes, edgemode = "directed")
    
    if (nrow(links) > 0) {
      for (i in 1:nrow(links)) {
        g <- graph::addEdge(as.character(links[i,1]), as.character(links[i,2]), g, 1)
      }
    }
    
    return(g)
    
  } else { # use igraph
    
    # Create graph with all the vertices
    g <- igraph::graph.empty()
    g <- g + igraph::vertices(nodes)
    
    # Add edges for Better-Than-Graph
    g <- g + igraph::graph.edgelist(matrix(as.character(links), nrow(links), ncol(links)))
    
    # Calculate root (maxima)
    root <- as.character(psel.indices(df, pref))
    
    # Create layout
    layout <- igraph::layout_as_tree(g, root = root, flip.y = !flip.edges)
    
    # Return igraph graph and its layout
    return(list(graph = g, layout = layout))
  }
}


# Get dot string for preference graph 
# If file is not NULL, write output to file
#' @rdname plot_btg
#' @export
get_btg_dot <- function(df, pref, labels = 1:nrow(df), flip.edges = FALSE, 
                        levelwise = TRUE, file = NULL) {
  
  check.plot.base(df, pref, labels)
  
  if (flip.edges) pref <- -pref
  
  links <- get_hasse_diag(df, pref)
  nodes <- as.character(1:nrow(df))
  
  # Apply resulting in strings and collapsing it
  joinstrapply <- function(lst, fun) {
    paste(vapply(lst, fun, ''), collapse = "")
  }
  
  # Init graph
  output <- 'digraph G {\n'
  
  # flip.edges => Build graph from bottom to top
  if (flip.edges) output <- paste0(output, 'rankdir = BT\n')
  
  if (levelwise) {
    
    # Each layer has its one row
    
    # Get levels
    df_lev <- psel.indices(df, pref, top = nrow(df), show_level = TRUE)
    
    # Generate subgraphs
    subgraphs <- lapply(1:max(df_lev$.level), function(l) {
      nodes <- as.character(df_lev[df_lev$.level == l, '.indices'])
      paste0("{\nrank=same;\n",
             joinstrapply(nodes, function(x) paste0(x, ";\n")),
             "}\n")
    })
    
    output <- paste0(output, paste(subgraphs, collapse = ""))
    
  } else { 
    
    # Just Maxima are one layer
    max_nodes <- as.character(psel.indices(df, pref))
    
    # Maxima as first layer
    output <- paste0(output, "{\nrank=same;\n",
                     joinstrapply(max_nodes, function(x) paste0(x, ";\n")),
                     "}\n")
  }
  
  # Labels
  output <- paste0(output, joinstrapply(1:nrow(df), function (i)
    paste0('"', as.character(i), '" [label="', labels[i], '"]\n')))
  
  # Edges
  output <- paste0(output, paste(apply(links, 1, function(x) 
    paste0(as.character(x[1]), ' -> ', as.character(x[2]), ';\n')),
    collapse = ""))
  
  # Finalize graph
  output <- paste0(output, '}')
  
  # Return string or write to file
  if (is.null(file)) 
    return(output)
  else
    write(output, file)
  
}


# Internal check before plotting
check.plot.base <- function(df, pref, labels = NULL) {
  
  # check for df/pref
  df.pref.check(df, pref)
  
  # Stop if empty df
  if (nrow(df) == 0) stop.syscall("No nodes for plotting available (empty data set)")
  
  if (length(labels) > 0) {
    ind <- anyDuplicated(labels)
    if (ind > 0)
      warning(paste0("The labels are not unique! First duplicated label is '", labels[ind], "'."))
  }
}


# ---------------------------------------------------------------------------------------------

#' Adjacency List of Hasse diagramm
#' 
#' Returns the adjacency list of the Hasse diagram of a preference as an (n x 2) matrix. 
#' This is the transitive reduction of the preference relation.
#' 
#' @param df A data frame.
#' @param pref A preference on the columns of \code{df}, see \code{\link{psel}} for details.
#' 
#' @details
#' 
#' A row (i, j) in the resulting matrix means that \code{df[i,]} is better than \code{df[j,]} with regard to the preference \code{p}.
#' The matrix is the transitive reduction (Hasse diagram) of the induced relations,
#' i.e., if (1,2) and (2,3) occur in the result, then (1,3) will not be contained.
#' The number of rows in the result depends on the number of non-transitive Better-Than-Relationships in \code{df} w.r.t. \code{p}.
#' 
#' @seealso \code{\link{get_btg}} to plot the Hasse diagram.
#' 
#' @examples
#' 
#' get_hasse_diag(mtcars, low(mpg))
#' 
#' @export 
get_hasse_diag <- function(df, pref) {
  
  df.pref.check(df, pref)
  
  # Calculate Hasse diagramm for pref on df
  res <- get_scores(pref, 1, df)
  scores <- res$scores
  pref_serial <- pserialize(res$p)
  links <- t(get_hasse_impl(scores, pref_serial)) + 1
  return(links)
}


#' Pareto Front Plot
#' 
#' Connects the points of a Pareto front (also known as Pareto frontier) and hence visualizes the dominance region of a Skyline.
#' 
#' @param df The data frame for which the Pareto front is plotted. This may be already a maximal set w.r.t. the preference \code{pref}, 
#'           but anyway the maximal set is recalculated via \code{psel(df, pref)}.
#' @param pref The preference representing the Skyline goals. This must be a Pareto composition (\code{p1 * p2}) or
#'                intersection composition (\code{p1 | p2}) of 
#'             two \code{\link{low}} or \code{\link{high}} preferences.
#' @param ... Additional graphic parameters which are passed to the \code{\link{segments}} function (internally used to plot the front).
#'             
#' @details
#'             
#' \code{plot_front} assumes that there is an existing plot, where the value of the first preference was plotted as x-coordinate
#' and the value of the second preference as y-coordinate.
#' 
#' Note that \code{plot_front} is only recommended if you want to use the plotting functionality from base R. 
#' If you prefer to use ggplot2, we recommend using \code{geom_step} for plotting the Pareto front.
#' See \code{vignette("visualization", package = "rPref")} for examples.
#' 
#' @examples
#' 
#' # plots Pareto fronts for the hp/mpg values of mtcars
#' show_front <- function(pref) {
#'   plot(mtcars$hp, mtcars$mpg)
#'   sky <- psel(mtcars, pref)
#'   plot_front(mtcars, pref, col = rgb(0, 0, 1))
#'   points(sky$hp, sky$mpg, lwd = 3)
#' }
#'
#' # do this for all four combinations of Pareto compositions
#' show_front(low(hp)  * low(mpg))
#' show_front(low(hp)  * high(mpg))
#' show_front(high(hp) * low(mpg))
#' show_front(high(hp) * high(mpg))
#' 
#' # compare this to the front of a intersection preference
#' show_front(high(hp) | high(mpg))
#' 
#' 
#' @importFrom graphics par segments
#' 
#' @export
plot_front <- function(df, pref, ...) {
  
  df.pref.check(df, pref)
  
  # Check if appropriate preference
  if (!(   is.binarycomplexpref(pref) && (pref@op == '*' || pref@op == '|')
        && (is.lowpref(pref@p1) || is.highpref(pref@p1))
        && (is.lowpref(pref@p2) || is.highpref(pref@p2))  ))
    stop("The plot_front function can only be applied to a 2-dimensional pareto preference of low/high preferences!")
    
  
  maxima <- psel(df, pref)
  
  # Get evaluated expressions (similar to score values, but for "high" we have to negate)
  scores <- get_scores(pref, 1, maxima)$scores
  if (is.highpref(pref@p1)) scores[,1] <- -scores[,1]
  if (is.highpref(pref@p2)) scores[,2] <- -scores[,2]
  
  # Get bounding box of current plot
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  
  if (is.highpref(pref@p1) && is.highpref(pref@p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], -scores[,2]),]
    
    # Plot segments
    segments(c(xmin, scores[-nrow(scores),1]), scores[,2], scores[,1], scores[,2], ...)
    segments(scores[,1], scores[,2], scores[,1], c(scores[-1,2], ymin), ...)
    
  } else if (is.lowpref(pref@p1) && is.lowpref(pref@p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], -scores[,2]),]
    
    # Plot segments
    segments(scores[,1], scores[,2], c(scores[-1,1], xmax), scores[,2], ...)
    segments(scores[,1], c(ymax, scores[-nrow(scores),2]), scores[,1], scores[,2], ...)
  
  } else if (is.lowpref(pref@p1) && is.highpref(pref@p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], scores[,2]),]
    
    # Plot segments
    segments(scores[,1], scores[,2], c(scores[-1,1], xmax), scores[,2], ...)
    segments(scores[,1], c(ymin, scores[-nrow(scores),2]), scores[,1], scores[,2], ...)
  
  } else if (is.highpref(pref@p1) && is.lowpref(pref@p2)) {
    
    # Sort by x in ascending order and y in ascending order
    scores <- scores[order(scores[,1], scores[,2]),]
    
    # Plot segments
    segments(c(xmin, scores[-nrow(scores),1]), scores[,2], scores[,1], scores[,2], ...)
    segments(scores[,1], c(scores[-1,2], ymax), scores[,1], scores[,2], ...)
    
  }  
}


  
  