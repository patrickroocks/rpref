

#' Better-Than-Graph
#' 
#' Returns a Hasse-Diagramm of a preference order (also called the Better-Than-Graph) on a given data set
#' to be plotted with the igraph package or plots the graph directly.
#' 
#' @param df A data frame.
#' @param pref A preference on the columns of \code{df}, see \code{\link{psel}} for details.
#' @param flip.edges (optional) Flips the orientation of edges,
#'        if \code{TRUE} than arrows point from worse nodes to better nodes.
#' @param labels (optional) Labels for the vertices when \code{plot_btg} is used.
#' 
#' @details The function \code{get_btg} returns a list \code{l} with the following list entries:
#' 
#' \describe{
#'   \item{\code{l$graph}}{An igraph object, created with the \code{\link{igraph}} package.}
#'   \item{\code{l$layout}}{A typical Hasse-diagram layout for plotting the graph, also created with igraph.}
#' }
#' 
#' To plot the resulting graph returned from \code{get_btg}, use the \code{plot} function as follows: 
#' 
#' \code{plot(l$graph, layout = l$layout)}. 
#' 
#' For more details, see \code{\link{igraph.plotting}} and the examples below.
#' The function \code{plot_btg} directly plots the Better-Than-Graph 
#' some defaults values for e.g., vertex size.
#' 
#' The Hasse diagram of a preference visualizes all the better-than-relationsships on a given data set.
#' All edges which can be retrieved by transitivity of the order are omitted.
#' 
#' By default, the arrows in the diagram point from better to worse nodes w.r.t. the preference. 
#' This means an arrow can be read as "is better than". If \code{flip.edges = TRUE} is set, 
#' then the arrows point from worse nodes to better nodes ("is worse than"). 
#' In any case, the better nodes are plotted at the top and the worse nodes at the bottom of the diagram.
#' 
#' The names of the vertices are characters ranging from \code{"1"} to \code{as.character(nrow(df))} 
#' and they correspond to the row numbers of \code{df}. 
#' By default, these are also the labels of the vertices. 
#' Alternatively, they can be defined manually in the \code{plot} function or 
#' using the \code{labels} parameter of \code{plot_btg}.
#' 
#' 
#' @seealso \code{\link{igraph.plotting}}
#' 
#' @examples
#' 
#' # pick a small data set and create preference and BTG 
#' df <- mtcars[1:10,]
#' pref <- high(mpg) * low(wt)
#' 
#' # directly plot the BTG with row numbers as labels
#' plot_btg(df, pref) 
#' 
#' # create the BTG and labels for the nodes with relevant values
#' btg <- get_btg(df, pref)
#' labels <- paste0(df$mpg, "\n", df$wt)
#' 
#' # plot the graph using igraph
#' library(igraph)
#' plot(btg$graph, layout = btg$layout, vertex.label = labels,
#'      vertex.size = 25)
#' 
#' # add colors for the maxima nodes and plot again
#' colors <- rep(rgb(1, 1, 1), nrow(df))
#' colors[psel.indices(df, pref)] <- rgb(0,1,0)
#' plot(btg$graph, layout = btg$layout, vertex.label = labels,
#'      vertex.size = 25, vertex.color = colors)
#' 
#' # show lattice structure of 3-dimensional Pareto preference
#' df <- merge(merge(data.frame(x = 1:3), data.frame(y = 1:3)), data.frame(z = 1:2))
#' labels <- paste0(df$x, ",", df$y, ",", df$z)
#' btg <- get_btg(df, low(x) * low(y) * low(z))
#' plot(btg$graph, layout = btg$layout, vertex.label = labels, 
#'      vertex.size = 20)
#'
#' 
#' @export
get_btg <- function(df, pref, flip.edges = FALSE) {
  
  # Arrows from worse to better?
  if (flip.edges) pref <- -pref

  # calculate Hasse diagram
  links <- get_hasse_diag(df, pref)
  
  # Create graph with all the vertices
  g <- graph.empty()
  g <- g + vertices(as.character(1:nrow(df)))

  # Add egdes for Better-Than-Graph
  g <- g + graph.edgelist(matrix(as.character(links), nrow(links), ncol(links)))

  # Calculate root (maxima)
  root <- as.character(psel.indices(df, pref))
     
  # Create layout
  layout <- layout_as_tree(g, root = root, flip.y = !flip.edges)
  
  # Return everything
  return(list(graph = g, layout = layout))
}

#' @rdname get_btg
#' @export
plot_btg <- function(df, pref, labels = 1:nrow(df), flip.edges = FALSE) {
  # Get BTG
  btg <- get_btg(df, pref, flip.edges)
  
  # Plot BTG using some reasonable defaults
  plot(btg$graph, layout = btg$layout, vertex.label = as.character(labels), 
       vertex.color = 'white', vertex.size = 20, vertex.label.cex = 1.5,
       edge.color = 'black', vertex.label.color = 'black')
}

#' Adjacency List of Hasse Diagramm
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
#' @seealso \code{\link{get_btg}} to plot the Hasse Diagram.
#' 
#' @examples
#' 
#' get_hasse_diag(mtcars, low(mpg))
#' 
#' @export 
get_hasse_diag <- function(df, pref) {
  # Calculate Hasse-Diagramm for pref on df
  scores <- pref$get_scorevals(1, df)$scores
  pref_serial <- pref$serialize()
  links <- t(get_hasse_impl(scores, pref_serial)) + 1
  return(links)
}


#' Pareto Front Plot
#' 
#' Connects the points of a Pareto front (also known as Pareto frontier) and hence visualizes the dominance region of a Skyline.
#' 
#' @param df The data frame for which the Pareto front is plotted. This may be already a maxima set w.r.t. the preference \code{pref}, 
#'           but anyway the maxima set is recalculated via \code{psel(df, pref)}.
#' @param pref The preference representing the Skyline goals. This must be a pareto composition (\code{p1 * p2}) or
#'                intersection composition (\code{p1 | p2}) of 
#'             two \code{\link{low}} or \code{\link{high}} preferences.
#' @param ... Additional graphic parameters which are passed to the \code{\link{segments}} function (internally used to plot the front).
#'             
#' @details
#'             
#' \code{plot_front} assumes that there is an existing plot, where the value of the first preference was plotted as x-coordinate
#' and the value of the second preference as y-coordinate.
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
#' @importFrom graphics par segments
#' 
#' @export
plot_front <- function(df, pref, ...) {
  
  # Check if appropiate preference
  if (!(   is.complexpref(pref) && (pref$op == '*' || pref$op == '|')
        && (is.lowpref(pref$p1) || is.highpref(pref$p1))
        && (is.lowpref(pref$p2) || is.highpref(pref$p2))  ))
    stop("The plot_front function can only be applied to a 2-dimensional pareto preference of low/high preferences!")
    
  
  maxima <- psel(df, pref)
  
  # Get evaluated expressions (similar to score values, but for "high" we have to negate)
  scores <- pref$get_scorevals(1, maxima)$scores
  if (is.highpref(pref$p1)) scores[,1] <- -scores[,1]
  if (is.highpref(pref$p2)) scores[,2] <- -scores[,2]
  
  # Get bounding box of current plot
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  
  if (is.highpref(pref$p1) && is.highpref(pref$p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], -scores[,2]),]
    
    # Plot segments
    segments(c(xmin, scores[-nrow(scores),1]), scores[,2], scores[,1], scores[,2], ...)
    segments(scores[,1], scores[,2], scores[,1], c(scores[-1,2], ymin), ...)
    
  } else if (is.lowpref(pref$p1) && is.lowpref(pref$p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], -scores[,2]),]
    
    # Plot segments
    segments(scores[,1], scores[,2], c(scores[-1,1], xmax), scores[,2], ...)
    segments(scores[,1], c(ymax, scores[-nrow(scores),2]), scores[,1], scores[,2], ...)
  
  } else if (is.lowpref(pref$p1) && is.highpref(pref$p2)) {
    
    # Sort by x in ascending order and y in descending order
    scores <- scores[order(scores[,1], scores[,2]),]
    
    # Plot segments
    segments(scores[,1], scores[,2], c(scores[-1,1], xmax), scores[,2], ...)
    segments(scores[,1], c(ymin, scores[-nrow(scores),2]), scores[,1], scores[,2], ...)
  
  } else if (is.highpref(pref$p1) && is.lowpref(pref$p2)) {
    
    # Sort by x in ascending order and y in ascending order
    scores <- scores[order(scores[,1], scores[,2]),]
    
    # Plot segments
    segments(c(xmin, scores[-nrow(scores),1]), scores[,2], scores[,1], scores[,2], ...)
    segments(scores[,1], c(scores[-1,2], ymax), scores[,1], scores[,2], ...)
    
  }  
}
  
  