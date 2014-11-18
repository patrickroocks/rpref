

#' Better-Than-Graph
#' 
#' Returns a Hasse-Diagramm of a preference order (also called the Better-Than-Graph) on a given dataset to be plotted with the igraph package.
#' 
#' @param df A dataframe.
#' @param pref A preference on the columns of \code{df}, see \code{\link{psel}} for details.
#' 
#' @details This function returns a list \code{l} with the following list entries:
#' 
#' \describe{
#'   \item{\code{l$graph}}{An igraph object, created with the \code{\link{igraph}} package.}
#'   \item{\code{l$layout}}{A typical Hasse-diagram layout for plotting the graph, also created with igraph.}
#' }
#' 
#' To plot the resulting graph use the \code{plot} function as follows: \code{plot(l$graph, layout = l$layout)}. 
#' For more details see \code{\link{igraph.plotting}} and the examples below.
#' 
#' The Hasse diagram of a preference visualizes all the better-than-relationsships on a given dataset.
#' All edges which can be retrieved by transitivity of the order are omitted.
#' 
#' The names of the vertices are characters ranging from \code{"1"} to \code{as.character(nrow(df))} and they correspond to the row numbers of \code{df}.
#' 
#' @seealso \code{\link{igraph.plotting}}
#' 
#' @examples
#' 
#' # Pick a small data set and create preference and BTG 
#' df <- mtcars[1:10,]
#' pref <- high(mpg) * low(wt)
#' btg <- get_btg(df, pref)
#' 
#' # Create labels for the nodes with relevant values
#' labels <- paste0(df$mpg, "\n", df$wt)
#' 
#' # Plot the graph using igraph
#' library(igraph)
#' plot(btg$graph, layout = btg$layout, vertex.label = labels,
#'      vertex.size = 25)
#' 
#' # Add colors for the maxima nodes and plot again
#' colors <- rep(rgb(1,1,1), nrow(df))
#' colors[psel.indices(df, pref)] <- rgb(0,1,0)
#' plot(btg$graph, layout = btg$layout, vertex.label = labels,
#'      vertex.size = 25, vertex.color = colors)
#' 
#' # Show lattice structure of 3-dimensional Pareto preference
#' df <- merge(merge(data.frame(x = 1:3), data.frame(y = 1:3)), data.frame(z = 1:2))
#' labels <- paste0(df$x, ",", df$y, ",", df$z)
#' btg <- get_btg(df, low(x) * low(y) * low(z))
#' plot(btg$graph, layout = btg$layout, vertex.label = labels, 
#'      vertex.size = 20)
#'
#' 
#' @export
get_btg <- function(df, pref) {

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
  layout <- layout.reingold.tilford(g, root = root)

  # Return everything
  return(list(graph = g, layout = layout))
}


#' Adjacency list of Hasse diagramm
#' 
#' Returns the adjacency list as a (n x 2) matrix. 
#' This is the transitive reduction of the preference relation.
#' 
#' @param df A dataframe.
#' @param pref A preference on the columns of \code{df}, see \code{\link{psel}} for details.
#' 
#' @details
#' 
#' A row (i, j) in the resulting matrix means that \code{df[i,]} is better than \code{df[j,]} with regard to the preference \code{p}.
#' The matrix is the transitive reduction (Hasse diagram) of the induced relations,
#' i.e. if (1,2) and (2,3) occur in the result, than (1,3) will not be contained.
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
#' @param df The dataframe for which the Pareto front is plotted. This may be already a maxima set w.r.t. the preference \code{pref}, 
#'           but anyway the maxima set is recalculated via \code{psel(df, pref)}.
#' @param pref The preference representing the Skyline goals. This must be a pareto (\code{p1 * p2}) or intersection (\code{p1 | p2}) composition of 
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
#' # Plots Pareto fronts for the hp/mpg values of mtcars
#' show_front <- function(pref) {
#'   plot(mtcars$hp, mtcars$mpg)
#'   sky <- psel(mtcars, pref)
#'   plot_front(mtcars, pref, col = rgb(0,0,1))
#'   points(sky$hp, sky$mpg, lwd = 3)
#' }
#'
#' # Do this for all four combinations of pareto compositions
#' show_front(low(hp)  * low(mpg))
#' show_front(low(hp)  * high(mpg))
#' show_front(high(hp) * low(mpg))
#' show_front(high(hp) * high(mpg))
#' 
#' # Compare this to the front of a intersection preference
#' show_front(high(hp) | high(mpg))
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
  
  