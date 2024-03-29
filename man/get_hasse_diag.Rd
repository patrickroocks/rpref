% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.r
\name{get_hasse_diag}
\alias{get_hasse_diag}
\title{Adjacency List of Hasse diagram}
\usage{
get_hasse_diag(df, pref)
}
\arguments{
\item{df}{A data frame.}

\item{pref}{A preference on the columns of \code{df}, see \code{\link{psel}} for details.}
}
\description{
Returns the adjacency list of the Hasse diagram of a preference as an (n x 2) matrix. 
This is the transitive reduction of the preference relation.
}
\details{
A row (i, j) in the resulting matrix means that \code{df[i,]} is better than \code{df[j,]} with regard to the preference \code{p}.
The matrix is the transitive reduction (Hasse diagram) of the induced relations,
i.e., if (1,2) and (2,3) occur in the result, then (1,3) will not be contained.
The number of rows in the result depends on the number of non-transitive Better-Than-Relationships in \code{df} w.r.t. \code{p}.
}
\examples{

get_hasse_diag(mtcars, low(mpg))

}
\seealso{
\code{\link{get_btg}} to plot the Hasse diagram.
}
