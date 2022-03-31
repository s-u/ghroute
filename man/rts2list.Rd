\name{rts2list}
\alias{rts2list}
\title{Split route result matrix by index}
\description{
\code{rts2list} converts route result matrix into a list of individual
matrices based on the supplied index which is typically part of the
matrix.
}
\usage{
rts2list(rts, nrow, index = m[, 3])
}
\arguments{
  \item{rts}{matrix}
  \item{nrow}{total number of input rows}
  \item{index}{index mapping rows of the matrix to result rows}
}
\details{
The matrix \code{rts} is split into indiviudal matrices according to
the \code{index}. It is similar to \code{split(rts, index)} except
that the result has exactly \code{nrow} entries corresponding the the
indices 1 .. \code{nrow} and index must be contiguous. Any missing
entries (i.e., indices between 1 and \code{nrow} which are not in
\code{index}) are filled with \code{NULL}.
}
\value{
A list of exactly \code{nrow} elements. The entries are either
\code{NULL} (index not present) or the corresponding slice of the
matrix.
}
\author{
Simon Urbanek
}
\examples{
m = matrix(1:10,, 2)
rts2list(m, 3, c(1,1,1,3,3))

## route matrices have the index as 3rd column
m = cbind(m, c(1,1,1,3,3))

## let's say the input had 5 pairs
rts2list(m, 5)

}
\keyword{manip}
