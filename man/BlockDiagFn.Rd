\name{BlockDiagFn}
\alias{BlockDiagFn}

\title{
BlockDiagFn
}
\description{
Computes a block diagonal matrix given a list of matrices.
}
\usage{
BlockDiagFn(boxes)
}

\arguments{
  \item{boxes}{
   A list of boxes (matrices) to combine to make the block diagonal matrix
}
}
\details{
Credit to simex package for this algorithm
}
\value{
Returns a block diagonal matrix.
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html

}
\author{
David Johnston and Gail Gong

}
\note{

}



\seealso{

}
\examples{
a = matrix(1, 2, 2)
b = matrix(2, 3, 3)
c = matrix(3, 4, 5)
BlockDiagFn(list(a, b, c))

}
