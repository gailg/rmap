\name{prob_CI_Fn}
\alias{prob_CI_Fn}

\title{
Calculate the 95\% confidence interval of a probability
}
\description{
Computes the lower and upper limits of a 95\% confidence interval for a probability
}
\usage{
prob_CI_Fn(ps, sds)
}

\arguments{
  \item{ps}{
  A numeric vector of probabilities 
}
  \item{sds}{
  A numeric vector of standard deviations.  The elements in this vector should correspond to the elements in \code{ps} above.  
}
}
\details{

}
\value{
A matrix with two columns: 'lower' and 'upper'.  There will be a row for each probability and standard deviation.
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


\examples{
prob_CI_Fn(ps = 0.99, sds = 0.01)
prob_CI_Fn(ps = c(0.98, 0.99), sds = c(0.01, 0.01))

set.seed(1)
someCIs = prob_CI_Fn( ps = (1:99) / 100, sds = runif(99, 0.01, 0.05) )
}
