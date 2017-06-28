\name{piHatSummaryFn}
\alias{piHatSummaryFn}

\title{
Create a summary of piHat
}
\description{
This function creates a summary of piHat, the estimated probability of getting disease in the duration of 
the study for each risk group.
}
\usage{
piHatSummaryFn(baseArgs = FALSE, extraArgs = FALSE)
}

\arguments{
  \item{baseArgs}{
A list that has class attribute 'baseArgs'.  This list hase named elements:
e, t, r, c, k, K, epsilon, tStar, ungrouped, N, n, rSummary, nBootstraps,
multicore, verbose.  This list is the output of the function \code{baseArgsFn}.
The elements of this list are the individual components that the low-level
functions in 'rmap' require.  A description of each of these 15 elements can
be found in \code{?baseArgsFn}.

}
  \item{extraArgs}{
A list of named elements, which can contain: gammaHat, lambdaHat, piHat,
Sigma, crp, and auc. This argument allows this function to take a "shortcut",
using intermediate values that have previously been calculated.

}
}
\details{
This function is designed to be called by \code{riskValidateInternalFn}.  The 
summary data frame that is returned by this data frame is one of the list 
outputs of \code{riskValidate} (and therefore \code{riskValidateExternalFn} and 
\code{riskValidateInternalFn}). If necessary, \code{riskValidateInternalFn} will append 
bootstrap confidence intervals as additional columns on this data.frame. 
}
\value{
A data.frame with a row for every risk group and six columns:
\itemize{
  \item r: The risk summary for each risk group.  See argument \code{rSummary} in \code{riskValidate}
  \item piHat: The piHat value for each risk group.  
  \item sigma:  The standard deviation for each piHat value
  \item lower:  The lower bound of a 95\% confidence interval for piHat
  \item upper:  The upper bound of a 95\% confidence interval for piHat
  \item inCI:  'yes' if the risk summary for a risk group (see column 1) is in the bounds of the 95\% 
                confidence interval for piHat (see columns 4 and 5). 'no' otherwise. 
}
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
set.seed(1)
d = df_twoStage(KKK = 3)
ba = baseArgsFn(e = d$d$e, t = d$d$t, r = d$d$r, design = list(c = d$d$c, N = d$N), 
  riskGroup = list(k = d$d$k), rSummary = "median", bootstrap = FALSE)
piHatSummaryFn(ba)

}
