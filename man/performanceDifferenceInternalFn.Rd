\name{performanceDifferenceInternalFn}
\alias{performanceDifferenceInternalFn}

\title{
Measure the difference in performance between two models
}
\description{
This function measures the difference in performance between two models.  It 
is an internal function that is designed to be called from \code{performanceDifference}.
}
\usage{
performanceDifferenceInternalFn(baseArgs1 = FALSE, baseArgs2 = FALSE)
}

\arguments{
  \item{baseArgs1}{
The "baseArgs" for the first model.  See \code{?baseArgs}
}
  \item{baseArgs2}{
The "baseArgs" for the second model.  See \code{?baseArgs}
}
}
\details{
See \code{?performanceDifference} for the user-friendly function.
}
\value{

A list that has a class attribute 'baseArgs'.  This list has named elements: e, t, r, c, k, K, N, n, rSummary, nBootstraps.  
These are the individual components that the internal functions in this package require.  Below is a description of each of the list elements:

\itemize{
  \item "e": A vector of events for each subject in the dataset.  The event for one subject can either 
             be a 0 (censored), 1 (disease) or 2 (death from other causes)
  \item "t": A vector of times until event for each subject in the dataset.  This is the time that the event 'e' happened. 
             This time must fall in the interval [0, t*), where t* is the duration of the study. 
  \item "r": A vector of risk values for each subject in the dataset.  This is the probability of disease, 
             as predicted by a risk model.  The primary concern of this 'rmap' package is to assess the validity of 
             such risk models.
  \item "c": A vector of sampling categories for each subject in the data.  The two-stage sampling category
             should be specified by capital letters, starting with "A", "B", ....  If 'design' is 
             "randomSample", this vector is comprised of "A"'s.
  \item "k": A vector of risk group assignments for each subject in the dataset.  This vector can be given 
             by the user or calculated by baseArgsFn.  Each element in this vector is in {1, ..., K}, where 
             K is defined below.
  \item "K": An integer describing the number of risk groups.
  \item "N": A named vector, with one element for each sampling category.  The names of this vector
             should be the capital letters "A", "B", ..., from list element 'c' above.  
             This vector holds the number of subjects (from the first stage) that fell into each category.
  \item "n": A named vector, with one element for each sampling category.  The names of this vector
             should be the same as the names of N.  This vector holds the number of subjects resampled in each category.
  \item "rSummary": A vector of risk summaries, one for each risk group.  This vector can be given
             by the user or calculated by baseArgsFn using method "mean", "median", or "midpoint".
  \item "nBootstraps": An integer describing the number of bootstraps, where 0 indicates no bootstraps.
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
\code{?performanceDifference}
}
\examples{
set.seed(1)
twoMod = df_twoStage_r1_r2()
baseArgs1 = baseArgsFn(e = twoMod$d$e, t = twoMod$d$t, r = twoMod$d$r1, 
                       design = list(c = twoMod$d$c, N = twoMod$N),
                       riskGroup = list(K = 2), rSummary = "mean", bootstrap = 300)
baseArgs2 = baseArgsFn(e = twoMod$d$e, t = twoMod$d$t, r = twoMod$d$r2, 
                       design = list(c = twoMod$d$c, N = twoMod$N),
                       riskGroup = list(K = 2), rSummary = "mean", bootstrap = 300)

pd = performanceDifferenceInternalFn(baseArgs1 = baseArgs1, baseArgs2 = baseArgs2)
pd


}
