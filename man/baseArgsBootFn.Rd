\name{baseArgsBootFn}
\alias{baseArgsBootFn}

\title{
Transforms user input into baseArgsBoot
}
\description{
This function translates arguments given by the user into a data structure of 
class "baseArgsBoot".  This resulting data structure is used internally in 
\code{performanceDifferenceInternalFn} and \code{sdBootRiskValidateInternalFn}.
}
\usage{
baseArgsBootFn(baseArgs)
}

\arguments{
  \item{baseArgs}{
A list that has class attribute 'baseArgs'.  This list has named elements:
e, t, r, c, k, K, epsilon, tStar, ungrouped, N, n, rSummary, nBootstraps, 
multicore, verbose.  This list is the output of the function \code{baseArgsFn}.
The elements of this list are the individual components that the low-level 
functions in 'rmap' require.  A description of each of these 15 elements can 
be found in \code{?baseArgsFn}.
}
}
\details{
In \code{performanceDifferenceInternalFn} it is important that baseArgsBoot1 
and BaseArgsBoot2 use the same indices.  Therefore, baseArgsBoot returns 
indices as one of its outputs. See the function definition of 
\code{performanceDifferenceInternalFn} for more details. 
}

\value{
A list that has a class attribute 'baseArgs'.  This list has named elements: 
e, t, r, c, k, K, esilon, tStar, ungrouped, N, n, multicore, verbose, indices, 
indicesTwoStage.  These are the individual components that \code{AUC_CI_Fn},
\code{performanceDifferenceInternalFn}, \code{piHatNNInternalFn}, and 
\code{sdBootRiskValidateInternalFn} use.  Below is a description of each of 
the list elements:

\itemize{
  \item "e": 
    A vector of events for each subject in the dataset.  The event for one 
    subject can either be a 0 (censored), 1 (disease) or 2 (death from other 
    causes).
  \item "t": 
    A vector of times until event for each subject in the dataset.  
    This is the time that the event 'e' occurred. This time must fall in the 
    interval [0, t*), where t* is the duration of the study. 
  \item "r": 
    A vector of risk values for each subject in the dataset.  This is the 
    probability of disease, as predicted by a risk model.  The primary concern 
    of this 'rmap' package is to assess the validity of such risk models.
  \item "c": 
    A vector of sampling categories for each subject in the data.  
    The two-stage sampling category should be specified by capital letters, 
    starting with "A", "B", ....  If 'design' is "randomSample", this vector 
    is comprised of "A"'s.
  \item "k": 
    If ungrouped = FALSE, k is a vector of risk group designations, one for each 
    subject in the dataset.  
    This vector can be given by the user or calculated by baseArgsFn.  
    Each element in this vector is in {1, ..., K}, where K is defined below.
    If ungrouped = TRUE, k is a vector of length equal to the number of subjects
    in the data set and all entries in the vector are equal to 1.
  \item "K": 
    If ungrouped = FALSE, K is an integer describing the number of risk groups.
    If ungrouped = TRUE, K = 1
  \item "epsilon":
    If ungrouped = FALSE, epsilon = NA. If ungrouped = TRUE, epsilon is a 
    number in (0, 1] which determines the number of observatyions in each 
    neigbhorhood in the nearest neighbor calculation of outcome probabilities.
  \item "tStar":
    If ungrouped = FALSE, tStar = NA.  If ungrouped = TRUE, tStar is a positive
    number equal to the duration of the study.  tStar must be at least as large
    as max(t).
  \item "ungrouped:
    A logical value.  If ungrouped = FALSE, then a grouped analysis will be 
    performed using k and K.  If ungrouped = TRUE, then a grouped analysis 
    will be performed using epsilon and tStar.
  \item "N": 
    A named vector, with one element for each sampling category.  The names 
    of this vector should be the capital letters "A", "B", ..., from list 
    element 'c' above.  This vector holds the number of subjects (from the 
    first stage) that fell into each category.
  \item "n": 
    A named vector, with one element for each sampling category.  The names 
    of this vector should be the same as the names of N.  This vector holds 
    the number of subjects resampled in each category.
  \item "multicore":
    A logical value equal to the input multicore.
  \item "verbose":
    A logical value equal to the input verbose.
  \item "indices": 
    Indices from a randomly-sampled bootstrap.
  \item "indicesTwoStage": 
    Indices from a two-stage sampled bootstrap.  This is a subset of 
    "indices".  
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

  sampleData1 = df_randomSample()
  baseArgs1 = baseArgsFn(
    e = sampleData1$e, t = sampleData1$t, r = sampleData1$r, 
    design = "randomSample", riskGroup = list(K = 3),
    rSummary = "median", bootstrap = FALSE)
  str(baseArgs1)
  str(baseArgsBootFn(baseArgs1))

  sampleData2 = df_randomSample_r1_r2(NTotal = 200)
  tStar = 10
  epsilon = nrow(sampleData2)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
  baseArgs2 = baseArgsFn(e = sampleData2$e, t = sampleData2$t, 
    r = sampleData2$r1, design = "randomSample", riskGroup = riskGroup,
    rSummary = "mean", bootstrap = 20, multicore = FALSE, verbose = TRUE)
  str(baseArgs2)
  str(baseArgsBootFn(baseArgs2))
}
