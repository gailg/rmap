\name{riskValidateInternalFn}
\alias{riskValidateInternalFn}

\title{
Validate a risk model for cohort data (internal)
}
\description{
This function is very similar to the function \code{riskValidateExternalFn}.  
Unlike \code{riskValidateExternalFn}, this function expects its inputs to be 
"massaged" by baseArgsFn.  The function \code{riskValidate} is the top 
level function and is designed to be called by package users.  If you are a user
of this package, chances are you want to use the function \code{riskValidate} 
and not this function.
}
\usage{
riskValidateInternalFn(baseArgs = FALSE, extraArgs = FALSE)
}

\arguments{
  \item{baseArgs}{
A list that has class attribute 'baseArgs'.  This list hase named elements:
e, t, r, c, k, K, epsilon, tStar, ungrouped, N, n, rSummary, nBootstraps,
multicore, and verbose.  This list is the output of the function \code{baseArgsFn}. 
The elements of this list are the individual components that the low-level 
functions in 'rmap' require.  A description of each of these 15 elements can 
be found in \code{?baseArgsFn}.

}
  \item{extraArgs}{
A list of named elements, which can contain: gammaHat, lambdaHat, piHat, and 
Sigma. This argument allows this function to take a "shortcut", 
using intermediate values that have previously been calculated.

}
}
\details{
\code{riskValidate} calls \code{riskValidateExternalFn}, 
and \code{riskValidateExternalFn} 
calls \code{riskValidateInternalFn}. 
}
\value{
    A list with several elements.  There will be 9 elements in this output 
    without using bootstrapping, or 11 with bootstrapping (see 'bootstrap' 
    argument).  
    \itemize{
      \item 'gammaHat': Estimated proportion of subjects in each risk group
      \item 'piHat': Estimated value of pi for each risk group.  pi is the 
        probability of getting disease in (0, t*], where t* is the duration 
        of the study
      \item 'Sigma': Define K as the number of risk groups.  Sigma is the 
        covariance matrix for the vector 
      	(gammaHat[1], ..., gammaHat[K-1], piHat[1], ..., piHat[K]).  
      \item 'piHatSummary':  A data frame with summary information for piHat.  
        See \code{help(piHatSummaryFn)} for more information.
      \item 'ChiSq': The Hosmer-Lemeshow Chi-squared goodness-of-fit statistic 
        and corresponding p-value 
      \item 'AUC': Area Under the Receiver Operating Characteristic curve
      \item 'AUC_CI': A 95\% confidence interval for the AUC
      \item 'AUC_CI_boot': A 95\% confidence interval for the AUC 
        (only reported if bootstrapping was used)
      \item 'SD': A measure of the standard deviation of the risk model
      \item 'SD_CI': A 95\% confidence interval for tau
      \item 'SD_CI_boot': A 95\% bootstrap confidence interval for tau 
        (only reported if bootstrapping was used)
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
\code{riskValidate}, \code{riskValidateExternalFn}
}
\examples{
# random sampled data:
set.seed(1)
d_rs = df_randomSample()
ba = baseArgsFn(
  e = d_rs$e, t = d_rs$t, r = d_rs$r, 
  design = "randomSample", riskGroup = list(K = 3), 
  rSummary = "median", bootstrap = FALSE)

rv1 = riskValidateInternalFn(ba)

# two-stage sampled data:
set.seed(1)
d_ts = df_twoStage()
ba = baseArgsFn(e = d_ts$d$e, t = d_ts$d$t, r = d_ts$d$r,
  design = list(c = d_ts$d$c, N = d_ts$N), 
  riskGroup = list(k = d_ts$d$k),
  rSummary = "median", bootstrap = FALSE)

rv2 = riskValidateInternalFn(ba)

}
