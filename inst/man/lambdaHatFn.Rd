\name{lambdaHatFn}
\alias{lambdaHatFn}

\title{
Compute lambdaHat, NAR, DDD, tau, and denom for each risk group.
}
\description{
This function is one of the first internal calculations that occur inside \code{riskValidate} and
\code{performanceDifference} (along with gammaHatFn).  This function puts together 
several data structures that are used in subsequent calculations inside \code{riskValidate}
and \code{performanceDifference}, namely in uuuFn and VVVFn. 
}
\usage{
lambdaHatFn(baseArgs)
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

}
\details{
This function is called internally by \code{riskValidate} and \code{performanceDifference}.  The 
typical user will not have to call this function directly. 

}
\value{
Define 'K' to be the number of risk groups for a given model (as defined in \code{riskValidate} or 
\code{performanceDifference}). Define 'k' to be one of the K risk groups.
The result of this function is a list with K elements, with
names "k1", ..., "kK".  Each of these K elements is a list with five named elements: 
lambdaHat, NAR, DDD, tau, denom.  Below is a description of these five elements for the kth element.
\itemize{
 \item \code{tau}: Ordered failure times for current risk group.  It is the vector of unique 
                   times for subjects in risk group k for which that subject's event (e) is 1 (disease) or 
                   2 (death from other causes).  The length of tau is M_k.
                   See equation (23) in "rmap-formulas-v01.pdf" from the website.
 \item \code{NAR}: Number at Risk at times tau. A logical matrix with one row for each subject in risk 
                   group k and one column for each tau.  The (n, m)th element of NAR is TRUE if the n-th 
                   subject in risk group k is at risk at time tau_m. In other words, NAR[n, m] is TRUE 
                   if t_n >= tau_m. 
                   See equation (29) in "rmap-formulas-v01.pdf" from the website.
 \item \code{DDD}: Disease or Death at times tau.  DDD is a list containing two elements.  
                   The first element is for e = 1 disease), and the second element is for e = 2 
                   (death from other causes).  Both elements in DDD will be a logical matrix with one row 
                   for each subject in risk group k and one column for each tau.  The (n, m)th element of 
                   DDD[[1]] will be TRUE if e_n == 1 and t_n == tau_m, and FALSE otherwise.  Likewise, the 
                   (n, m)th element of DDD[[2]] will be TRUE if e_n == 2 and t_n == tau_m, and FALSE 
                   otherwise.  
		   See equation (30) in "rmap-formulas-v01.pdf" from the website.
 \item \code{denom}: The number at risk at times tau.  If using random sampling, this will simply be the 
                   column-wise sums of NAR.  If using two-stage sampling, these values will be weighted 
                   according to two-stage sampling weights.  
 		   See equation (61) in "rmap-formulas-v01.pdf" from the website.
 \item \code{lambdaHat}:  The estimated hazard rate at times tau for event = 1 (disease) or event = 2 
                   (death from other causes). lambdaHat is a list with K elements.  The kth element is a 
                   numeric matrix with two rows (for e = 1 and e = 2) and M_k columns, where M_k is the 
                   length of tau.
		   See equation (68) "rmap-formulas-v01.pdf" from the website where \\lambda_{kem} is the hazard of e at time 
                   tau_m.

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
  options(digits = 3)
  options(scipen = 10)
  set.seed(5)
  ddd = df_twoStage(NTotal = 10, KKK = 2)
  d = ddd$d
  d[9,] = d[1,]
  d[9,]$e = 0
  d[9,]$k = 1
  d[1,]$e = 0
  d[7,]$e = 0
  d[3,]$e = 2
  d[8,]$e = 0
  d[8,]$c = "B"
  rownames(d) = NULL
  d  #A very doctored data set that will give a tiny example with quite general features.
  N = ddd$N
  N["B"] = 6
  N  #First stage numbers
  n = ddd$n
  n["B"] = 3
  n  #Second stage numbers
  d_k_equals_1 = cbind(d[d$k == 1,c("e", "t", "c")], aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], k = 1)
  d_k_equals_1 #The relevant columns of d for k = 1.
  d_k_equals_2 = cbind(d[d$k == 2,c("e", "t", "c")], aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], k = 2)
  d_k_equals_2 #Ditto for k = 2.
  baseArgs = baseArgsFn(e = d$e, t = round(d$t,2), r = round(d$r,2), design = list(N = N, c = d$c),
    riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
  lambdaHat = lambdaHatFn(baseArgs)
  names(lambdaHat)
  names(lambdaHat$k1)
  lambdaHat[["k1"]]$tau
  lambdaHat[["k1"]]$lambdaHat
  lambdaHat[["k2"]]$tau
  lambdaHat[["k2"]]$lambdaHat
}
