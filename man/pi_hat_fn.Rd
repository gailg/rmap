% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pi_hat_fn.R
\name{pi_hat_fn}
\alias{pi_hat_fn}
\title{pi_hat_fn}
\usage{
pi_hat_fn(baseArgs)
}
\arguments{
\item{baseArgs}{A list provided by \code{baseArgsFn}.
The objects required here are
\code{e}, \code{t},
\code{weight}, \code{tStar}, \code{K}, \code{k}, and
\code{N_nonzero_events}.}
}
\value{
A vector containing one estimated risk per
risk group.
}
\description{
For estimating the outcome probability for each risk group
defined by \code{baseArgs$k}.  Uses \code{baseArgs$weight}
which \code{baseArgsFn} provides for all three 
kinds of sampling: random sampling, two-stage sampling,
and weighted sampling.  This functionreplaces 
\code{piHatFn} which called \code{lambdaHatFn} which
throws an error if there are no non-zero events. 
\code{pi_hat_fn} allows an estimate to be zero. 
\code{Sigma} requires just \code{lambdaFn} 
(and not \code{piHatFn}) and therefore for calculating
theoretical variances, 
rmap requires at least one nonzero event in each 
risk group.
}
\examples{
set.seed(1)
NNN = 100
N_bootstrap_reps = 10
cutoffs = c(0, 0.40, 1)
weighted_example = weighted_example_fn(NNN)
cohort_sampling_probability_dictionary = weighted_example$cohort_sampling_probability_dictionary
cohort_sample = weighted_example$cohort_sample
target_sample = weighted_example$target_sample
tStar = weighted_example$t_star
which_model = "r_B"
cohort_category = cohort_sample$category
target_category = target_sample$category
r = cohort_sample[[which_model]]
e = cohort_sample$eee
t = cohort_sample$ttt
design = list(target_category = target_category, c = cohort_category)
riskGroup = list(cutoffs = cutoffs)
rSummary = "mean"
bootstrap = N_bootstrap_reps
baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
pi_hat_fn(baseArgs)

}
