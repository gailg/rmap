\name{caseRiskPercentiles}
\alias{caseRiskPercentiles}

\title{
Calculate the risk percentiles of cases of risk models.
}
\description{
This function is produces case risk percentiles and for comparing two risk models scatterplots of case risk percentiles. This function assumes random samples (not two-stage samples)..
}
\usage{
caseRiskPercentiles(cutoff, e, t, tStar, r, rAnother, ...)
}

\arguments{
  \item{cutoff}{
A real number between 0 and 1.
}
  \item{e}{
A vector of events for each subject in the dataset.  The event for one subject can either 
be a 0 (censored), 1 (disease) or 2 (death from other causes)
}
  \item{t}{
A vector of times until event for each subject in the dataset.  This is the time that the event 'e' happened. 
 This time must fall in the interval [0, t*), where t* is the duration of the study. 
}
  \item{r}{
A vector of assigned risks.
}
  \item{rAnother}{
Another vector of assigned risks. If this is missing, then results for only one model will be output; no scatterplot will be produced.
}
  \item{dot-dot-dot}{
pars to be passed to plot.  See help(par).
}

}
\details{

}
\value{
A scatterplot together with a named list containing:  
\enumerate{
  \item "N_cases": The number of cases found.  Cases are defined as outcome positive persons (e == 1)

  \item "the_proportion_of_points_below_diagonal": The proportion of the cases for which the model r crp is bigger than the model rAnother crp.

  \item "the_proportion_of_crps_above_cutoff_for_model_r": The proportion of the cases whose model r crp is bigger than cutoff.

  \item "the_proportion_of_crps_above_cutoff": Ditto.

  \item "the_proportion_of_crps_above_cutoff_for_model_rAnother": The proportion of the cases whose model rAnother crp is bigger than cutoff.

  \item "crp1": A vector containing the crps of model r.  

  \item "crp2": A vector containing the crps of mode rAnother.  


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
data(data_set_score_statistics)
xxx = data_set_score_statistics
tail(xxx)
cutoff = 0.6
tStar = 10
e = xxx$e
t = xxx$t
r = xxx$r_1
rAnother = xxx$r_2
crp = caseRiskPercentiles(
  cutoff, e, t, tStar, r, rAnother, 
  main = "Case risk percentiles",
  xlab = "Risk model 1", ylab = "Risk model 2",
  cex = 1, pch = 16)
crp

# In case you want to look at the ecdfs of the two vectors of crps.
F1 = ecdf(crp$crp1)
F2 = ecdf(crp$crp2)
plot(F1, verticals = TRUE, do.points = FALSE, col = "red", 
     xlim = c(0, 1),
     main = "Case Risk Percentiles CDF")
plot(F2, verticals = TRUE, do.points = FALSE, col = "blue", add = TRUE)
legend(x = 0, y = 0.95, legend = c("Risk model 1", "Risk model 2"), 
       col = c("red", "blue"),
       lty = c(1, 1))

# For just one risk model
crp = caseRiskPercentiles(cutoff, e, t, tStar, r)
crp
}
