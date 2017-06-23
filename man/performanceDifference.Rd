\name{performanceDifference}
\alias{performanceDifference}

\title{
Measure the difference in performance between two models
}
\description{
This function measures the difference in performance between
two models.  Both models (hereafter called model 1 and model 2) 
are applied to the same dataset, meaning both models will share a similar 
"e", "t", and "design".  A model will calculate an assigned risk for each
subject, along with a risk group assignment.  This function 
is comparing two models, so therefore "rs" and "models" will be 
lists with two elements (one for model 1 and one for model 2).  The
confidence intervals for measures of performance difference are obtained 
using bootstrapping.
}
\usage{
performanceDifference(e, t, rs, 
  design = "randomSample", riskGroups, 
  bootstrap = 100)
}

\arguments{
  \item{e}{
A vector of events for each subject in the dataset.  The event for one subject can either 
be a 0 (censored), 1 (disease) or 2 (death from other causes)
}
  \item{t}{
A vector of times until event for each subject in the dataset.  This is the time that the event 'e' happened. 
 This time must fall in the interval [0, t*), where t* is the duration of the study. 
}
  \item{rs}{
A named list with two elements: r1 and r2.  r1 holds assigned risk values for each subject according to model 1. r2 holds 
assigned risk values for each subject according to model 2.  These assigned risk vectors the probability of disease, 
as predicted by risk model 1 or 2.
}
  \item{design}{
If the design is a random sample, this argument should be the character string: "randomSample" 
If using two stage data, this argument should be a named list of two elements, c and N:

\itemize{
  \item \code{c}: A vector of sampling categories for each subject in the data.  The two-stage sampling category
                  should be specified by capital letters, starting with "A", "B", .... 
  \item \code{N}: A named vector, with one element for each sampling category.  The names of this vector
                  should be the capital letters "A", "B", ..., from list element 'c' above.  
                  This vector should hold the number of subjects (from the first stage) that fell into each category.
}

}
  \item{riskGroups}{
A named list with two elements.  Each element name must start with "k", "K", or "cutoffs".  The first element of this list
is the riskGroup selection for model 1, and the second element of the list is the riskGroup selection for model 2.  

There are three ways that each riskGroup can be described (indicated by the name of the list element; the list element's name must 
start with either "k", "K", or "cutoffs").  Both riskGroups do not need to be described in the same way, but for the examples in
the following list, both riskGroups are described the same way.

\itemize{
  \item "k": The user can manually assign risk groups using an integer vector 'k' containing risk group 
             assignments for each subject in the dataset.  Define K to be the 
             total number of risk groups.  This vector will hold a risk group assignment  in {1, ..., K} for each
             subject in the dataset.  Subjects with risk group assignment = 1 will have the smallest assigned risks (r), and 
             subjects with the risk group assignment = K will have the highest assigned risks.  

	     The user can create this type of riskGroup selection using the construct:
	     \code{ list(k1 = my_k_assignment_vector_model1, k2 = my_k_assignment_vector_model2) }

  \item "K": The user can opt to have the package automatically create K risk groups, where K is a single integer value.  
  	     Each subject will be assigned 
             a risk group assignment  in {1, ..., K}.  Each risk group will have approximately the same number of subjects.
             Risk group 1 will have the subjects with the smallest assigned risks (r), and risk group K will have the subjects
             with the largest assigned risks. 

	     The user can create this type of riskGroup selection using the construct:
	     \code{ list(K1 = how_many_risk_grps_model1, K2 = how_many_risk_grps_model2) }

  \item "cutoffs": Risk groups can also be assigned to each subject in the dataset based on risk "cutoffs". 
             Assume that I want to create three risk groups.  I want all subjects having an assigned risk between 0 and 0.2 to 
             be in risk group 1, all subjects with assigned risk between 0.2 and 0.5 to be in risk group 2, and 
             all subjects with assigned risk between 0.5 and 1 to be in risk group 3.  

	     I can create this type of riskGroup selection using the construct:
	     \code{ list(cutoffs1 = c(0, 0.2, 0.5, 1), cutoffs2 = c(0, 0.2, 0.4, 0.6, 1)) }
}

}
  \item{bootstrap}{
A length-1 integer vector.  The number of bootstrap replications to be used in performance difference confidence intervals. 
}
}
\details{

}
\value{
A named list.  Items #1 - #4 will always be returned.  Items #5 - #8 will only be returned if both models have 
only two risk groups.  
\enumerate{
  \item "AUC_diff": AUC model 2 - AUC model 1.  AUC is 'Area Under the Curve'.  See \code{?riskValidate} for 
                    more details

  \item "ci_AUC_diff":  A bootstrap confidence interval for AUC model 2 - AUC model 1

  \item "SD_diff": SD model 2 - SD model 1.  SD is 'standard deviation of the model'.  See \code{?riskValidate}
                   for more details

  \item "ci_SD_diff": A bootstrap confidence interval for SD model 2 - SD model 1

  \item "PPP_diff": PPP model 2 - PPP model 1.  PPP is 'predictive power positive'.  For models where K = 2, 
                    PPP is analogous to piHat[2]

  \item "ci_PPP_diff": A bootstrap confidence interval for PPP model 2 - PPP model 1.  

  \item "PPN_diff":  PPN model 2 - PPN model 1.  PPN is 'predictive power negative'.  For models where K = 2, 
                     PPN is (1 - piHat[1]).  

  \item "ci_PPN_diff": A bootstrap confidence interval for PPN model 2 - PPN model 1. 

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
twoMod = df_twoStage_r1_r2()

# Two risk groups in for each model, so PPP_diff and PPN_diff are reported.
pd1 = performanceDifference(e = twoMod$d$e, t = twoMod$d$t, 
                           rs = list(r1 = twoMod$d$r1, r2 = twoMod$d$r2),
                           design = list(c = twoMod$d$c, N = twoMod$N),
                           riskGroups = list(K1 = 2, K2 = 2), bootstrap = 300)
pd1

# One model has 3 risk groups, so PPP_diff and PPN_diff are not reported.
pd2 = performanceDifference(e = twoMod$d$e, t = twoMod$d$t, 
                           rs = list(r1 = twoMod$d$r1, r2 = twoMod$d$r2),
                           design = list(c = twoMod$d$c, N = twoMod$N),
                           riskGroups = list(K1 = 2, K2 = 3), bootstrap = 300)
pd2
}
