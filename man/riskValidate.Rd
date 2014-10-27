\name{riskValidate}
\alias{riskValidate}

\title{
Validate a risk model for cohort data
}
\description{
This function provides several measures to assess the validity of a risk model.  These measures
will be explained in the \code{Value} section below.  The function also draws
an attribute diagram which compares assigned risk (from the \code{rSummary} argument) to piHat, the 
estimated risk as computed by the program.  Bootstrap confidence intervals are offered for several 
measures of validity.  
}
\usage{
riskValidate(
  e, t, r, design = "randomSample", riskGroup, rSummary, 
  bootstrap = FALSE, rvpar = rvparFn())
}

\arguments{
  \item{e}{
    An integer vector of events for each subject in the dataset.  0 denotes 
    censored, 1 denotes disease, and 2 denotes death from other causes.
}
  \item{t}{
    A numeric vector of times until event for each subject in the dataset.  
    These times must be in the interval [0, t*), where t* is the duration 
    of the study. 
}
  \item{r}{
    A numeric vector representing the risk assigned to each subject in the 
    dataset.  These risks must be in the range (0, 1). 
}
  \item{design}{
    This argument describes the sampling design used in the dataset.  
    A dataset can either be random sample, or a two-stage sample.  
    This argument is defined differently in each case:

    If random sampling was used, this argument takes the value of the 
    character string "randomSample"

    If two-stage sampling was used, more information must be provided to 
    describe the details of the sampling design.  In the first stage, a 
    random sample of subjects are screened and placed into two or more 
    sampling categories -- for example categories A and B.  The first step 
    is to define the number of subjects from the first stage that fell into 
    categories A and B.  This must be a named integer vector, such as:
    c(A = ___, B = ___), where the user would input integers in place of the 
    underscores.  

    In the second stage of two-stage sampling subjects are resampled with 
    different probabilities depending on the category.  These resampled 
    subjects are what we see in a two-stage dataset.  Each subject originally 
    comes from sampling group A or B, so this must be indicated.  

    If two stage sampling, the \code{design} argument will be a list 
    comprised of two named elements: N and c.  'N' is the number of first-stage 
    subjects in each category.  'c' is a character vector with one element for 
    each subject in the dataset describing the subject's sampling category.  
    Each element of 'c' is a single character, for instance "A" or "B".  
    The set of characters in this 'c' vector must correspond to the names of 'N'.
}
  \item{riskGroup}{
    This argument describes the way in which subjects are divided into risk groups. 
    There are three ways that riskGroup can be described.  This argument is a 
    named list with one element, and the name indicates the way that riskGroup 
    is being described.  Possible names for riskValidate are "k", "K", 
    and "cutoffs". 

   \itemize{
     \item "k": If you have already designated risk groups to each subject in 
        your dataset, riskGroup can be  described by this vector.  This should 
        be an integer vector with values in {1, ..., K}, where K is the total 
        number of risk groups.  As a small (unrealistic) example, I will assume 
        there are 5 subjects in my dataset and I want to divide them into two 
        risk groups.  I could define the riskGroup argument as follows: 
        \code{list(k = c(1, 2, 1, 1, 2))}
     \item "K": If you have not already assigned each subject a risk group, 
        the package can do it for you. One way to have the package do this for 
        you is to tell it how many risk groups to use.  The program will then 
        automatically divide the subjects into risk groups automatically.  Each 
        risk group will have approximately the same number of subjects.  
        The first group (indicated as risk group 1, or with value k = 1) will 
        hold the subjects with the smallest assigned risks (see argument 'r'). 
	The last risk group will hold the subjects with the highest assigned risk.  
        To prompt the package to automatically assign 4 risk groups, you may 
        define the riskGroup argument as \code{list(K = 4)}. 
     \item "cutoffs": The program can automatically assign risk groups based 
        on risk 'cutoffs'.  Assume that I want all subjects having an assigned 
        risk, r, between 0 and 0.33 to be in risk group 1; all subjects
	with r between 0.33 and 0.66 to be in risk group 2; and all subjects 
        with r between 0.66 and 1 to be the final third risk group.  To do this, 
        I would specify the riskGroup argument as 
        \code{list(cutoffs = c(0, 0.33, 0.66, 1))}.  Note that the first value 
        of cutoffs should be 0, and the last value should be 1.
    }
}
  \item{rSummary}{
    The rSummary is a summary statistic for the assigned risks for all subjects 
    in each risk group.  The rSummary can be calculated by the program or 
    provided by the user.  There are four options in specifying rSummary:
    \itemize{
      \item mean:     The package can compute the rSummary as the *mean* 
        value of the assigned risks in each risk group.  To use this option, 
        the argument rSummary should be defined as the character string "mean"
      \item median:   The package can compute the rSummary as the *median* 
        value of the assigned risks in each risk gorup.  To use this option, 
        the argument rSummary should be defined as the character string "median"  
      \item midpoint: If "cutoffs" were supplied for the \code{riskGroup} argument, 
        then this option can be used.  The rSummary can be calculated as 
        the midpoint of each interval defined in the riskGroup's cutoffs. If the 
        values for the cutoffs in the \code{riskGroup} argument were 
        0, 0.4, and 1, then the package would automatically compute the 
        rSummary values to be 0.2 and 0.7.  To use this option, the argument 
        rSummary should be defined as the character string "midpoint"
      \item custom:   If I would like to specify my own rSummary values and 
        not use any of the above options, I can define my own rSummary values 
        directly as a numeric vector.  For example, I could define the rSummary 
        argument as c(0.2, 0.7). 
    }
}
  \item{bootstrap}{
    If bootstrapping is "turned on", this function will report additional 
    bootstrap confidence intervals for some of its measures of validity, 
    including piHat, AUC, and SD.  These measures are described below in 
    the "Value" section.  To turn this feature off, set this bootstrap 
    argument to FALSE.  To turn it on, set this argument to an integer 
    representing the number of bootstraps.  This function will take a 
    longer amount of time when many bootstraps are used.  Many bootstraps 
    are recommended (200+) to stabilize bootstrap variances.
}
  \item{rvpar}{
    If no figure is desired, set rvpar = FALSE; otherwise, rvpar is
    a named list of class "rvpar" that contains fifteen elements: 
    col, lightCol, xmax, ymax, inflate, comment, annotate, pch, cex, 
    axesInPercents, atX, atY, xlab, ylab, main. Such an object is the
    the output of the function \code{rvparFn}.  
    See \code{?rvparFn} for more information. 
}
}
\details{

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

}
\examples{

set.seed(1)
d_rs = df_randomSample()

# riskValidate for random sample data with bootstraps, 
# draws attribute diagram, using predetermined risk groups.
rv1 = riskValidate(
    e = d_rs$e, t = d_rs$t, r = d_rs$r, 
    design = "randomSample", riskGroup = list(k = d_rs$k), rSummary = "mean", 
    bootstrap = 200, rvpar = rvparFn(col = "darkgreen"))
rv1

# riskValidate for random sample data with no bootstraps, 
# draws no attribute diagram, using 3 risk groups determined by the package
rv2 = riskValidate(
    e = d_rs$e, t = d_rs$t, r = d_rs$r, 
    design = "randomSample", riskGroup = list(K = 3), rSummary = "mean", 
    bootstrap = FALSE, rvpar = FALSE)
rv2

# riskValidate for random sample data with no bootstraps, 
# draws no attribute diagram, using cutoffs for riskGroup, midpoint for rSummary
rv3 = riskValidate(
    e = d_rs$e, t = d_rs$t, r = d_rs$r, 
    design = "randomSample", riskGroup = list(cutoffs = c(0, 0.4, 0.5, 1)),
    rSummary = "midpoint", bootstrap = FALSE, rvpar = FALSE)
rv3

# riskValidate for two-stage sample data with bootstraps, 
# draws attribute diagram, using predetermined risk groups
set.seed(1)
d_ts = df_twoStage()
rv4 = riskValidate(
    e = d_ts$d$e, t = d_ts$d$t, r = d_ts$d$r,
    design = list(N = d_ts$N, c = d_ts$d$c),
    riskGroup = list(K = 2), rSummary = "mean", 
    bootstrap = 200, rvpar = rvparFn())
rv4

}
