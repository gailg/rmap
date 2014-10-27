\name{riskValidateUngrouped}
\alias{riskValidateUngrouped}

\title{
Validate a risk model for ungrouped cohort data.
}
\description{
Description: 
  This function provides measures to assess the validity of
  a risk model using ungrouped data.  In particular this function
  provides the AUC, a 95 percent confidence interval for the AUC, and an
  individualized attribute diagram together with 95 percent confidence
  bands which compare assigned risk to observed risk.  

  The AUC is obtained by calculating the mean of the Case Risk
  Percentiles (CRP).

  The individualized attribute diagram is a scatterplot of points 
  x = rho and y = piHatNN  with line segments connecting addjacent
  points.  The vector rho contains the distinct values of assigned 
  risks, and piHatNN contains the epsilon kernel nearest neighbor 
  estimates of outcome probabilities.  Confidence bands are gotten 
  using a bootstrap.
}
\usage{
riskValidateUngrouped(
  e, t, r, design = "randomSample", riskGroup, 
  bootstrap = FALSE, rvpar = rvparFn(), 
  multicore = FALSE, verbose = FALSE)
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
    A list with one element named "ungrouped".  ungrouped is itself a list with 
    named elements epsilon and tStar.
    \itemize{
      \item{epsilon}{
          A real number between 0 and 1.  The epsilon in the
          "epsilon kernel nearest neighbor estimates (piHatNN) of outcome
          probabilities". The epsilon kernel nearest neighborhood of the
          distinct assigned risk rho is defined to be the set of all
          observations with assigned risk r that satisfies |GHat(r) -
          GHat(rho)| < epsilon, where G is the empirical distribution
          function of the assigned risks.
      }
      \item{tStar}{
          A positive real number equal to the right end point of duration of the 
          study and at least as large as max(t).
      }
    }
}
  \item{bootstrap}{
    If bootstrapping is "turned on", this function will report  
    bootstrap confidence intervals for the outcome probabilities.
    To turn bootstrapping on, set this argument to an integer 
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

  \item{multicore}{
    A logical value indicating whether or not the bootstraps for
    the confidence bands of the individualized attribute diagram
    should be calculated by multiple processors.
}
  \item{verbose}{
    A logical value indicating whether or not progress
    statements should be printed during the calculation of the
    individulaized attribute diagram.  Setting this value to TRUE can
    give you some idea that the function is running and the amount of
    time required for completion.
}
}

\value{
  An object of type rvu, which is a list that contains the
  elements AUC (an object from AUCInternalFn) and PNN (an object from
  piHatNNInternalFn). 

 AUC is a list that contains three elements: (1) AUC is a numerical
    value equal to the Area Under the Receiver Operating
    Characteristic curve. (2) AUC_CI is a numerical vector of length 2
    containing the lower and upper limits of the 95 percent confidence
    interval for the AUC. (3) CRP is a list containing two lists, each
    of length equal to the number of cases and each element in the two
    lists corresponding to a case; the first sublist of CRP
    is also called CRP and contains the Case Risk Percentiles; the
    second sublist of CRP is called a and contains the two-stage
    sampling weights of the cases.

PNN is a list of matrices. The first matrix is named est and has
    two columns each of length equal to the number of distinct values
    of assigned risk.  The first column is rho and equal to the sorted
    distinct values of assigned risk.  The second column is piHatNN
    contains the nearest neighbor estimate of outcome probability at
    each rho.
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html

}
\author{
David Johnston and Gail Gong

}

\seealso{

}
\examples{
  set.seed(1)
  sampleData = df_randomSample_r1_r2(NTotal = 200)
  epsilon = nrow(sampleData)^(-1/3)
  tStar = 10
  riskGroup = list(ungrouped = list(epsilon = epsilon, tStar = tStar))
  rvu1 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup)
  rvu1
  
  rvu1b = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, 
    bootstrap = 20, rvpar = FALSE, 
    multicore = FALSE, verbose = TRUE)
  rvu1b
  str(rvu1b)
  IAD(rvu1b)
}
