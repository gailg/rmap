\name{baseArgsFn}
\alias{baseArgsFn}

\title{
Transforms user input into baseArgs
}
\description{
This function translates arguments given by the user into a data structure of 
class "baseArgs".  This resulting data structure is used internally throughout 
the 'rmap' package.    
}
\usage{
baseArgsFn(e, t, r, design, riskGroup, rSummary, bootstrap, 
  multicore = FALSE, verbose = FALSE)
}

\arguments{
  \item{e}{
A vector of events for each subject in the dataset.  
The event for one subject can either 
be a 0 (censored), 1 (disease) or 2 (death from other causes).
}
  \item{t}{
A vector of times until event for each subject in the dataset.  
This is the time that the event 'e' occurred. 
This time must fall in the interval [0, t*), where t* is 
the duration of the study. 
}
  \item{r}{
A vector of risk values for each subject in the dataset.  
This is the probability of disease, as predicted by a risk model.  
The primary concern of this 'rmap' package is to assess the validity of 
such risk models.
}
  \item{design}{
If the design is a random sample, this argument should be the 
character string: "randomSample". If using two stage data, this 
argument should be a named list of two elements, c and N:
    \itemize{
      \item \code{c}: 
        A vector of sampling categories for each 
        subject in the data.  The two-stage sampling category
        should be specified by capital letters, starting with "A", "B", .... 
      \item \code{N}: 
        A named vector, with one element for each sampling category.  
        The names of this vector should be the capital letters "A", "B", ..., 
        from list element 'c' above.  This vector should hold the number of 
        subjects (from the first stage) that fell into each category.
    }
}
  \item{riskGroup}{
The riskGroup argument describes the way in which the user wants the subjects 
to be divided into risk groups.  This argument is a named list with  
one list element.  The name of this list element indicates the type 
of riskGroup selection being used (possible names are "K", "k", "cutoffs", or 
"ungrouped").  
There are four ways that the riskGroup can be described:

    \itemize{
      \item "k": 
        The user can manually assign risk groups using an integer vector 'k' 
        containing risk group assignments for each subject in the dataset.  
        Define K to be the total number of risk groups.  This vector will hold 
        a risk group assignment  in {1, ..., K} for each subject in the dataset.  
        Subjects with risk group assignment = 1 will have the smallest assigned 
        risks (r), and subjects with the risk group assignment = K will have the 
        highest assigned risks.  

	The user can create this type of riskGroup selection using the construct:
	\code{ list(k = my_k_assignment_vector) }

      \item "K": 
        The user can opt to have the package automatically create K risk groups, 
        where K is a single integer value. Each subject will be assigned a risk 
        group assignment  in {1, ..., K}.  Each risk group will have 
        approximately the same number of subjects. Risk group 1 will have the 
        subjects with the smallest assigned risks (r), and risk group K will 
        have the subjects with the largest assigned risks. 

	The user can create this type of riskGroup selection using the construct:
	\code{ list(K = how_many_risk_grps) }

      \item "cutoffs": 
        Risk groups can also be assigned to each subject in the dataset based 
        on risk "cutoffs". Assume that I want to create three risk groups.  I 
        want all subjects having an assigned risk between 0 and 0.2 to be in 
        risk group 1, all subjects with assigned risk between 0.2 and 0.5 to 
        be in risk group 2, and all subjects with assigned risk between 0.5 and 
        1 to be in risk group 3.  

	I can create this type of riskGroup selection using the construct:
	\code{ list(cutoffs = c(0, 0.2, 0.5, 1)) }

      \item "ungrouped":
        The previous three ways that the riskGroup argument was specified were ways 
        to describe risk groups. We can also validate risk models 
        using an ungrouped analysis. The calculations require two quantities 
        tStar, which is the duration of the study, and epsilon, which is a 
        parameter used to create the kernel neighborhoods of each distinct 
        risk value of assigned risk.
 
        I can create this type of riskGroup agrument using
        \code{ list(tStar = durationOfStudy, epsilon = numberOfObservations^(-1/3)) }
}

}
  \item{rSummary}{
The rSummary is a summary statistic for the assigned risks for all subjects 
in a risk group.  The rSummary can be calculated by the program or provided 
by the user. There are four options in specifying the rSummary: 

    \itemize{
      \item 
        The package can compute the rSummary as the mean value of the 
         assigned risks for the subjects in each risk group.
         To use this option, the user would define 'rSummary' as the 
         character string: "mean"

       \item 
         The package can compute the rSummary as the median value of 
         the assigned risks for the subjects in each risk group.
         To use this option, the user would define 'rSummary' as the 
         character string: "median"

       \item 
         If cutoffs were supplied for the riskGroup argument, then this 
         option can be used. The rSummary can be calculated as 
         the midpoint of each interval defined in the riskGroup's cutoffs. 
         If the values for the cutoffs in the riskGroup argument 
         were 0, 0.2, 0.5, and 1, then the package would automatically 
         compute the rSummary values to be 0.1, 0.35, and 0.75.
         To use this option, the user would define 'rSummary' as the 
         character string: "midpoint"

       \item 
         The user can manually specify his/her own values for 'rSummary' 
         (and not use any of the above options).  To use this option, 
         the user would define 'rSummary' as a numeric vector, where 
         each element represents the user's summary statistic for a risk 
         group.  The length of this numeric vector is the number of risk 
         groups.  
    }

If names(riskGroup) = "ungrouped", rSummary is not required;  functions not
usually seen by the user sets rSummary = "mean".
}
  \item{bootstrap}{
Either a single integer number, describing the number of bootstraps or 
FALSE to turn off bootstrapping.
}
  \item{multicare}{
A logical value. If multicore = TRUE (and names(riskGroup) = "ungrouped" and 
bootstrap is not set to FALSE), multiple processors are used to compute 
the bootstrap confidence intervals for the individualized attribute diagram.  
The package multicore is required.
}
  \item{verbose}{
A logical value.  If verbose = TRUE (and names(riskGroup) = "ungrouped" and bootstrap
is not set to FALSE), progress of the calculation will be output during the 
calculations of the nearest neighbor and the bootstrap nearest neighbor.  
Such output is useful for checking the progress of these calculations which
can require a long time depending on the number of observations and the number
of distinct assigned risks.
}

}
\details{

}
\value{
A list that has a class attribute 'baseArgs'.  This list has named elements: 
e, t, r, c, k, K, epsilon, tStar, ungrouped, N, n, rSummary, nBootstraps,
multicore, and verbose.  These are the individual components that the internal 
functions in this package require. Below is a description of each of the list 
elements:

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
    If ungrouped = FALSE, k is a vector of risk group assignments for each 
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
  \item "rSummary": 
    If ungrouped = FALSE, rSummary is a vector of risk summaries, one for 
    each risk group.  This vector can be given by the user or calculated by 
    baseArgsFn using method "mean", "median", or "midpoint".
    If ungrouped = TRUE, rSummary is the mean of r.
  \item "nBootstraps": 
    An integer describing the number of bootstraps, where 0 indicates no 
    bootstraps.
  \item "multicore":
    A logical value equal to the input multicore.
  \item "verbose":
    A logical value equal to the input verbose.
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

\examples{
  set.seed(1)

  sampleData1 = df_randomSample()
  baseArgs1 = baseArgsFn(
    e = sampleData1$e, t = sampleData1$t, r = sampleData1$r, 
    design = "randomSample", riskGroup = list(K = 3),
    rSummary = "median", bootstrap = FALSE)
  str(baseArgs1)

  sampleData2 = df_randomSample_r1_r2(NTotal = 200)
  tStar = 10
  epsilon = nrow(sampleData2)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
  baseArgs2 = baseArgsFn(e = sampleData2$e, t = sampleData2$t, 
    r = sampleData2$r1, design = "randomSample", riskGroup = riskGroup,
    rSummary = "mean", bootstrap = 20, multicore = FALSE, verbose = TRUE)
  str(baseArgs2)
}
