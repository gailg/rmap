\name{score_statistics}
\alias{score_statistics}
\alias{standardized_residuals}
\alias{plot.standardized_residuals}

\title{
Test the null hypothesis that a risk model is correct
}

\description{
The function \code{score_statistics} calculates a statistic that can be used to test the 
null hypothesis that a given risk model is correct. Such a statistic is gotten by embedding the outcome and mortality hazards underlying the risk model into larger model by regressing on one or several covariates. The null hypothesis states that the regression coefficients vanish or equivalently the original risk model is correct. 

The function \code{standardized_residuals} calculates the numbers that are used in the attribute-diagram-and-standardized-residuals plot.  This plot is a tool for investigating the reasons for poor fit when the null hypothesis is rejected.  
}

\usage{
score_statistics(e, t, ...)
standardized_residuals(e, t, ...)
plot.standardized_residuals(sr, grouping_name, plot_pars = NULL)
}

\arguments{
  \item{e}{
An integer vector (of length \code{N}) containing the event of each subject in the data set.  \code{0} denotes censored or alive and free from disease at time \code{tStar}, \code{1} denotes outcome, and \code{2} denotes death from other causes. 
}
  \item{t}{
A numeric vector (of length \code{N}) containing the time until event for each subject in the data set.  These times must be in the interval \code{[0, tStar]} where \code{tStar} is the duration of the study.
}
  \item{\dots}{
One or more \code{risk_model} specifications.  Given a risk model whose accuracy the user wishes to evaluate, the \code{risk_model} specification is a list containing the following four named objects:

  \itemize{
    \item{r}: 
        A numeric vector (of length \code{N}) containing the assigned risk of each subject.  
        This is the probability of positive outcome according to the risk model.  This number 
        lies in \code{[0, tStar]}. 

    \item{Lambda_outcome}: 
        A numeric vector (of length \code{N}) containing the cumulative outcome hazard
        until time \code{t} underlying the risk model.  We assume that the risk model depends 
        on an outcome hazard rate and a death or mortality hazard rate for each subject. Then
       \code{Lambda_outcome[n]} is the integral from \code{0} to \code{t[n]} of the \code{n}-th
       person's outcome hazard rate.

    \item{Lambda_mortality}:   
        A numeric vector (of length \code{N}) containing the cumulative mortality hazard 
        until time \code{t} underlying the risk model.  \code{Lambda_mortality[n]} is the                    integral from \code{0} to \code{t[n]} of the hazard rate of mortality of the                         \code{n}-th subject.

    \item{groupings}:
        A named list, each element of which is a \code{grouping} description.  A 
        \code{grouping} description is a list that provides a description of a grouping or 
        partition of the data into \code{K} subgroups.  This list can be defined in various 
        ways.

        \itemize{

            \item{1.} The \code{grouping} list contains an integer vector of length 
                \code{N}. This vector is named \code{k} and contains the subgroup number 
                for each subject.  The subgroup number must be an integer in \code{1:K} 
                where \code{K} is the number of subgroups. For example, if \code{x$k}
                is a vector containing subgroup numbers, we could use the 
                \code{grouping} list \code{list(k = x$k)}.

            \item{2.} The \code{grouping} list contains an integer vector of length 
                \code{1}.  This vector is named \code{K} and contains a single positive 
                integer.  This specifies 
                the grouping is quantiles of the assigned risk \code{r}.  For example if 
                the \code{grouping} list is \code{list(K = 4)}, the grouping is quartiles 
                of \code{r}.

            \item{3.} The \code{grouping} list contains a numerical vector named 
                \code{cutoffs}. This vector contains increasing probabilities beginning with 
                \code{0} and ending with \code{1}. This specifies the grouping is subgroups 
                created by binning \code{r} using the cutpoints in the \code{cutoffs}
                vector.  For example 
                if the \code{grouping} list is \code{list(cutoffs = c(0, 0.33, 0.66, 1))}, 
                the first subgroup contains all subjects with \code{r} in 
                \code{c[0, 0.33]}, the second subgroup all subjects with \code{r} in 
                \code{c(0.33, 0.66]}, and the third subgroup all subjects with \code{r} 
                in \code{c(0.66, 1]}.

           \item{4.} The \code{grouping} list contains a single integer named \code{K} 
               and a vector of length $\code{N} named \code{variable}.  This specifies 
               the grouping is quantiles of the vector \code{variable}.  For example if 
               the \code{grouping} is \code{list(K = 4, variable = x$w)}, the grouping is 
               quartiles of the vector \code{x$w}. 

           \item{5.} The \code{grouping} list contains a vector named \code{cutoffs} and 
               a vector of length \code{N} named \code{variable}.  This specifies the 
               grouping is subgroups created by binning the vector using the cutpoints in 
               \code{cutoffs}.  For example, if the \code{grouping} is 
               \code{list(cutoffs = c(0, 1, 2, Inf), variable = x$w)}, the first subgroup 
               contains all subjects with \code{x$w} in \code{c[0, 1]}, the second 
               subgroup contains all subjects with \code{x$w} in \code{c(1, 2]}, and the 
               third subgroup contains all subjects with \code{x$w} greater than \code{1}.

        }

  }
}

  \item{sr}{
An object of type \code{standardized_residuals}.  Create this object using the function 
\code{standardized_residuals}.
}

  \item{grouping_name}{
A character string.  The name of a \code{grouping_description} in \code{groupings}.
}
  \item{plot_pars}{
A named list that controls the attribute-diagram-and-standardized-residual plot produced by 
\code{plot.standardized_residuals}. The four elements in this list are

    \itemize{
        \item{1.} \code{x_max}: A number between \code{0} and \code{1} that specifies the
           right-hand side of the range of the x-axis in the attribute diagrams and 
           standardized residual plots. The x-axes always displays the mean risk 
           \code{r} of each subgroup.

        \item{2.} \code{y_max}: A positive number bigger at least \code{2} that specifies 
           range of the y-axis in absolute value of the standardized residual plots.

        \item{3.} \code{y_max_ad}: A positive number that specifies the upper range of the 
           y-axis in the attribute diagrams.

        \item{4.} \code{xlab}: A character string that specifies the title for the x-axes.

    }
}
}

\details{
The function \code{score_statistics} calculates a statistic that can be used to test the null hypothesis that a given risk model is correct. The function \code{standardized_residuals} calculates an object of class \code{standardized_residuals} that are used in drawing the attribute-diagram-and-standardized-residuals plot.  

These function assume that the user has a random sample of \code{N} subjects; the user has created one or more risk models that assign to each subject at the time she enters the study, a risk, which is the probability that she suffers a specific adverse outcome within a given time period \code{tStar}, the duration of the study; and the user has followed each subject in the study until one of the following events first occurs:  (a) the subject suffers the adverse outcome, (b) the subject dies from other causes, (c) the subject is censored (lost from the
study) before time \code{tStar}, and (d) the subject is alive and free from
the adverse outcome at time \code{tStar}. 

To test the null hypothesis that a given risk model is correct, the underlying outcome and mortality hazards of the risk model are embedded into a larger model by regressing on one or several covariates. The null hypothesis that the original risk model is correct is equivalent to vanishing regression coefficients. Changing the covariate(s) in the regression gives a different score statistic. \code{score_statistic} reports the score statistic for a battery of covariates giving a powerful tool for exploring the accuracy of a risk model.
}

\value{
The output for \code{score_statistics} consists of a matrix, each column corresponding to a 
\code{risk_model} specification and each row corresponding to a different score statistic 
resulting from a different vector of covariates that expands the hazard rates for outcome 
and/or death of the risk model of each column.  

The rows of this matrix are the p-values for the following score statistics.

    \itemize{
        \item{1.} \code{overall_hosmer_lemeshow}:  The Hosmer_Lemeshow Chi-squared 
          goodness-of-fit statistic (Gong, Quante, Terry, Whittemore 2013
          Equation (3) with \code{K = 1}). This statistic compares the mean
          overall outcome probability of outcome to the mean overall assigned
          risk. 

       \item{2.} \code{overall_combined}: The score statistic with covariate 
          z = z_1 = z_2 = 1 described in Section 2A of 
          http://stanford.edu/~ggong/rmap/doc/score-statistics-formulas-v01.html
    

       \item{3.} \code{overall_outcome}: The score statistic with covariate z = z_1.

       \item{4.} \code{overall_mortality}: The score statistic with covariate z = z_2.

       \item{5.} \code{weighted_combined}: The score statistic with covariate for the
         \code{n}-th subject z_{n1} = z_{n2} = exp(|r_n - mean(r)|)/exp(max(r) - mean(r)).

       \item{6.} \code{weighted_outcome}: The score statistic with covariate for the
         \code{n}-th subject z_{n1} in 5.

       \item{7.} \code{weighted_mortality}: The score statistic with covariate for the
         \code{n}-th subject z_{n2} in 5. 
    }

For each \code{grouping} description in the \code{groupings} list of the \code{risk_model},
the output also contains a row containing the p-value for each of the following statistics.

\itemize{
  \item{1.} \code{grouping_hosmer_lemeshow} The Hosmer-Lemeshow Chi-squared
  goodness-of-fit statistic with \code{K} equal to the number of subgroups 
  in this grouping.
  
  \item{2.} \code{grouping_combined}: The score statistic with covariate for the
  \code{n}-th subject equal to z_n = z_{n1} = z_{n2} where z_{njk} indicates
  membership in subgroup \code{k}.
  
  \item{3.} \code{grouping_outcome}: The score statistic with covariate for the 
  \code{n}-th subject equal to z_{n1} described in 2.
  
  \item{4.} \code{grouping_mortality}: The score statistic with covariate for the 
  \code{n}-th subject equal to z_{n2} described in 2.
  
}

The output for \code{standardized_residuals} is an object of clsss 
\code{standardized_residuals} that is useful for creating 
attribute-diagram-and-standardized-residual plots.  This plot is a tool for investigating 
the reasons for poor fit when the null hypothesis is rejected.
}

\references{
}

\author{
Gail Gong
}
\note{

}



\seealso{

}
\examples{
data(score_statistic_example)
x = score_statistic_example
e = x$e
t = x$t
risk_model_1 = list(
  r = x$r_1, 
  Lambda_outcome = x$Lambda_outcome_1,
  Lambda_mortality = x$Lambda_mortality_1,
  groupings = list(
    risk = list(k = x$k_1),
    missing = list(K = 4, variable = x$w)))
risk_model_2 = list(
  r = x$r_2, 
  Lambda_outcome = x$Lambda_outcome_2,
  Lambda_mortality = x$Lambda_mortality_2,
  groupings = list(
    risk = list(K = 4),
    missing = list(K = 4, variable = x$w)))
ss = score_statistics(e, t, 
  risk_model_1 = risk_model_1, 
  risk_model_2 = risk_model_2)
ss

sr = standardized_residuals(e, t, 
                            risk_model_1 = risk_model_1, 
                            risk_model_2 = risk_model_2)
sr$risk_model_1$risk$hosmer_lemeshow
sr$risk_model_1$risk$residuals$outcome
grouping_name = "risk"
plot(sr, grouping_name, plot_pars = NULL)
plot_pars = list(x_max = 0.17,
                 y_max = 9,
                 y_max_ad = 0.33,
                 xlab = "Risk (percent)")
plot(sr, grouping_name, plot_pars)
}

