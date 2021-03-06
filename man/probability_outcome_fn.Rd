% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability_outcome_fn.R
\name{probability_outcome_fn}
\alias{probability_outcome_fn}
\title{Calculate the probability of disease during a given time interval}
\usage{
probability_outcome_fn(lambda_1, lambda_2, t_star)
}
\arguments{
\item{lambda_1}{A positive real number reflecting the exponential rate of disease.}

\item{lambda_2}{A positive real number reflecting the exponential rate of death.}

\item{t_star}{A positive number reflecting the duration of the study.}
}
\value{
A positive number equal to the probability of disease during the interval [0, t_star].
}
\description{
Assuming that disease and death are independent and each follow an exponential distribution
this function calculates the probability of disease during [0, t_star]
given exponential hazards lambda_1 and lambda_2 respectively for disease and death.
}
\examples{
lambda_1 = 0.057
lambda_2 = 0.056
t_star = 10
probability_outcome_fn(lambda_1, lambda_2, t_star)

}
