\name{df_raw}
\alias{df_raw}

\title{
Generate a random sample of people who are subject to competing risks.  
}
\description{
Individuals in the data set are subject to times (t0) to censoring,
times (t1) to disease, and times (t2) to death. We observe 
t = min(t0, t1, t2) and e = which of t0, t1, t2 is equal to t.

The generating functions generate t0, t1, and t2 independently
according to exponential distributions with rates = eta0, eta1, and 
eta2 respectively, where eta0 and eta2 are inputs and eta1 is
generated from either a lognormal or beta distribution.  If lognormal,
then log(t2) is normal with mean param1 and sd param2.  If beta, then
param1 and param2 are the shape parameters of beta.

Given these models for generating data, the probability of developing
disease given a person's eta1 can be calculated using Equation (18) in 
"rmap-formulas-v01.pdf" from the website.  
This probability called the risk is calculated and recorded as r in 
the output variables.

This is an internal function that is used by df_randomSample and df_twoStage.  The
user is advised to use either of these two functions, not this raw function. 
}
\usage{
df_raw(NTotal, distribution, param1, param2, eta0, eta2, tStar)
}

\arguments{
  \item{NTotal}{
An integer equal to the number of people in the random sample.
}
  \item{distribution}{
A character string equal to "lognormal" or "beta".  This is the distribution used to
generate eta1 which is the rate of the exponential that generates times to disease.
Mean time to disease is equal to 1/eta1.
}
  \item{param1}{
A number that determines the first parameter of the distribution that generates rate 
of disease.  If distribution = "lognormal", log(t1) is normal with mean param1.  If 
distribution = "beta", param1 is the shape1 parameter of rbeta.
}
  \item{param2}{
A number that determines the second parameter of the distribution that generates rate 
of disease.  If distribution = "lognormal", log(t1) is normal with sd param2.  If 
distribution = "beta", param2 is the shape2 parameter of rbeta.
}
  \item{eta0}{
A positive number that determines the rate of censoring.  Times to censoring are generated
according to rexp with rate parameter eta0.
}
  \item{eta2}{
A positive number that determines the rate of death.  Times to death are generated
according to rexp with rate parameter eta2.
}
  \item{tStar}{
A positive number that determines the length of time of the study.  All times are censored at 
tStar.
}
}
\details{

}
\value{
A data frame with nrows equal to NTotal and with columns
\item{\code{e}}{
Disease Status -- 0 for censored, 1 for death from disease,
and 2 for death from other causes
}
\item{\code{t}}{
Time of event e.  Ranges from (0, t*]
}
\item{\code{w}}{
A vector of positive numbers. The rate of the exponential time to disease.
}
\item{\code{r}}{
A vector of probabilities. The risk.  The probability of disease given the covariate w.
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
df_raw(NTotal = 10, distribution = "lognormal", 
  param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10)
}
