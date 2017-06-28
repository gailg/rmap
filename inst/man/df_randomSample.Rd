\name{df_randomSample}
\alias{df_randomSample}

\title{
Generate a random sample of people who are subject to competing risks.  
This random sample can be used to test functions such as riskValidate in rmap.
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
}
\usage{
df_randomSample(NTotal = 1000, distribution = "lognormal", 
  param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10, KKK = 5)
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
  \item{KKK}{
A positive integer that determines the number of risk groups. 
The program assigns the subjects into approximately equal-sized risk groups.
The risk group assignment is recorded in the column k, 1 for the people with the smallest risk r, 
and so on up to KKK for the people with the largest risk.
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
\item{\code{c}}{
A vector of "A"s.  This vector stores the two-stage category under two-stage sampling.
For random samples, all subjects are considered to be from the same category "A".
}
\item{\code{k}}{
A vector of integers representing the risk group assignment based on the risk 
vector r and the input parameter KKK equal to the number of risk groups.
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
df1 = df_randomSample()
head(df1)
}
