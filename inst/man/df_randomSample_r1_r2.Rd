\name{df_randomSample_r1_r2}
\alias{df_randomSample_r1_r2}

\title{
df_randomSample_r1_r2
}
\description{
Generate a random sample of people like df_randomSample, but replace the column r with r1 and add another
column called r2 which is a noisy version of r1.
}
\usage{
df_randomSample_r1_r2(NTotal = 1000, distribution = "lognormal", 
  param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, 
  tStar = 10, KKK = 5)
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
A data frame with nrows equal to NTotal and with columns the same as df_randomSample, except replace 
the column r with r1 and add a column r2 which is a noisy version of r1.  
r2 = plogis(qlogis(r1) + rnorm(n = length(r1), mean = 0, sd = 0.5))
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
df1 = df_randomSample_r1_r2()
head(df1)

}
