\name{kBinOneStage}
\alias{kBinOneStage}

\title{
Bin subjects into risk groups (random sampling)
}
\description{
This function is used internally to bin subjects into 
risk groups, determined by each subject's assigned risks.
Each bin will hold approximately the same number of 
subjects.  Subjects with the smallest assigned risks will be 
in risk group 1, and subjects with the hightest assigned 
risk will be in risk group K, where K = total number of risk groups. 
}
\usage{
kBinOneStage(df, KKK, rCol = "r")
}

\arguments{
  \item{df}{
A data frame, which is the simulated data set.  This function will
assign risk groups to the subjects in this simulated dataset.
}
  \item{KKK}{
A length-1 integer vector. This value describes the total number of 
desired risk groups.  
}
  \item{rCol}{
The name of the column in the data frame containing assigned risks 
for each subject.
}
}
\details{
This function is only used within the simulation functions.  These 
simulation functions start with "df_"
}
\value{
A data.frame, representing the simulated data set.  
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

dataf = df_raw(NTotal = 1000, distribution = "lognormal", 
  param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10)
kbin1stg = kBinOneStage(dataf, KKK = 5)
head(kbin1stg)
}
