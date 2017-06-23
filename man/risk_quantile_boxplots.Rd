\name{risk_quantile_boxplots}
\alias{risk_quantile_boxplots}

\title{
Create boxplots of risk quantiles 
}
\description{
Boxplots of risk quantiles can be useful for judging information loss from grouping data into quantiles.
}
\usage{
risk_quantile_boxplots((list_of_risk_models, K, risk_max = 1, text_x = NULL, text_y = NULL)
}

\arguments{
  \item{list_of_risk_models}{
    A list containing a number of named numeric vectors.  Each vector contains the assigned risk of each subject.  (See standardized_residuals dots r for a description of this numeric vector.) Assigned risks are probabilities and lie between \code{0} and \code{1}.
  }
  \item{K}{
   A positive integer specifying the number of quantiles.  \code{K = 4} specifies quartiles.
  }
  \item{risk_max}{
   A number between \code{0} and \code{1} specifying the largest risk on the x-axis.
  }
  \item{text_x}{
   A number between \code{0} and \code{1} specifying the location on the x-axis (translated to percents) that the risk model name will be printed.
  }
  \item{text_y}{
   A number between \code{0} and \code{K} specifying the location on the y-axis that the risk model name will be printed. The y-axis of the box plot ranges from \code{0} to \code{K}.
  } 
  
}
\details{

}
\value{
NULL.  The plot is a side-effect.  The plot consists of a grid containing 1 column and a row for each vector in \code{list_of_risk_models}.  Each row contains a boxplot display of the assigned risks of a risk model broken down into \code{K} quantiles.
}
\references{

}
\author{
Gail Gong
}

\examples{
data(score_statistic_example)
x = score_statistic_example
list_of_risk_models = list(
  risk_model_1 = x$r_1,
  risk_model_2 = x$r_2)
K = 4
risk_quantile_boxplots(list_of_risk_models, K)
}

