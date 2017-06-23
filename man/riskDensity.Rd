\name{riskDensity}
\alias{riskDensity}
\title{
Draw a density graphic for a risk model.
}
\description{
This function draws densities comparing the assigned risk of
affected subjects to that of unaffected subjects in a
dataset.  "Affected" subjects are those who 
experienced a disease event (e = 1).  "Unaffected" 
subjects are those who died from other causes (e = 2) or 
were censored before tStar (e = 0 & t < tStar).
}
\usage{
riskDensity(e, t, r, tStar, 
  adjust = 1, rvpar = rvparFn(), ocall = NULL)
}

\arguments{
  \item{e}{
  An integer vector of events for each subject in the dataset.
  0 denotes censored, 1 denotes disease, and 2 denotes death
  from other causes.

}
  \item{t}{
   A numeric vector of times until event for each subject in the
   dataset.  These times must be in the interval [0, tStar), where
   tStar is the duration of the study.
}
  \item{r}{
  A numeric vector representing the risk assigned to each
  subject in the dataset.  These risks must be in the range (0, 1).

}
  \item{tStar}{
  The duration of the study
}
  \item{adjust}{
   A smoothing multiplier that adjusts the bandwith for the density 
   calculation.  This parameter is passed to the \code{density} function.
}
  \item{rvpar}{
A named list of class "rvpar" that contains fifteen elements: 
col, lightCol, xmax, ymax, inflate, comment, annotate, pch, cex, 
axesInPercents, atX, atY, xlab, ylab, main. Such an object is the
the output of the function \code{rvparFn}.  
See \code{?rvparFn} for more information. 

}
  \item{ocall}{
A function argument that allows the user to see the original function 
call in the box under the IAD.  This argument should be left as 
the default value.
}
}
\details{
Semi-transparent light colors should be specified for this function in the 
\code{rvpar} graphical parameters.  This can be specified using the 
\code{lightCol} argument to \code{rvparFn}. Using semi-transparent colors
will allow you to concurrently see both densities on the same plot.
}
\value{
Draws a risk density to the current graphics device.  
Invisibly returns a list of all the elements that make up the graphic.
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
  sampleData = df_randomSample_r1_r2()
  riskDensity(sampleData$e, sampleData$t, sampleData$r1, 
    tStar = 10, adjust = 1)
}
