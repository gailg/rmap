\name{riskDensityRawFn}
\alias{riskDensityRawFn}
\title{
Draw a *raw* density graphic for a risk model.
}
\description{
This function draws a *raw* density plot graphic for a
risk model.  It is a "raw" function because it requires
\code{setUpTrellisFn} to be called first to set up the 
plotting region(s).  Separate densities are drawn to compare the 
affected subjects to the unaffected subjects in the dataset. 
"Affected" subjects are those who experienced a disease event
(e = 1).  "Unaffected" subjects are those who died from other 
causes (e = 2) or were censored before tStar (e = 0 & t < tStar). 

For a more user-friendly density plot function, see 
\code{riskDensity}, which sets up the plotting region(s),
then draws the density plot.  This function is designed to 
allow the user to create sophisticated multi-faceted (trellis-style)
graphics.
}
\usage{
riskDensityRawFn(pos, e, t, r, tStar, 
  col1, col2, lightCol1, lightCol2, 
  adjust = 1, legend = TRUE
}

\arguments{
  \item{pos}{
An integer vector with two elements.  Before this function is 
called, \code{setUpTrellisFn} must be called, which sets up a 
"grid" of graphing regions.  This \code{pos} argument specifies
the row and column in which the density plot should be drawn.
}
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
  \item{col1}{
  The color of the outline of the "affected" density
}
  \item{col2}{
  The color of the outline of the "unaffected" density
}
  \item{lightCol1}{
  The color of the fill of the "affected" density.  A 
  semi-transparent color is suggested for this value so both 
  densities can be seen concurrently. 
}
  \item{lightCol2}{
  The color of the fill of the "unaffected" density.  A
  semi-transparent color is suggested for this value so both
  densities can be seen concurrently. 
}
  \item{adjust}{
   A smoothing multiplier that adjusts the bandwith for the density 
   calculation.  This parameter is passed to the \code{density} function.
}
  \item{legend}{
  A logical value.  Should a legend be drawn?
}
}
\details{

}
\value{





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
  
  setUpTrellisFn(2, 1, main = "Densities of Models 1 and 2",
                 axesInPercents = FALSE,
                 xmax = 1, ymax = 4)
  
  riskDensityRawFn(pos = c(1, 1), e = sampleData$e,
                   t = sampleData$t, r = sampleData$r1,
                   tStar = 10, col1 = "#B23AEE", col2 = "#006400",
                   lightCol1 = "#B23AEE60", lightCol2 = "#00640060")
  addTextToTrellisFn(pos = c(1, 1), "model 1")
  
  riskDensityRawFn(pos = c(2, 1), e = sampleData$e,
                   t = sampleData$t, r = sampleData$r2,
                   tStar = 10, col1 = "#B23AEE", col2 = "#006400",
                   lightCol1 = "#B23AEE60", lightCol2 = "#00640060")
  addTextToTrellisFn(pos = c(2, 1), "model 2")
}
