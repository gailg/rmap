\name{riskScatterRawFn}
\alias{riskScatterRawFn}
\title{
Plot a *raw* scatter plot of two risk models (or two sets of CRPs)
}
\description{
This function draws a *raw* scatter plot of two risk models (or two
sets of CRPs) using the same "style" of graphics as the other
rmap graphing functions.  It is a "raw" function because it requires 
\code{setUpTrellisFn} to be called first to set up the plotting
region(s).  

For a more user-friendly density plot function, see \code{riskScatter},
which sets up the plotting region(s), then draws the density plot.  
This function is designed to allow the user to create sophisticated
multi-faceted (trellis-style) graphics.  
}
\usage{
riskScatterRawFn(
  pos, r1, r2, 
  col = "#0000FF40", pch = 19, cex = 0.6)
}

\arguments{
  \item{pos}{
  An integer vector with two elements.  Before this function is 
  called, \code{setUpTrellisFn} must be called, which sets up a 
  "grid" of graphing regions.  This \code{pos} argument specifies
  the row and column in which the scatter plot should be drawn.
}
  \item{r1}{
Risk values (using risk model 1) for each person in a data set. 
}
  \item{r2}{
Risk values (using risk model 2) for each person in the *same* data set. 
}
  \item{col}{
The color of the plotted points
}
  \item{pch}{
The plotting character, specified as an integer (or a single character).  
Similar to 'pch' in traditional graphics.
}
  \item{cex}{
The character expansion factor, specified as a numeric value.
Similar to 'cex' in traditional graphics.
}
}
\details{

}
\value{
Draws a scatter plot to the current graphics device.  
An alternative may be the native functions \code{plot} or
\code{xyplot} in the lattice package.  This function is offered
to accommodate the same "style" of graphics as the other 
rmap graphing facilities.  

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
  sampleData = df_randomSample_r1_r2(NTotal = 1000)
  setUpTrellisFn(1, 1, 
    main = "Scatterplot of risks for Models 1 and 2",
    xlab = "r1", ylab = "r2",
    axesInPercents = TRUE,
    xmax = 100, ymax = 100)
  riskScatterRawFn(pos = c(1, 1), r1 = sampleData$r1, r2 = sampleData$r2, 
    col = "#0000BB40", pch = 19, cex = .8)

}
