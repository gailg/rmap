\name{attributeDiagramRawFn}
\alias{attributeDiagramRawFn}
\title{
Plot a *raw* attribute diagram for a risk model
}
\description{
This function draws a *raw* attribute diagram for a risk model.
It is a "raw" function because it requires \code{setUpTrellisFn}
to be called first to set up the plotting region(s).  

For a more user-friendly attribute diagram function, see 
\code{attributeDiagram}, which sets up the plotting region(s), then
draws the attribute diagram.  This function is designed to 
allow the user to create sophisticated multi-faceted (trellis-style) 
graphics. 
}
\usage{
attributeDiagramRawFn(pos, rv, color, lightCol = "#FFFFFF00")
}

\arguments{
  \item{pos}{
An integer vector with two elements.  Before this function is 
called, \code{setUpTrellisFn} must be called, which sets up a 
"grid" of graphing regions.  This \code{pos} argument specifies
the row and column in which the attribute diagram should be drawn.
}
  \item{rv}{
An object of class "rv", which is obtained by \code{riskValidate}.  
}
  \item{color}{
The color to be used to draw the "I" shaped confidence bands of the 
attribute diagram.
}
  \item{lightCol}{
If the "rv" object included bootstrap confidence intervals of piHat, 
then these bootstrap confidence intervals are depicted with a lighter
color rectangle.  This color value can optionally be semi-transparent.
}
}
\details{
If bootstrapping was used to obtain argument "rv", then a
  lightly-shaded box will appear behind the "I" shaped confidence bands.
  These lightly-shaded boxes represent bootstrap confidence
  intervals.  
}
\value{
Draws an attribute diagram to the current graphics device.  The 
\code{rv} object is returned invisibly.
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
  
  rv1 = riskValidate(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
  
  rv2 = riskValidate(e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
  
  setUpTrellisFn(2, 1, main = "Attribute Diagrams for Models 1 and 2")
  
  attributeDiagramRawFn(pos = c(1, 1), rv1, col = "#FF0000", lightCol = "#FF666640")
  addTextToTrellisFn(pos = c(1, 1), "model 1")
  
  attributeDiagramRawFn(pos = c(2, 1), rv2, col = "#0000FF", lightCol = "#6666FF40")
  addTextToTrellisFn(pos = c(2, 1), "model 2")
}
