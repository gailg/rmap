\name{setUpTrellisFn}
\alias{setUpTrellisFn}

\title{
Sets up the trellis-style graphing region(s)
}
\description{
This function sets up the trellis-style graphing region(s).
This function is designed to work in tandem with the "raw"
family of rmap graphing functions (\code{attributeDiagramRawFn}, 
\code{IAD_RawFn}, \code{riskScatterRawFn}, and \code{riskDensityRawFn}).
This function only sets up the graphing region.  It does not add
any meaningful graphics.
}
\usage{
setUpTrellisFn(nrow, ncol, main = NULL, 
  xlab = if (axesInPercents) "assigned risk (\%)" else "assigned risk", 
  ylab = if (axesInPercents) "observed risk (\%)" else "observed risk", 
  xmax = 100, ymax = 100, axesInPercents = TRUE, 
  inflate = 1.1, newpage = TRUE, atX = NULL, atY = NULL)
}

\arguments{
  \item{nrow}{
An integer. How many rows of trellis-style graphics regions should be created?
}
  \item{ncol}{
An integer.  How many columns of trellis-style graphics regions should be created?
}
  \item{main}{
A character string. The title of the graphic.
}
  \item{xlab}{
A character string. The x-label of the graphic.
}
  \item{ylab}{
A character string. The y-label of the graphic.
}
  \item{xmax}{
A numeric value.  The maximum x value for the axes of the graphic.  
This value should be a percentage if axesInPercents is TRUE. 
}
  \item{ymax}{
A numeric value.  The maximum y value for the axes of the graphic. 
This value should be a percentage if axesInPercents is TRUE.  
}
  \item{axesInPercents}{
A logical value. Should axes be represented in percentages?
}
  \item{inflate}{
A multiplier applied to xmax and ymax to get the actual limits of the 
graphics region.  The default value is 1.1.  This allows a little 
"cushion" past the end of your data, so your data is not plotted 
on the edges of your graph.
}
  \item{newpage}{
A logical value.  If TRUE, grid.newpage() will be called before 
drawing the graphing region(s).  If FALSE, the graphing region(s) 
will be drawn into the current viewport.
}
  \item{atX}{
A numeric vector of locations for the tick-marks on the x-axis.  
These should be specified in percentages if axesInPercents is TRUE.  
If NULL (the default value), then "smart" tick marks are computed
automatically.
}
  \item{atY}{
A numeric vector of locations for the tick-marks on the y-axis.  
These should be specified in percentages if axesInPercents is FALSE. 
If NULL (the default value), then "smart" tick marks are computed
automatically.
}
}
\details{

}
\value{
A list containing information about the type of trellis 
set up that has been drawn.
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
  sampleData = df_randomSample_r1_r2(NTotal = 200)
  
  rv1 = riskValidate(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
    
  rv2 = riskValidate(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
    
  rvu1 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15, tStar = 10)), 
    bootstrap = 20, rvpar = FALSE, 
    multicore = FALSE, verbose = TRUE)

  rvu2 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15, tStar = 10)), 
    bootstrap = 20, rvpar = FALSE, 
    multicore = FALSE, verbose = TRUE)
 
  setUpTrellisFn(2, 2, main = "Attribute and Individualized Attribute Diagrams")
  
  attributeDiagramRawFn(pos = c(1, 1), rv1, col = "#FF0000", lightCol = "#FF666640")
  addTextToTrellisFn(pos = c(1, 1), "model 1")
  
  attributeDiagramRawFn(pos = c(1, 2), rv2, col = "#0000FF", lightCol = "#6666FF40")
  addTextToTrellisFn(pos = c(1, 2), "model 2")
  
  IAD_RawFn(pos = c(2, 1), rvu1, col = "#FF0000", lightCol = "#FF666640")
  addTextToTrellisFn(pos = c(2, 1), "model 1")
  
  IAD_RawFn(pos = c(2, 2), rvu2, col = "#0000FF", lightCol = "#6666FF40")
  addTextToTrellisFn(pos = c(2, 2), "model 2")
  
  
  setUpTrellisFn(1, 2, main = "Strange example")
  downViewport("1 1")
  setUpTrellisFn(2, 1, newpage = FALSE)

}
