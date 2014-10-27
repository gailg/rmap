\name{addTextToTrellisFn}
\alias{addTextToTrellisFn}
\title{
Add text to a rmap trellis-style graphic
}
\description{
This function adds text to a rmap trellis-style graphic.
}
\usage{
addTextToTrellisFn(pos, text, where = "topleft", ...)
}

\arguments{
  \item{pos}{
An integer vector with two elements.  Before this function is 
called, \code{setUpTrellisFn} must be called, which sets up a 
"grid" of graphing regions.  This \code{pos} argument specifies
the row and column in which the text 
should be drawn.
}
  \item{text}{
A character string (or vector) containing the text to be drawn.
}
  \item{where}{
A character vector.  Either "topleft", "topright", "bottomleft", 
or "bottomright".  Which corner of the graphing region should the text be 
drawn to?
}
  \item{\dots}{
Additional arguments that get passed to grid.text()
}
}
\details{

}
\value{
Draws the text, then invisibly returns that text as a character string
(or vector).  
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
  addTextToTrellisFn(pos = c(1,1), 
    text = "Data generated from df_randomSample_r1_r2", where = "topleft")
  addTextToTrellisFn(pos = c(1,1), 
    text = "Darker regions indicate overprinting", where = "bottomright")

}
