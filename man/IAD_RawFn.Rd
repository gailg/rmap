\alias{IAD_RawFn}
\name{IAD_RawFn}
\title{
Plot a *raw* Individualized Attribute Diagram for a risk model
}
\description{
This function draws a *raw* individualized attribute 
diagram (IAD) for a risk model.  It is a "raw" function 
because it requires \code{setUpTrellisFn} to be called first
to set up the plotting region(s).

For a more user-friendly individualized attribute diagram 
function, see \code{IAD}, which sets up the plotting region, then 
draws the individualized attribute diagram.  This function is
designed to allow the user to create sophisticated multi-faceted
(trellis-style) graphics.
}
\usage{
IAD_RawFn(pos, rvu, color, lightCol)
}

\arguments{
  \item{pos}{
An integer vector with two elements.  Before this function is 
called, \code{setUpTrellisFn} must be called, which sets up a 
"grid" of graphing regions.  This \code{pos} argument specifies
the row and column in which the individualized attribute diagram 
should be drawn.
}
  \item{rvu}{
An object of class "rvu", which is obtained by \code{riskValidateUngrouped}.
}
  \item{color}{
The color of the continuous line depicting the nearest-neighbor 
estimates of observed risk for each value of assigned risk.  This 
color is also used as a border for the bootstrap confidence interval.
}
  \item{lightCol}{
If the "rvu" object included bootstrap confidence bands, then 
the shape created by the 95\% confidence bands is filled with this 
lighter color.  This color can optionally be semi-transparent.
}
}
\details{
If bootstrapping was used to obtain argument "rvu", then a lightly-shaded
polygon shape will appear depicting the 95\% confidence bands. 
}
\value{
Draws an individualized attribute diagram to the current graphics device.  The 
\code{rvu} object is returned invisibly.
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
  riskGroup = list(ungrouped = list(epsilon = 0.15, tStar = 10))
  rvu1b = riskValidateUngrouped(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  rvu2b = riskValidateUngrouped(e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)  
  
  setUpTrellisFn(2, 1, main = "Individualized ADs for Models 1 and 2")
  
  IAD_RawFn(pos = c(1, 1), rvu1b, col = "#FF0000", lightCol = "#FF666640")
  addTextToTrellisFn(pos = c(1, 1), "model 1")
  
  IAD_RawFn(pos = c(2, 1), rvu2b, col = "#0000FF", lightCol = "#6666FF40")
  addTextToTrellisFn(pos = c(2, 1), "model 2")

}
