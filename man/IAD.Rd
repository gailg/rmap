\name{IAD}
\alias{IAD}
\title{
Plot an Individualized Attribute Diagram for a risk model
}
\description{
This function draws an individualized attribute diagram (IAD) for 
one or more risk models.  The individualized attribute
diagram compares assigned risk to observed risk using an
ungrouped nearest-neighbor approach.
}
\usage{
IAD(
  rvus, 
  rvpar = rvparFn(lightCol = paste(rvparFn()$col, "30", sep = "")), 
  ocall = NULL)
}

\arguments{
  \item{rvus}{
If the user wishes to draw an individualized attribute diagram for 
one risk model, this argument must be a single object of class "rvu".  
If the user wishes to draw an individualized attribute diagram for 
multiple risk models, this argument must be a list, where each element
is an object of class "rvu".  An "rvu" object can be obtained by
calling the function \code{riskValidateUngrouped}. 

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
If bootstrapping was used to obtain an "rvu" object, then continuous 
95\% confidence bands will be drawn using a lighter color. 
}
\value{
Draws an individualized attribute diagram to the current graphics 
device.  The \code{rvus} objects are returned invisibly.
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
  riskGroup = list(
    ungrouped = list(epsilon = 0.15, tStar = 10))
  rvu1 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup,
    rvpar = FALSE)
  rvu1

  rvu2 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", riskGroup = riskGroup,
    rvpar = FALSE)
  rvu2
  
  rvu1b = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  rvu2b = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  IAD(rvu1)
  
  IAD(
    list(rvu1, rvu2), 
    rvpar = rvparFn(col = c("red", "blue"), 
    comment = "no bootstraps"))
  
  IAD(
    list(rvu1b, rvu2b), 
    rvpar = rvparFn(
      col = c("red", "blue"),
      atX = seq(0, 100, 20), atY = seq(0, 100, 20),
      comment = "20 bootstraps for confidence bands"))
}
