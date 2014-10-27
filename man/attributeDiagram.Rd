\name{attributeDiagram}
\alias{attributeDiagram}

\title{
Plot an attribute diagram for a risk model
}
\description{
This function draws an attribute diagram for one or more risk models.
This attribute diagram compares rSummary to piHat (see
\code{?riskValidate}) 
}

\usage{
attributeDiagram(rvs, 
  rvpar = rvparFn(
    xlab = "assigned risk (percent)", 
    ylab = "observed risk (percent)"),
  ocall = NULL)
}

\arguments{
  \item{rvs}{
If the user wishes to draw an attribute diagram for one risk model,
  this argument must be a single object of class "rv". If the user
  wishes to draw an attribute diagram for multiple risk models, this
  argument must be a list, where each element is an object of class
  "rv".  An "rv" object can be obtained by calling the function
  \code{riskValidate}. 
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
  call in the box under the attribute diagram.  This argument should
  be left as the default value. 
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
  \code{rvs} objects are returned invisibly.
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
  
  rv1 = riskValidate(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
  
  rv2 = riskValidate(e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample",
    riskGroup = list(K = 5), rSummary = "mean",
    bootstrap = 20, rvpar = FALSE)
  
  attributeDiagram(rv1)
  
  attributeDiagram(list(rv1, rv2), rvpar = rvparFn(col = c("red", "blue"),
    annotate = FALSE,
    comment = "red = model1, blue = model2"))
  
  attributeDiagram(rv1, rvpar = rvparFn(annotate = FALSE, axesInPercents = FALSE,
    xmax = 0.8, ymax = 0.9, inflate = 1,
    atX = c(0, 0.2, 0.4, 0.6), atY = c(0, 0.3, 0.6, 0.9),
    col = "darkgreen", main = "Sample Attribute Diagram",
    xlab = "assigned risk for model r1",
    ylab = "observed risk for model r1",
    comment = "Trying out different rvpar options"))
}
