\name{riskScatter}
\alias{riskScatter}
\title{
Draw a scatter plot of two risk models (or two sets of CRPs)
}
\description{
This function draws a scatter plot of two risk models (or two
sets of CRPs) using the same "style" of graphics as the other 
rmap graphing functions.  
}
\usage{
riskScatter(
  r1, r2, 
  rvpar = rvparFn(col = "#0000FF40"), 
  ocall = NULL)
}

\arguments{
  \item{r1}{
Risk values (using risk model 1) for each person in a data set. 
}
  \item{r2}{
Risk values (using risk model 2) for each person in the *same* data set. 
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
An alternative would be the native functions \code{plot} or \code{xyplot} 
in the lattice package.  This function is offered to accommodate
 the same "style" of graphics as the other 
rmap graphing facilities.  
}
\value{
Draws a scatter plot to the current graphics device using the 'rmap'.
Invisibly returns a list of r1 and r2.
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
  
  riskScatter(sampleData$r1, sampleData$r2)
  
  riskScatter(sampleData$r1, sampleData$r2,
              rvpar = rvparFn(axesInPercents = TRUE, inflate = 1,
                atX = seq(0, 100, 20), atY = seq(0, 100, 20),
                col = "#00BB0040", comment = "A prettier picture"))
  
  rvu1 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15, tStar = 10)), 
    bootstrap = FALSE,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  rvu2 = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15, tStar = 10)), 
    bootstrap = FALSE,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  riskScatter(rvu1$AUC$CRP$CRP, rvu2$AUC$CRP$CRP,
    rvpar = rvparFn(col = "#44008840",
    xlab = "CRP for model 1", ylab = "CRP for model 2",
    xmax = 100, ymax = 100,
    main = "CRP Scatter Plot", inflate = 1.01,
    atX = seq(0, 100, by = 20), atY = seq(0, 100, by = 20),
    comment = "riskScatter works for CRPs too!"))
}
