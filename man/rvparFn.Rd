\name{rvparFn}
\alias{rvparFn}

\title{
Set up attribute diagram plotting parameters
}
\description{
This function is similar to \code{gpar()} in the 'grid' package.  
The output of this function is an object of class "rvpar".  
In this package, several functions are used to create graphics
which may help assess the performance of a risk model.  These 
high-level graphing functions include: \code{riskValidate}, 
\code{riskValidateUngrouped}, \code{attributeDiagram}, \code{IAD},
\code{riskScatter}, and \code{riskDensity}. Each of these functions
has a \code{rvpar} argument, which expects a list object with class "rvpar". 
This list object is generated using the \code{rvparFn} function.  

This function can be used to define graphing options for each of these
functions.  If this function is called with no arguments, 
default values are used for the graphing parameters.  See the arguments 
section for details about the graphing parameters that can be specified.
}
\usage{
rvparFn(...)
}

\arguments{
  \item{\dots}{
  There are fifteen named arguments that can be specified with this function: col, 
  lightCol, xmax, ymax, inflate, comment, annotate, pch, cex, axesInPercents,
  atX, atY, xlab, ylab, main
  
  \itemize{
    \item \code{col}: A character vector of colors.  Some graphics
    functions will draw representations of two or more models concurrently, so
    in such cases, multiple color values will be used from this
    vector. 

    \item \code{lightCol}: A character vector of lighter colors.  Some 
    graphics functions will draw representations of two or more models
    concurrently, so in such cases, multiple lighter color values will
    be used.  It may make sense to make these lighter versions of the
    \code{col} vector (see above).  For the functions \code{IAD} and 
    \code{riskDensity}, it may be useful to specify this vector as a
    series of transparent colors so you can see "under" shaded parts
    of the graphics.

    \item \code{xmax}: The maximum x-value for the graphing region.  
    If specified, this is multiplied by \code{inflate} (default value
    of 1.1) to get the maximum x-limit for the graphic.  If this is
    not specified, it will be computed intelligently by the package.
    This should be represented as a percentage if axesInPercents ==
    TRUE.   

    \item \code{ymax}: The maximum y-value for the graphing region.  
    If specified, this is multiplied by \code{inflate} (default value
    of 1.1) to get the maximum y-limit for the graphic.  If this is
    not specified, it will be computed intelligently by the package.
    This should be represented as a percentage if axesInPercents ==
    TRUE. 
    
    \item \code{inflate}: A multiplier to allow a small amount of 
    white space to be added to past xmax and ymax values.  The default 
    value is 1.1.
    
    \item \code{comment}: A single character string.  This comment
    will appear on the first line of the box for footnotes on an
    attribute diagram. If this argument is omitted, no comment will
    show up on the line of the box for footnotes 

    \item \code{annotate}: A single logical value.  Should annotations
    be drawn in the corner of the graphic? For an attribute diagram,
    annotations include a Hosmer-Lemeshow Chi Squared
    goodness of fit statistic and corresponding p-value, an AUC
    estimate and corresponding 95\% confidence interval, and a
    standard deviation estimate for the model, and corresponding 95\%
    confidence interval.  These annotations will appear for functions
    \code{attributeDiagram} and \code{riskValidate}.  For 
    an individualized attribute diagram, annotations include an AUC
    estimate and corresponding 95\% confidence interval.  These
    annotations will appear for functions \code{IAD} and
    \code{riskValidateUngrouped}. 

    \item \code{pch}:  The plotting character to be used for the 
    function \code{riskScatter}.  The default value is 19, which 
    is a filled-in circular dot.

    \item \code{cex}: The character expansion multiplier to determine
    the size of the dots.

    \item \code{axesInPercents}: A logical value. Should the axes be
    represented in percentages? 

    \item code{atX}: Intervals for the axis ticks on the x-axis.  This 
    should be a percentage if axesInPercents is TRUE.  If this is
    unspecified, the package will automatically determine x-axis
    ticks.

    \item code{atY}: Intervals for the axis ticks on the y-axis.  This
    should be a percentage if axesInPercents is TRUE.  If this is
    unspecified, the package will automatically determine y-axis
    ticks.  

    \item \code{xlab}: A character string containing the text that
    should label the x-axis.  If this is unspecified, a default value 
    will be used (and this default value differs depending on the
    calling function).  

    \item \code{ylab}: A character string containing the text that
    should label the y-axis.  If this is unspecified, a default value
    will be used (and this default value differs depending on the
    calling function). 

    \item \code{main}: The title for the graphic.  If unspecified, the
    graphic will not have a title.  

    }
  }
}
\details{
	
}
\value{
A list of class "rvpar". 
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
riskValidate, riskValidateUngrouped, attributeDiagram, IAD,
riskScatter, and riskDensity

}
\examples{
d = df_randomSample()
rv = riskValidate(e = d$e, t = d$t, r = d$r, design = "randomSample", 
   model = list(K = 3), rSummary = "mean",
   bootstrap = 200, rvpar = rvparFn()) # default plotting parameters

rv2 = riskValidate(e = d$e, t = d$t, r = d$r, design = "randomSample", 
model = list(k = d$k), rSummary = "mean", bootstrap = FALSE, rvpar =
FALSE)  
 # no plot from riskValidate

# Specifying multiple colors and a comment in attributeDiagram
attributeDiagram(rvs = list(rv, rv2), rvpar = rvparFn(col =
c("magenta", "blue"), comment = "my comment"))

# Specifying many different graphing options:
attributeDiagram(rv, rvpar = rvparFn(annotate = FALSE, axesInPercents = FALSE,
  xmax = 0.8, ymax = 0.9, inflate = 1,
  atX = c(0, 0.2, 0.4, 0.6), atY = c(0, 0.3, 0.6, 0.9),
  col = "darkgreen", main = "Sample Attribute Diagram",
  xlab = "assigned risk for model r1",
  ylab = "observed risk for model r1",
  comment = "Trying out different rvpar options"))

  riskScatter(sampleData$r1, sampleData$r2,
              rvpar = rvparFn(axesInPercents = TRUE, inflate = 1,
                atX = seq(0, 100, 20), atY = seq(0, 100, 20),
                col = "#00BB0040", comment = "A prettier picture"))
}
