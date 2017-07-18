#' Weighted empirical cumulative distribution function
#' 
#' Compute or plot the empirical cumulative distribution function
#' allowing for weights.  Extends ecdf
#' 
#' @param x A numeric vector whose cumulative distribution you
#'   wish to compute.
#' @param aaa A numeric vector of the same length as \code{x} 
#'   containing the weights.
#' @return A function of class \code{ecdf}
#' @examples 
#' set.seed(2)
#' twoStageSample = df_twoStage(40)
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = round(xxx$r, 3)
#' tStar = 10
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 20
#' confidenceLevel = 0.95
#' multicore = FALSE
#' verbose = TRUE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap,
#'                       confidenceLevel, multicore, verbose)
#' G_Fn_unweighted = ecdf(r) 
#' 
#' aaa = (baseArgs$N_two_stage / baseArgs$n_two_stage)[baseArgs$c]
#' aaa
#' G_Fn = ecdf_two_stage(r, aaa)
#' plot(G_Fn_unweighted, col = rgb(0, 0, 1, 0.7), cex = 0.5) # small blue
#' plot(G_Fn, add = TRUE, col = rgb(1, 0, 0, 0.3))           # light red
#' @export
#'   
ecdf_two_stage = function (x, aaa) 
{
    x <- sort(x)
    n <- length(x)
    if (n < 1) 
        stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    # DJ replaced from      [                                                           ]
    #                                        [        ] added this section in 08-dj-response...R
    #                                                   fixes an error, matches DJ to GG

   
    rval <- approxfun(vals, cumsum(tapply(aaa[order(x)], match(x, vals), sum)) / sum(aaa),
                      method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    assign("nobs", n, envir = environment(rval))
    attr(rval, "call") <- sys.call()
    rval
}
