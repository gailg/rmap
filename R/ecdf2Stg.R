
## Fri Aug 12 11:28:37 PDT 2011
## Copied from inside
##    trunk/projects/riskModel/53-nearest-neighbor/03-scribbles/17e-destined-for-rmap-nne-mclapply-printing.R



ecdf2Stg = function (x, aaa) 
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
