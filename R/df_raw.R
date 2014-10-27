

df_raw = function(NTotal, distribution, param1, param2, eta0, eta2, tStar) {
  eta1 = w = switch(distribution,
           lognormal = exp(rnorm(n = NTotal, mean = param1, sd = param2)),
           beta = rbeta(NTotal, shape1 = param1, shape2 = param2)
           )
  tEvents = matrix(c( 
    pmin(rexp(NTotal, eta0), tStar),  
    rexp(NTotal, eta1),                   
    rexp(NTotal, eta2)), 
    ncol = 3)         
  te = t(apply(tEvents, 1, function(row) {
    c(min(row), which.min(row)-1)
  }))
  numerator = eta1
  denominator = numerator + eta2
  r = (numerator / denominator) * (1 - exp(-(eta1 + eta2) * tStar))
  data.frame(e = te[,2], t = te[,1], w = w, r = r)
}


## df_oneStage = function(NTotal = 1000, distribution = "lognormal",
##   param1 = -1.8, param2 = 0.4,
##   eta0 = .1, eta2 = .1, tStar = 10, KKK = 5)
## {
##   dataf = df_raw(NTotal, distribution, param1, param2, eta0, eta2, tStar)
##   kBinOneStage(dataf, KKK)
## }

## # Wed Mar 16 11:15:17 PDT 2011
