
# This function does
# 1) computes affected and unaffected
# 2) computes crp's (with 2-stage weights)
# 3) returns crp's and the weights for those crp's
#    (weights for affected)


CRP_Fn = function(baseArgs = FALSE, extraArgs = FALSE) {
  
  aaa =  (baseArgs$N/baseArgs$n)[baseArgs$c]
  affected = baseArgs$e == 1
  unaffected = (baseArgs$e == 0 & baseArgs$t >  baseArgs$tStar - .01) | (baseArgs$e == 2)

  crp = sapply(baseArgs$r[affected], function(r1){
    sum(aaa[unaffected][baseArgs$r[unaffected] < r1]) / sum(aaa[unaffected])
  })
  
  ans = list(CRP = crp, a = aaa[affected])
  class(ans) = c("crp", class(ans))
  ans
}

# Tue Aug 23 10:39:39 PDT 2011
# Tue Sep 20 11:32:09 PDT 2011  Added class crp to the result of this function.


