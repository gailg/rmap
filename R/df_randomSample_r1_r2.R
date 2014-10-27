

df_randomSample_r1_r2 = function(NTotal = 1000, distribution = "lognormal",
  param1 = -1.8, param2 = 0.4,
  eta0 = .1, eta2 = .1, tStar = 10, KKK = 5) {
  
  df = df_randomSample(NTotal = NTotal, distribution = distribution, param1 = param1,
    param2 = param2, eta0 = eta0, eta2 = eta2, tStar = tStar, KKK = KKK)

  r1 = df$r
  r2 = plogis(qlogis(r1) + rnorm(n = length(r1), mean = 0, sd = 0.5))
  df = df[, c("e","t","c")]
  df = cbind(df, data.frame(r1,r2))
  df
}

# Fri Mar 25 13:59:33 PDT 2011

