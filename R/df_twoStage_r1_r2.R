

df_twoStage_r1_r2 = function(NTotal = 1000, distribution = "lognormal", param1 = -1.8, 
  param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10, KKK = 5, ppp = c(A = 1, B = 0.5)) {
  ddd = df_twoStage(NTotal = NTotal, distribution = distribution, param1 = param1, 
    param2 = param2, eta0 = eta0, eta2 = eta2, tStar = tStar, KKK = KKK, 
    ppp = ppp)
  df = ddd$d
  NNN = ddd$N
  nnn = ddd$n
  r1 = df$r
  r2 = plogis(qlogis(r1) + rnorm(length(r1), mean = 0, sd = 0.5))
  df = df[, c("e","t","c")]
  df = cbind(df, data.frame(r1,r2))
  list(d = df, N = NNN, n = nnn)
}

# Wed Mar 16 11:17:18 PDT 2011
