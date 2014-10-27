

df_randomSample = function(NTotal = 1000, distribution = "lognormal",
  param1 = -1.8, param2 = 0.4,
  eta0 = .1, eta2 = .1, tStar = 10, KKK = 5)
{
  dataf = df_raw(NTotal, distribution, param1, param2, eta0, eta2, tStar)
  kBinOneStage(dataf, KKK)
}

# Fri Mar 25 13:59:01 PDT 2011

