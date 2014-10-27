

df_twoStage = function(NTotal = 1000, distribution = "lognormal",
  param1 = -1.8, param2 = 0.4,
  eta0 = .1, eta2 = .1, tStar = 10, KKK = 5,
  ppp = c(A = 1., B = 0.5) )
{
  dataf = df_raw(NTotal, distribution, param1, param2, eta0, eta2, tStar)
  dataf$c = LETTERS[(!dataf$e == 1) + 1]
  keep = rbinom(NTotal, 1, ppp[dataf$c])
  NNN = c(A = sum(dataf$c == "A"), B = sum(dataf$c == "B") )
  nnn = c(A = sum(dataf$c == "A" & keep == 1), B = sum(dataf$c == "B" & keep == 1))
  aaa = NNN/nnn
  dataf = dataf[as.logical(keep), ]
  list(d = kBinTwoStage(dataf, KKK, aaa), N = NNN, n = nnn)
}

# Wed Mar 16 11:15:35 PDT 2011
