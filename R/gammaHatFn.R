

gammaHatFn = function(baseArgs) {
  aaa = baseArgs$N_two_stage / baseArgs$n_two_stage
  aaaPerson = aaa[baseArgs$c]
  Nk = sapply(1:baseArgs$K, function(kkk) sum(aaaPerson[baseArgs$k==kkk]))
  Nk / sum(Nk)
}

# Wed Mar 16 10:48:40 PDT 2011
