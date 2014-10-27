
vvTsumFnC = function(uuu, mult = rep(1, nrow(uuu))) {
  adder = matrix(0, ncol(uuu), ncol(uuu))
  uuT_C = .C("vvTsumFn", uuu = t(uuu), nrow = nrow(uuu), ncol = ncol(uuu), adder = adder, mult = mult)$adder
  uuT_C
}

# Fri Apr  1 11:17:33 PDT 2011

