

vvTsumFn = function(uuu, mult = rep(1, nrow(uuu))) {
  NB1 = matrix( rep(0, ncol(uuu)^2), nrow = ncol(uuu))          
  for(n in 1:nrow(uuu)) {                                           
    row = uuu[n, ]
    u_uT_a = (row %*% t(row) ) * mult[n]
    NB1 = NB1 + u_uT_a
  }
  NB1
}

# Wed Mar 16 11:02:23 PDT 2011
