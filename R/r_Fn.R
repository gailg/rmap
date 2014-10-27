r_fn = function(eta1, eta2, tStar){
  (eta1 / (eta1 + eta2)) * (1 - exp(-(eta1 + eta2) * tStar))
}
