

kBinOneStage = function(df, K, rCol = "r"){
  NNN = nrow(df)
  nnn = NNN
  names(NNN) = names(nnn) = "A"
  df$c = 'A'
  aaa = NNN/nnn
  kBinTwoStage(df, K, aaa, rCol)
}

# Wed Mar 16 11:14:32 PDT 2011
