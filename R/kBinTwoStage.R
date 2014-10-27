

kBinTwoStage = function(df, KKK, aaa, rCol = "r"){  
  names(df)[names(df) == rCol] = "r"
  or = order(df$r)
  df = df[or, ]
  cum = cumsum(aaa[df$c])
  df$k =  apply(
    sapply(seq(0, sum(aaa[df$c]), length.out = KKK + 1)[-1],function(tile) cum <= tile),
    1, function(row) min(which(row))
  )
  df[or,] = df
  df
}

# Wed Mar 16 11:14:55 PDT 2011
