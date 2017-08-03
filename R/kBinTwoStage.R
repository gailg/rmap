#' @title bin subjects into risk groups assuming two-stage sampling
#' 
#' @description 
#' This function is used internally to bin subjects into 
#' risk groups, determined by each subject's assigned risks.
#' Each bin will hold approximately the same number (weighted
#' by two-stage sampling) of 
#' subjects.  Subjects with the smallest assigned risks will be 
#' in risk group 1, and subjects with the hightest assigned 
#' risk will be in risk group K, where K = total number of risk groups. 
#' 
#' This function is used only within the simulation functions beginning 
#' with \code{df}.
#' 
#' @param df
#' A data frame, containing a column that will determine binning.
#' In this package it is simulated data set.  
#' 
#' @param K
#' An integer vector of length 1 describing the total number
#' of desired risk groups.
#' 
#' @param aaa
#' A named vector containing the two-stage sampling weights 
#' code{N/n}.
#' 
#' @param rCol 
#' The name of the column in the data.frame containing 
#' assigned risks that will determine the binning.
#' 
#' @return A data.frame containing the input \code{df}, but with
#' the columns \code{c = "A"} and \code{k} added. \code{k} is an 
#' integer from \code{1} through \code{K} specifying which bin.
#' 
#' @examples 
#' dataf = df_raw(NTotal = 1000, distribution = "lognormal", 
#' param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10)
#' dataf$c = LETTERS[(!dataf$e == 1) + 1]
#' ppp = c(A = 1., B = 0.5)      
#' keep = rbinom(1000, 1, ppp[dataf$c])
#' NNN = c(A = sum(dataf$c == "A"), B = sum(dataf$c == "B") )
#' nnn = c(A = sum(dataf$c == "A" & keep == 1), B = sum(dataf$c == "B" & keep == 1))
#' aaa = NNN/nnn
#' dataf = dataf[as.logical(keep), ]
#' head(dataf)
#' K = 4
#' kbin2stg = kBinTwoStage(dataf, K, aaa)
#' head(kbin2stg)
#' 
#' @export

kBinTwoStage = function(df, K, aaa, rCol = "r"){  
  names(df)[names(df) == rCol] = "r"
  or = order(df$r)
  df = df[or, ]
  cum = cumsum(aaa[df$c])
  df$k =  apply(
    sapply(seq(0, sum(aaa[df$c]), length.out = K + 1)[-1],function(tile) cum <= tile),
    1, function(row) min(which(row))
  )
  df[or,] = df
  df
}

# Wed Mar 16 11:14:55 PDT 2011
