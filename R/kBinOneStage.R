#' @title bin subjects into risk groups assuming random sampling
#' 
#' @description 
#' This function is used internally to bin subjects into 
#' risk groups, determined by each subject's assigned risks.
#' Each bin will hold approximately the same number of 
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
#'   param1 = -1.8, param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10)
#' head(dataf)
#' kbin1stg = kBinOneStage(dataf, K = 5)
#' head(kbin1stg)
#' 
#' @export

kBinOneStage = function(df, K, rCol = "r"){
  NNN = nrow(df)
  nnn = NNN
  names(NNN) = names(nnn) = "A"
  df$c = 'A'
  aaa = NNN/nnn
  kBinTwoStage(df, K, aaa, rCol)
}

