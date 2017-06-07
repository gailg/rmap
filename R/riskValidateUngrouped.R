

riskValidateUngrouped = function(
  e, t, r, tStar, design = "randomSample", riskGroup, bootstrap = FALSE,
  rvpar = rvparFn(), multicore = FALSE, verbose = FALSE) {

  rvu = riskValidateUngroupedExternalFn(
    e = e, t = t, r = r, tStar, design = design,
    riskGroup = riskGroup,
    bootstrap = bootstrap, multicore = multicore, verbose = verbose) 

  ocall = deparse(match.call())

  if( is.list(rvpar) && "rvpar" %in% class(rvpar) ) {
    IAD(rvu, rvpar = rvpar, ocall = ocall)
  } else {
#     cat(paste("Note: No plot produced.  If an IAD is desired, ", 
#               "rvparFn() should be used to set the argument 'rvpar'", 
#               sep = "\n"))
#     cat("\n")
  }
  
  rvu = list(PNN = rvu$PNN)
  class(rvu) = c("rvu", class(rvu))
  rvu

}

# Sat Sep 10 09:02:21 PDT 2011
# Replace epsilon and tStar with riskGroup.

#Sun Sep 11 06:43:26 PDT 2011
#design = "randomSample"
