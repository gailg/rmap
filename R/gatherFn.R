
## Fri Aug 12 11:28:37 PDT 2011
## Copied from inside
##    trunk/projects/riskModel/53-nearest-neighbor/03-scribbles/17e-destined-for-rmap-nne-mclapply-printing.R
## Tue Aug 30 15:19:10 PDT 2011
##   bsSamp was changed from a data.frame to a matrix.
##   So we need to use bsSamp[, "rho"] instead of bsSamp$rho, and similarly for bsSamp[,"piHatNN"]


gatherFn = function(bootstrapsRaw, all_rho) {
  
  bsPiHats = do.call(rbind, lapply(bootstrapsRaw, function(bsSamp) {
    ans = rep(NA, length(all_rho))
    ans[all_rho %in% bsSamp[, "rho"]] = bsSamp[,"piHatNN"]
    ans
  }))
  
  colnames(bsPiHats) = paste("rho_", seq_along(all_rho), sep = "")
  rownames(bsPiHats) = paste("bs", seq_len(length(bootstrapsRaw)), sep = "")

  bsPiHats
}

