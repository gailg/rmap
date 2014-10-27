## Fri Aug 12 11:28:37 PDT 2011
## Copied from inside
##    trunk/projects/riskModel/53-nearest-neighbor/03-scribbles/17e-destined-for-rmap-nne-mclapply-printing.R


interpolateOneBsFn = function(oneBs, all_rho) {

  ans = oneBs

  # Fill in left endpoint(s) with the first non-NA value
  if(is.na(ans[1])) {
    whichLeftEnd = seq_len(which(duplicated(is.na(ans)) == FALSE)[2] - 1)
    ans[whichLeftEnd] = ans[max(whichLeftEnd) + 1]
  } 


  # Fill in the right endpoint(s) with the last non-NA value
  if(is.na(ans[length(ans)])) {
    whichRightEnd = length(ans) - (seq_len(which(duplicated(is.na(rev(ans))) == FALSE)[2] - 1) - 1)
    ans[whichRightEnd] = ans[min(whichRightEnd) - 1]
  }

  # Fill in any NA's in the middle by connecting a line from the first
  # non-NA value on the left to the first non-NA value on the right.
  # Interpolate the y = piHatNN value for this missing NA with x = rho.

  whichMid = which(is.na(ans))

  for(ii in whichMid) {

    # 1.) Find left point.
    left = ii - 1
    while( is.na(ans[left]) ) left = left - 1

    # 2.) Find right point
    right = ii + 1
    while( is.na(ans[right]) ) right = right + 1

    ans[ii] = (((all_rho[ii] - all_rho[left]) * (ans[right] - ans[left]) ) /
               (all_rho[right] - all_rho[left])) + ans[left]  
  }

  ans
}
