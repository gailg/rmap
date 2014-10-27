

## VVVFn = function(baseArgs = FALSE, extraArgs = FALSE) {
##   gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
##   lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)

##   VVVgamma = if(length(gammaHat) >= 3) {
##     diag(gammaHat[1:(baseArgs$K - 1)]) - (gammaHat[1:(baseArgs$K - 1)] %*% t(gammaHat[1:(baseArgs$K - 1)] ) )
##   } else if(length(gammaHat) == 2) {
##     gammaHat[1:(baseArgs$K - 1)] - (gammaHat[1:(baseArgs$K - 1)] %*% t(gammaHat[1:(baseArgs$K - 1)] ) )
##   } else if(length(gammaHat) == 1) {
##     matrix(0, 0, 0)
##   }

##   lambdaDenoms = lapply(seq_len(baseArgs$K), function(kkk) lambdaHat[[kkk]]$denom)

##   lambdaBoxes = lapply( 1:baseArgs$K, function(kkk) {
##     lambdaHat[[kkk]]$lambdaHat
##   })

##   VBoxes = lapply( seq_len(baseArgs$K), function(kkk) {
##     lambdaBoxes_k = lambdaBoxes[[kkk]]
##     MMM = ncol(lambdaBoxes_k)
##     squares = lapply(seq_len(MMM), function(mmm) {
##       lambda1and2 = lambdaBoxes_k[, mmm]
##       diag(lambda1and2) - ( lambda1and2 %*% t(lambda1and2) )
##     })
##     lambDenoms_k = lambdaDenoms[[kkk]]
##     squaresArray = array( unlist(squares) / rep(lambDenoms_k, rep(4, length(lambDenoms_k))), dim = c(2, 2, MMM))
##     V_k = sum(baseArgs$N)  * rbind(cbind( diag(squaresArray[1,1,]), diag(squaresArray[1,2,])),
##       cbind( diag(squaresArray[2,1,]), diag(squaresArray[2,2,])))
##     V_k
##   })

##   VVVlambda = BlockDiagFn(VBoxes)
  
##   VVV = if(baseArgs$K > 1 ) {
##     BlockDiagFn(list(VVVgamma, VVVlambda))
##   } else {
##     VVVlambda
##   }

##   VVV  
## }

## # Wed Mar 16 11:00:46 PDT 2011
