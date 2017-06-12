baseArgsFn = function(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, multicore = FALSE, verbose = FALSE) {

  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 1: Checking vectors, and putting them in order of r.
  ## vecs is temporary, and is only used in this section.

  # browser()
  
  vecs = vector("list", 6)
  vecs[[1]] = e
  vecs[[2]] = t
  vecs[[3]] = r
  if( "c" %in% names(design)) vecs[[4]] = design$c
  if( "k" %in% names(riskGroup)) vecs[[5]] = riskGroup$k
  # "2017-06-12 09:28:47 PDT" GG added to handle weighted analysis
  if( "cohortCategory" %in% names(design)) vecs[[6]] = design$cohortCategory
  names(vecs) = c("e", "t", "r", "c", "k", "cohortCategory")

  # Vector checks:
  if(length(unique(sapply(vecs[sapply(vecs, function(vec) !is.null(vec))], length))) != 1) {
    stop("e, t, r (and design$c, riskGroup$k, design$cohortCategory, design$targetCategory if supplied) must all be the same length")
  }

  if(!all(is.numeric(unlist(vecs[c("e", "t", "r", "k")])))) {
    stop("e, t, r (and riskGroup$k) must be numeric vectors.")
  }

  if( !is.null(vecs[["c"]]) && !is.character(vecs[["c"]])) {
    stop("design$c must be a character vector.")
  }
  
  # "2017-06-12 09:28:47 PDT" GG I need to add something about cohortCategory and targetCategory possible errors

  # New Tue Apr 26 15:16:04 PDT 2011 >>> 
  if( !all( vecs[["e"]] %in% c(0, 1, 2) ) )
    stop("every 'e' value must be 0, 1, or 2")
  # <<<


  ord = order(vecs$r)   ###DJDJ
  vecs = lapply(vecs, function(vec) vec[ord])
  # Now the vectors are ordered by r.
  # look at cleanUpDf in old code.

  
  e = vecs[["e"]]
  t = vecs[["t"]]
  r = vecs[["r"]]
  if( "c" %in% names(design)) design$c = vecs[["c"]]
  if( "k" %in% names(riskGroup)) riskGroup$k = vecs[["k"]]
  # "2017-06-12 09:28:47 PDT" GG added to handle weighted analysis
  if( "cohortCategory" %in% names(design)) design$cohortCategory = vecs[["cohortCategory"]]
  rm(vecs)
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 2: Design checks:
  
  if(length(design) == 1 && is.character(design) && design == "randomSample") {
    # this code segment is for setting up oneStage

    design = list(sampling = "oneStage")
    design$N = structure(length(e), .Names = "A")
    design$n = structure(length(e), .Names = "A")
    design$c = rep("A", length(e))
    design$a = structure(1, .Names = "A")

  } else if(is.list(design) && "N" %in% names(design) &&
            "c" %in% names(design)) {
    # this code segment is for setting up twoStage

    if("n" %in% names(design)) stop("Please only give 'N' and 'c' elements in the design list.")

    if(!all(sort(names(design$N)) == sort(unique(design$c)))) {
      stop("names of design$N should correspond to the unique values in design$c")
    }

    if(any(is.null(design$N))) stop("No elements of design$N can be NULL")

    design$n = structure(as.numeric(table(design$c)), .Names = names(table(design$c)))

    design$a = design$N / design$n
    design$sampling = "twoStage"
  
  # "2017-06-12 09:28:47 PDT" GG added to handle weighted analysis
  } else if(is.list(design) && "targetCategory" %in% names(design) &&
            "cohortCategory" %in% names(design)) {
    # this code segment is for setting up weighted
    # with cohort_category and target_category provided
    weight_0 = weight_fn(cohort_category, target_category)
    weight_code = weight_0$code
    weight_message = weight_0$message
    design$weight = unname(weight_0$weight)
    
    if(weight_code != 0) stop(weight_message)
    
    design$sampling = "target_and_cohort_categories_provided"
  } else {
    stop(paste("Design must be either 'randomSample' or a list of c, N, and n",
               "to designate two stage sampling.", sep = "\n"))
  }
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    

  
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 3: Model checks:
  
  cum = cumsum(design$a[design$c])
  ## ungrouped = FALSE
  ## tStar = NA

  riskGroupErrorTxt = "Model must be a list with one named element: either k, K, cutoffs, or ungrouped."
  
  if(!is.list(riskGroup)) {
    stop(riskGroupErrorTxt)
  }
  if(length(riskGroup) != 1) {
    stop(riskGroupErrorTxt)
  }
  if(is.null(names(riskGroup))) {
    stop(riskGroupErrorTxt)
  }
  if(!names(riskGroup) %in% c("k", "K", "cutoffs", "ungrouped")) {
    stop(riskGroupErrorTxt)
  }
  
  if( ("ungrouped" %in% names(riskGroup)) && ("epsilon" %in% names(riskGroup$ungrouped)) &&
     ((riskGroup$ungrouped$epsilon <= 0) || (riskGroup$ungrouped$epsilon > 1)) )  {
    stop("Please specify epsilon to be (0, 1].")
  }


       
  
  if(names(riskGroup) == "k") {
    ## riskGroup$k is given
    riskGroup$K = max(riskGroup$k)
    riskGroup$cutoffs = NULL
    riskGroup$epsilon = NA
    riskGroup$tStar = NA
    riskGroup$ungrouped = FALSE

  } else if(names(riskGroup) == "K") {

    if(length(riskGroup$K) > 1) stop("riskGroup$K should be a length-1 integer indicating the number of risk groups.")

    riskGroup$k = apply(sapply(seq(0, sum(design$a[design$c]),
      length.out = riskGroup$K + 1)[-1],
      function(tile) cum <= tile), 1, function(row) min(which(row)))
    ## riskGroup$K is given    
    riskGroup$cutoffs = NULL
    riskGroup$epsilon = NA
    riskGroup$tStar = NA
    riskGroup$ungrouped = FALSE
    
  } else if(names(riskGroup) == "cutoffs") { 
    if(!is.numeric(riskGroup$cutoffs) || any(riskGroup$cutoffs < 0) || any(riskGroup$cufoffs > 1) ||
       riskGroup$cutoffs[1] != 0 || riskGroup$cutoffs[length(riskGroup$cutoffs)] != 1 ||
       ! all(riskGroup$cutoffs[2:length(riskGroup$cutoffs)] - riskGroup$cutoffs[1:(length(riskGroup$cutoffs) - 1)] > 0)) {
      stop(paste("riskGroup$cutoffs must be a increasing numeric vector of points, with the first",
                 "value being 0, the last value being 1.  This vector will be used to break",
                 "up the subjects into risk groups based on their predicted risk, r.", sep = "\n"))
    }

    riskGroup$k = as.numeric(cut(r, breaks = riskGroup$cutoffs, include.lowest = TRUE))
    riskGroup$K = length(riskGroup$cutoffs) - 1
    ## riskGroup$cutoffs is given
    riskGroup$epsilon = NA
    riskGroup$tStar = NA
    riskGroup$ungrouped = FALSE
 
  } else { # riskGroup == "ungrouped"

    riskGroup$k = rep(1, length(e))
    riskGroup$K = 1
    riskGroup$cutoffs = NULL
    riskGroup$epsilon = if("epsilon" %in% names(riskGroup$ungrouped)) riskGroup$ungrouped$epsilon else NA
    riskGroup$tStar = if("tStar" %in% names(riskGroup$ungrouped)) riskGroup$ungrouped$tStar else NA
    riskGroup$ungrouped = TRUE
  }

  # Checks that there is at least one significant event (death from disease) in every k group:
  # New Tue Apr 26 15:15:14 PDT 2011 >>>
  # if (!all(tapply(e, riskGroup$k, function(e_k) any(e_k == 1)  ) ) ) 
  #  stop("There must be at least one disease event in every risk group")

  # New Tue May  3 12:17:57 PDT 2011
  offendingRGs = which(! tapply(e, riskGroup$k, function(e_k) any(e_k == 1)  ))
  if(length(offendingRGs > 0)) {
    stop(paste("\nFor the following risk groups: ",
               paste(offendingRGs, collapse = " "), "\n",
               "there were no observations for which e = 1 (disease events) \n",
               "piHat for each of these risk groups is 0, but no \n",
               "confidence intervals can be created.  You may wish to \n",
               "restructure your risk groups so that there is at least \n",
               "one observation with e = 1 for each risk group. "),
         collapse = "")
  }
  # <<<
  
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    

  
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 4: rSummary checks:
  
  if(length(rSummary) == 1 && rSummary == "mean") {

    rSummary = sapply( 1:riskGroup$K, function(kkk) {
      sum( design$a[ design$c[riskGroup$k == kkk] ] * r[riskGroup$k == kkk]) /
        sum( design$a[design$c[riskGroup$k == kkk] ] )
    })

  } else if(length(rSummary) == 1 && rSummary == "median") {

    # FLAG: why does it need to be ordered?
    rSummary = sapply(1:riskGroup$K, function(kkk) {
      www = design$a[design$c[riskGroup$k == kkk] ][order(r[riskGroup$k == kkk])]
      sort(r[riskGroup$k == kkk])[cumsum(www) >= (sum(www) / 2)][1]
    })

  } else if(length(rSummary) == 1 && rSummary == "midpoint") {

    if(! "cutoffs" %in% names(riskGroup)) {
      stop("If rSummary is 'midpoint', riskGroup$cutoffs must be provided.")
    }

    rSummary = (riskGroup$cutoffs[2:length(riskGroup$cutoffs)] + riskGroup$cutoffs[1:(length(riskGroup$cutoffs) - 1)]) / 2
    
  } else if(length(rSummary) == riskGroup$K && is.numeric(rSummary) &&
            all(rSummary > 0) && all(rSummary < 1)) {

    # Don't do anything. rSummary is okay as it is provided.
    
  } else {

    stop(paste("rSummary must either be the string 'mean', 'median', 'midpoint', or ",
               "a numeric vector with one summary value for each risk group.", sep = "\n")) 
  }
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    

  
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 5: bootstrap checks
  if(length(bootstrap) > 1) {
    stop("bootstrap must be length 1. FALSE or an integer.")
  }
  
  if(is.logical(bootstrap) && bootstrap) {
    stop("bootstrap cannot be 1 or TRUE")
  }

  if(is.logical(bootstrap) && !bootstrap) {
    nBootstraps = 0
  } else if(is.numeric(bootstrap) && bootstrap > 1) {
    nBootstraps = floor(bootstrap)
  } else {
    stop("bootstrap must be either FALSE or a positive integer greater than 2.")
  }
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  
  # "2017-06-12 09:28:47 PDT" GG why do they need to be reordered again, weren't they alread done on lines 41-44?
  baseArgs = list(e = e[order(ord)],  ###DJDJ 
                  t = t[order(ord)],  ###DJDJ
                  r = r[order(ord)],  ###DJDJ 
                  cohortCategory = design$cohortCategory[order(ord)], # "2017-06-12 09:28:47 PDT" GG
                  c = design$c[order(ord)],  ###DJDJ 
                  k = riskGroup$k[order(ord)],  ###DJDJ 
                  weight = design$weight, # "2017-06-12 12:06:35 PDT" GG
                  K = riskGroup$K,
                  epsilon = riskGroup$epsilon,
                  ungrouped = riskGroup$ungrouped,
                  N = design$N,
                  n = design$n,
                  rSummary = rSummary,
                  nBootstraps = nBootstraps,
                  multicore = multicore,
                  targetCategory = design$targetCategory, # "2017-06-12 09:28:47 PDT" GG
                  tStar = tStar,
                  verbose = verbose,
                  offendingRGs = offendingRGs
    )

  class(baseArgs) = c("baseArgs", class(baseArgs))

  baseArgs
}

# Wed Aug 17 14:22:40 PDT 2011

# "Sat Aug 31 00:39:21 2013" V0.01-05 Add offendingRGs to baseArgs.

# "Thu Sep 12 07:04:41 2013"
# riskGroup$k = as.numeric(cut(r, breaks = riskGroup$cutoffs, include.lowest = TRUE))

