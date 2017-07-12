#' \code{baseArgsFn}
#' 
#' This function translates arguments given by the user into a data structure 
#' that is used internally throughout the \code{rmap} package.
#' 
#' @param e A vector of events for each subject in the dataset.  
#' The event for one subject can either be a 
#' \code{0} (censored), \code{1} (disease) or \code{2} (death from other causes).
#' 
#' @param t A vector of times until event for each subject in the dataset.  
#' This is the time that the event \code{e} occurred.  
#' This time must fall in the interval \code{[0, tStar)}, where \code{tStar} is 
#' the duration of the study. 
#' @param r A vector of risk values for each subject in the dataset.  
#' This is the probability of disease, as predicted by a risk model.  
#' The goal of the \code{rmap} package is to assess the validity of and
#' calibration of this model.
#' @param tStar A positive number equal to the duration of the the study.
#' @param design One of the following choices
#' \itemize{
#' \item{\code{"randomSample"}: }{Using \code{design = "randomSample"}
#'   signals to \code{rmap} that you obtained your data using a
#'   random sample.
#' }
#' \item{\code{list(c = c, N_two_stage = N_two_stage)}: }{If your
#'   design is a two-stage sample, name your two-stage sampling
#'   categories with capital letters beginning with \code{"A"},
#'   \code{"B"}, etc., let \code{c} be the vector of each subjects's
#'   sampling category, and \code{N_two_stage} be a named vector
#'   counting the number
#'   of subjects from the first stage that fell into each category.
#'   (For example, if the two categories were \code{"A"} and \code{"B"}, 
#'   and there were \code{13} in the first stage who were categorized
#'   to \code{"A"} and \code{42} to \code{"B"}, then define
#'   \code{N_two_stage = c(A = 13, B = 42)}; you would of course have
#'   much larger numbers than \code{13} and \code{42}).
#'   Specify \code{design = list(c = c, N_two_stage = N_two_stage)}.
#' }
#' \item{\code{list(c = c, target_category = target_category)}: }{ If your
#'   cohort sample does not match your target population and you have 
#'   a sample from your target population and both samples contain 
#'   relevant categories, label the categories beginning with \code{"A"},
#'   \code{"B"}, etc, and let \code{c} be the vector of cohort subjects'
#'   categories and \code{target_category} be the vector of target
#'   subjects' categories, and specify 
#'   \code{design = list(c = c, target_category = target_category)}.
#' }
#' }
#' @param riskGroup Describes the way you want the subjects to be divided
#' into risk groups. One of the following named lists:
#' \itemize{
#' \item{\code{list(K = k)}: }{You can manually assign risk groups using 
#' an integer vector \code{k} containing risk group assignments for each 
#' subject in the dataset.  Define \code{K} to be the total number of 
#' risk groups.  Then your vector \code{k} is a risk group assignment  
#' in \code{\{1, ..., K\}} for each subject in the dataset.  
#' Subjects with risk group assignment \code{= 1} should have the smallest 
#' assigned risks (\code{r}), and subjects with the risk group assignment 
#' \code{= K} should have the highest assigned risks.
#' }
#' \item{\code{list(K = K)}: }{Define \code{K} to be a positive integer
#' and let \code{rmap} divide your sample into \code{K} quantiles.  For 
#' example, if \code{K = 4}, your risk groups will consist of \code{4}
#' quartiles, the first group, \code{k = 1}, containing the subjects
#' with the smallest risks \code{r}, the fourth group, \code{k = 4}, 
#' containing the subjects with the largest risks.
#' }
#' \item{\code{list(cutoffs = cutoffs)}: }{Risk groups can also be 
#' assigned to each subject in the dataset based on risk "cutoffs". 
#' If you wished to create three risk groups, say all subjects having 
#' an assigned risk between 0 and 0.2 to be in risk group 1, 
#' all subjects with assigned risk between 0.2 and 0.5 to be in risk 
#' group 2, and all subjects with assigned risk between 0.5 and 1 
#' to be in risk group 3, define \code{cutoffs = c(0, 0.2, 0.5, 1)}.
#' }
#' \item{\code{list(epsilon = epsilon)}: }{The previous three 
#' ways that the riskGroup argument was specified were ways to 
#' group subjects for a grouped analysis. We can also validate 
#' risk models using an ungrouped analysis. The calculations 
#' require a small positive number, epsilon, specifying
#' the  kernel neighborhoods used to calculate observed risk
#' at each distinct assigned risk. Asymptotic theory suggests
#' good behavior for \code{epsilon = N^(-1/3)}.
#' }
#' }
#' @param rSummary A summary statistic for summarizing the 
#' assigned risks for all subjects in a risk group.  The \code{rSummary} 
#' can be calculated by the program or you may wish to provide one. 
#' If you would like an ungrouped analysis 
#' (\code{riskGroup = list(epsilon = epsilon)}), then 
#' \code{rmap} will automatically use "mean" to summarize the 
#' risk in each epsilon neighborhood.
#' There are four options in specifying the rSummary: 
#' \itemize{
#' \item{\code{"mean"}: } {Summarize each risk group with
#' code{mean(r)} over all values of \code{r} in that risk group.
#' }
#' \item{\code{"median"}: } {Summarize each risk group with
#' code{median(r)} over all values of \code{r} in that risk group.
#' }
#' \item{\code{"midpoint"}: } {If 
#' \code{riskGroup = list(cutoffs = c(0, 0.2, 0.5, 1))}, then the 
#' \code{rSummary} would be assigned \code{c(0.1, 0.35, 0.75)}.
#' }
#' \item{A user supplied summary vector } {You may manually specify
#' your own values for \code{rSummary}.
#' To use this option, define \code{rSummary} to be a 
#' numeric vector of length \code{K} specifying your summary statistic
#' for each risk group.
#' }
#' }
#' @param bootstrap Either a single integer number, 
#' describing the number of bootstraps or \code{FALSE} 
#' to turn off bootstrapping.
#' @param multicore Currently not used.
#' A logical value. If multicore = TRUE and
#' (\code{riskGroup = list(epsilon = epsilon)}) and bootstrap 
#' is not set to FALSE), multiple processors are used to 
#' compute the bootstraps.
#' @param verbose Currently not used.
#' If verbose = TRUE and
#' (\code{riskGroup = list(epsilon = epsilon)}) and bootstrap 
#' is not set to FALSE), progress of the calculation will be 
#' output during the calculations of the nearest neighbor and 
#' the bootstrap nearest neighbor.  Such output is useful for 
#' checking the progress of these calculations which can require 
#' a long time depending on the number of observations and the number
#' of distinct assigned risks.
#' 
#' @return A list containing
#' \itemize{
#' \item{\code{c}: }{Equal to the input \code{c} if two-stage sampling
#'   or weighted sampling, and equal to a vector of all \code{"A"}s if
#'   random sampling. A character vector of length \code{N}. 
#' }
#' \item{\code{category_weights}: }{Equal to \code{NULL} 
#'   for random sampling and two-stage sampling. For weighted sampling, 
#'   a named vector (analogous to 
#'   \code{N_two_stage / n_two_stage}) with one element for each
#'   sampling category counting the number of subjects in the 
#'   \code{target_category / c}. (A better name for \code{c}
#'   would be \code{cohort_category} when in the universe of 
#'   weighted sampling.)
#' }
#' \item{\code{cohort_category}: }{The same as \code{c} and will be removed shortly.
#' }
#' \item{\code{confidence_level}: }{Equal to the input 
#'   \code{confidenceLevel}.
#' }
#' \item{\code{e}: }{Equal to the input \code{e}.
#' }
#' \item{\code{epsilon}: }{Equal to the input \code{epsilon} 
#' if \code{riskGroup = list(epsilon = epsilon)}.  Otherwise equal to \code{NA}
#' }
#' \item{\code{k}: }{A vector of length \code{N} indicating the
#' risk group membership of each subject.  
#' If \code{ungrouped = TRUE}, this is a vector of all \code{1}'s.
#' If \code{ungrouped = FALSE}, this vector was either specified
#' by the user (\code{riskGroup = list{k = k}}) or calculated
#' by \code{baseArgsFn}. 
#' Each element in this vector is in \code{\{1, ..., K\}}.
#' }
#' \item{\code{K}: }{A positive integer equal to the number
#' of risk groups.  If \code{ungrouped = TRUE}, this is equal to \code{1}.
#' }
#' \item{\code{multicore}: }{A logical value equal to the input 
#' \code{multicore}
#' }
#' \item{\code{nBootstraps}: }{Equal to \code{N_bootstraps}.
#' }
#' \item{\code{N_bootstraps}: }{An integer describing the 
#' number of bootstraps, where \code{0} indicates no bootstraps.
#' }
#' \item{\code{N_two_stage}: }{A named integer vector with one element
#'   for each sampling category counting the number of subjects in the
#'   first stage of a two-stage sampling design.  For
#'   random sampling, (\code{design = "randomSample"}), 
#'   \code{N_two_stage = N}. For weighted design
#'   (\code{design = list(c = c, target_category = target_category)}),
#'   this variable is not used and
#'   \code{N_two_stage = NULL}.
#' }
#' \item{\code{n_two_stage}: }{A named integer vector with one element
#'   for each sampling category counting the number of subjects in the
#'   second stage of a two-stage sampling design.  For
#'   random sampling, (\code{design = "randomSample"}), 
#'   \code{N_two_stage = N}. For weighted design
#'   (\code{design = list(c = c, target_category = target_category)}),
#'   this variable is not used and
#'   \code{N_two_stage = NULL}.
#' }
#' \item{\code{N_nonzero_events}: }{An integer vector of 
#'   length \code{K} counting the number events in each
#'   risk group that occurred before \code{tStar}
#'   and were not censored.
#' }
#' \item{\code{r}: }{Equal to the input \code{r}.
#' }
#' \item{\code{rSummary}: }{If \code{ungrouped = FALSE}, 
#' \code{rSummary} is a vector of risk summaries, one for 
#'   each risk group.  This vector can be given by the 
#'   user or calculated by \code{baseArgsFn} using 
#'   method \code{"mean"}, \code{"median"}, or 
#'   \code{"midpoint"}.  If \code{ungrouped = TRUE}, 
#'   \code{rSummary} is the forced to be the mean of 
#'   the code{r} values in each epsilon 
#'   neighborhood.
#' }
#' \item{\code{sampling}: }{A string equal to one of
#'   \code{"randomSample"}, \code{"twoStage"},
#'   \code{"weighted"}
#' }
#' \item{\code{t}: }{Equal to the input \code{t}.
#' }
#' \item{\code{tStar}: }{Equal to the input \code{tStar}.
#' }
#' \item{\code{target_category}: }{If
#'   \code{sampling = "weighted"}, the same as
#'   \code{target_category} in the input
#'   \code{design = list(c = c, target_category = target_category)}.
#'   For \code{sampling = "randomSampling"} or
#'   \code{sampling = "twoStage"}, \code{target_category = NULL}.
#' }
#' \item{\code{ungrouped}: }{Equal to \code{TRUE} if
#'   \code{riskGroup = list(epsilon = epsilon)} was chosen.
#' }
#' \item{\code{verbose}: }{A logical equal to the input
#'   \code{verbose}.
#' }
#' \item{\code{weight}: }{A vector of length \code{N} equal
#'   to the weight to assign to each subject.  For 
#'   \code{sampling = "randomSampling"}, \code{weight = 1}.
#'   For \code{sampling = "twoStage"}, \code{weight} is equal to
#'   \code{aaa} in the formulas (\code{N_two_stage/n_two_stage}).
#'   For \code{sampling = "weighted"}, \code{weight} is determined by 
#'   \code{c} and \code{category_weights}.
#' }
#' }
#' @examples
#' ################################################# randomSample grouped
#' options(digits = 3)
#' set.seed(1)
#' randomSample = df_randomSample(100)
#' xxx = randomSample
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' design = "randomSample"
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 20
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' baseArgs
#' ################################################# randomSample grouped
#' options(digits = 3)
#' set.seed(1)
#' randomSample = df_randomSample(100)
#' xxx = randomSample
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' design = "randomSample"
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 20
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' baseArgs
#' ################################################# twoStage grouped
#' set.seed(1)
#' twoStageSample = df_twoStage(200)
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' baseArgs
#' ################################################# twoStage ungrouped
#' set.seed(1)
#' twoStageSample = df_twoStage(200)
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' baseArgs
#' ################################################# weighted grouped
#' set.seed(1)
#' NNN = 200
#' N_bootstrap_reps = 100
#' cutoffs = c(0, 0.20, 1)
#' weighted_example = weighted_example_fn(NNN)
#' cohort_sampling_probability_dictionary = weighted_example$cohort_sampling_probability_dictionary
#' cohort_sample = weighted_example$cohort_sample
#' target_sample = weighted_example$target_sample
#' tStar = weighted_example$t_star
#' which_model = "r_B" 
#' cohort_category = cohort_sample$category
#' target_category = target_sample$category
#' r = cohort_sample[[which_model]]
#' e = cohort_sample$eee
#' t = cohort_sample$ttt
#' design = list(targetCategory = target_category, c = cohort_category)
#' riskGroup = list(cutoffs = cutoffs)
#' rSummary = "mean"
#' bootstrap = N_bootstrap_reps
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap) 
#' baseArgs
#' @export


baseArgsFn = function(
  e, t, r, tStar, design, riskGroup, rSummary, bootstrap, confidenceLevel = 0.95, 
  multicore = FALSE, verbose = FALSE) {

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
  if( "w" %in% names(design)) vecs[[6]] = design$w
  names(vecs) = c("e", "t", "r", "c", "k", "w")

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
  if( "w" %in% names(design)) design$w = vecs[["w"]]
  rm(vecs)

  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 2: Design checks:
  
  if(length(design) == 1 && is.character(design) && design == "randomSample") {
    # this code segment is for setting up oneStage

    design = list(sampling = "oneStage")
    design$N_two_stage = structure(length(e), .Names = "A")
    design$n = structure(length(e), .Names = "A")
    design$c = rep("A", length(e))
    design$a = structure(1, .Names = "A")
    design$weight = rep(1, length(e))
    design$sampling = "randomSample"

  } else if(is.list(design) && "N_two_stage" %in% names(design) &&
            "c" %in% names(design)) {
    # this code segment is for setting up twoStage

    if("n" %in% names(design)) stop("Please only give 'N_two_stage' and 'c' elements in the design list.")

    if(!all(sort(names(design$N_two_stage)) == sort(unique(design$c)))) {
      stop("names of design$N_two_stage should correspond to the unique values in design$c")
    }

    if(any(is.null(design$N_two_stage))) stop("No elements of design$N_two_stage can be NULL")

    design$n = structure(as.numeric(table(design$c)), .Names = names(table(design$c)))

    design$a = design$N_two_stage / design$n
    design$weight = design$a[ design$c ]
    design$sampling = "twoStage"
  
  # "2017-06-12 09:28:47 PDT" GG added to handle weighted analysis
  } else if(is.list(design) && "targetCategory" %in% names(design) &&
            "c" %in% names(design)) {
    # this code segment is for setting up weighted
    # with target_category and c provided
    weight_0 = weight_fn(design$c, target_category)
    weight_code = weight_0$code
    weight_message = weight_0$message
    if(weight_code != 0) stop(weight_message)
    
    design$category_weights = weight_0$category_weights # "2017-06-12 14:07:48 PDT" GG 
    # When the design is twoStage, a contains the weights
    # When the design is target_category and c provided, weight_fn gives us category_weights
    # which are analogous to design$a for twoStage
    design$a = design$category_weights[design$c]
    design$weight = unname(design$category_weights[design$c]) # "2017-06-13 08:16:16 PDT" GG
    # In the twoSample version, the weights get calculated twice 
    # once in gammaHatFn and again in lambdaHatFn
    # For weighted, I  do it just once, here 
    design$a = design$category_weights

    design$sampling = "weighted"
  #   
  # } else if( is.list(design) && "w" %in% names(design) ) {
  #   design$sampling = "w_provided" # design$weight needs to be brought into the vecs
  #   design$weight = design$w
  } else {
    stop(paste("Design must be either 'randomSample' or a list of c and N_two_stage",
               "to designate two stage sampling.", sep = "\n"))
  }
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 3: Model checks:
  
  cum = cumsum(design$a[design$c])

  riskGroupErrorTxt = "Model must be a list with one named element: either k, K, cutoffs, or epsilon."
  
  if(!is.list(riskGroup)) {
    stop(riskGroupErrorTxt)
  }
  if(length(riskGroup) != 1) {
    stop(riskGroupErrorTxt)
  }
  if(is.null(names(riskGroup))) {
    stop(riskGroupErrorTxt)
  }
  if(!names(riskGroup) %in% c("k", "K", "cutoffs", "epsilon")) {
    stop(riskGroupErrorTxt)
  }
  
  if( "epsilon" %in% names(riskGroup)  &&
     ((riskGroup$epsilon <= 0) || (riskGroup$epsilon > 1)) )  {
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
 
  } else { # names(riskGroup) == "epsilon"

    riskGroup$k = rep(1, length(e))
    riskGroup$K = 1
    riskGroup$cutoffs = NULL
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
  ##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ## Section 4: rSummary checks:
  if(riskGroup$ungrouped) rSummary = "mean"
  if(length(rSummary) == 1 && rSummary == "mean") {

    rSummary = sapply( 1:riskGroup$K, function(kkk) {
      sum( design$weight[riskGroup$k == kkk] * r[riskGroup$k == kkk]) /
        sum( design$weight[riskGroup$k == kkk] )
    })

  } else if(length(rSummary) == 1 && rSummary == "median") {

    # FLAG: why does it need to be ordered?
    rSummary = sapply(1:riskGroup$K, function(kkk) {
      www = design$weight[riskGroup$k == kkk][order(r[riskGroup$k == kkk])]
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

  N_in_risk_group = unlist(lapply(seq(1, riskGroup$K, by = 1), function(kkk){
    sum(riskGroup$k == kkk)
  }))
  N_in_risk_group_message = paste(
    "risk groups", 
    paste(which(N_in_risk_group == 0), collapse = ", "),
    "are empty")
  N_nonzero_events = unlist(lapply(seq(1, riskGroup$K, by = 1), function(kkk){
    e_inside_pi_hat = ifelse(t[riskGroup$k == kkk] > tStar, 0, e[riskGroup$k == kkk])
    sum(e_inside_pi_hat > 0)
  }))
  
  error_code = if(design$sampling != "target_and_cohort_categories_provided"){
    NULL
  } else if( weight_code == 0 && all(N_in_risk_group > 0) ){
    0
  } else {
    1
  }
  error_message = if(design$sampling != "target_and_cohort_categories_provided"){
    NULL
  } else if( weight_code != 0 ){
    weight_message
  } else {
    paste("weight ok but", N_in_risk_group_message)
  }
  
  # "2017-06-12 09:28:47 PDT" GG why do they need to be reordered again, weren't they alread done on lines 41-44?
  baseArgs = list(
    c = design$c[order(ord)],  
    category_weights = design$category_weights,
    confidence_level = confidenceLevel,
    e = e[order(ord)], 
    epsilon = riskGroup$epsilon,
    K = riskGroup$K,
    k = unname(riskGroup$k[order(ord)]),  
    multicore = multicore,
    N_bootstraps = nBootstraps,
    nBootstraps = nBootstraps,
    N_nonzero_events = N_nonzero_events,
    N_two_stage = design$N_two_stage,
    n_two_stage = design$n,
    r = r[order(ord)], 
    rSummary = rSummary,
    sampling = design$sampling,
    t = t[order(ord)],  
    target_category = design$targetCategory, 
    tStar = tStar,
    ungrouped = riskGroup$ungrouped,
    verbose = verbose,
    weight = unname(design$weight[order(ord)])
    )

  class(baseArgs) = c("baseArgs", class(baseArgs))

  baseArgs
}

# Wed Aug 17 14:22:40 PDT 2011

# "Sat Aug 31 00:39:21 2013" V0.01-05 Add offendingRGs to baseArgs.

# "Thu Sep 12 07:04:41 2013"
# riskGroup$k = as.numeric(cut(r, breaks = riskGroup$cutoffs, include.lowest = TRUE))

