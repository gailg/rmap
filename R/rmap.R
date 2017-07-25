rmap = function(e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
                confidence_level = 0.95, multicore = FALSE, verbose = FALSE){
  tStar = t_star
  riskGroup = risk_group
  rSummary = r_summary
  bootstrap = if(N_bootstraps == 0){
    FALSE
  } else {
    N_bootstraps
  }
  confidenceLevel = confidence_level
  multicore = if(N_cores > 1){
    TRUE
  } else {
    FALSE
  }
  design = if(is.character(design) && design == "random_sample"){
    "randomSample"
  } else if("N_first_stage" %in% names(design) && "category" %in% names(design)){
    list(c = design$category,
         N_two_stage = design$N_first_stage)
  } else if("N_target" %in% names(design) && "category" %in% names(design)){
    list(c = design$category,
         N_target = design$N_target)
  } else if ("target_category" %in% names(design) && "category" %in% names(design)){
    list(c = design$category,
         targetCategory = design$target_category)
  } else {
    stop(paste("Design must be 'randomSample' or a list containing 'category'",
               "and one of 'N_first_stage', 'N_target', or 'target_category'"))
  }
  baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, 
                        confidenceLevel, multicore, verbose)
  if( baseArgs$ungrouped ) {
    plots = rmap_ungrouped_fn(baseArgs)
    summary = NULL
    list(plots = plots, summary = summary)
  } else {
    rmap_grouped_fn(baseArgs)
  }
}