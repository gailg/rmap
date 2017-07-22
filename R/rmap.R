rmap = function(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, 
                confidenceLevel = 0.95, multicore = FALSE, verbose = FALSE){
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