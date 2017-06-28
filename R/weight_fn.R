weight_fn = function(cohort_category, target_category){
    categories = sort(unique(cohort_category))
    # message
    target_categories = sort(unique(target_category))
    target_not_cohort = setdiff(target_categories, categories)
    target_not_cohort_message = paste( 
      paste0(target_not_cohort, collapse = ", "), 
      "in target categories but not in cohort categories" )
    cohort_not_target = setdiff(categories, target_categories)
    cohort_not_target_message = paste(
      paste0(cohort_not_target, collapse = ", "),
      "in cohort categories but not in target categories" )
    message = if(length(target_not_cohort) > 0 & length(cohort_not_target) > 0){
      list(code = 1, 
           message = paste(target_not_cohort_message, "and", 
                           cohort_not_target_message))
    } else if(length(target_not_cohort) > 0){
      list(code = 2,
           message = target_not_cohort_message)
    } else if(length(cohort_not_target) > 0){
      list(code = 0, # "2017-06-15 15:36:26 PDT" GG changed from code = 3;  allowing weights = 0
           message = cohort_not_target_message)
    } else {
      list(code = 0,
           message = "cohort categories and target categories match")
    }
    # message end
    category_proportions = lapply(list(cohort_category, target_category), function(this){
      category_counts = unlist(sapply(categories, function(category){
        sum(this == category)
      }, simplify = FALSE))
      category_proportions = category_counts/length(this)
    })
    category_weights = category_proportions[[2]] / category_proportions[[1]]
    weight = unname(category_weights[ cohort_category ])
    list(category_weights = category_weights,
         code = message$code,
         message = message$message,
         weight = weight)
}
