

riskValidate = function(e, t, r, design = "randomSample", riskGroup, rSummary, bootstrap = FALSE,
                        rvpar = rvparFn()) {
  
  rv = riskValidateExternalFn(e, t, r, design, riskGroup, rSummary, bootstrap)

  force(rvpar)

  ocall = deparse(match.call())

  if("rvpar" %in% class(rvpar)) {
    
    attributeDiagram(rvs = rv, rvpar = rvpar, ocall = ocall)

  } else {
    cat(paste("Note: No plot produced.  If an attribute diagram is desired, ",
                "rvparFn() should be used to set the argument 'rvpar'", sep = "\n"))
    cat("\n")
  }

  rv
}

# Wed Aug 24 11:33:44 PDT 2011
