## A `score_statistics` example {#ss-example}

library(rmap)
x = read.csv(
  file = "data_set_score_statistics.csv", 
  stringsAsFactors = FALSE)
head(x)

e = x$e
t = x$t
risk_model_1 = list(
  r = x$r_1, 
  Lambda_outcome = x$Lambda_outcome_1,
  Lambda_mortality = x$Lambda_mortality_1,
  groupings = list(
    risk = list(k = x$k_1),
    missing = list(K = 4, variable = x$w)))
risk_model_2 = list(
  r = x$r_2, 
  Lambda_outcome = x$Lambda_outcome_2,
  Lambda_mortality = x$Lambda_mortality_2,
  groupings = list(
    risk = list(K = 4),
    missing = list(K = 4, variable = x$w)))
ss = score_statistics(e, t, 
  risk_model_1 = risk_model_1, 
  risk_model_2 = risk_model_2)
ss

## Adding `standardized_residuals` to our example

sr = standardized_residuals(e, t, 
  risk_model_1 = risk_model_1, 
  risk_model_2 = risk_model_2)
plot(sr, grouping_name = "risk")

### Prettier...

plot_pars = list(x_max = 0.17,
                 y_max = 9,
                 y_max_ad = 0.33,
                 xlab = "Risk (%)")
plot(sr, grouping_name = "risk", plot_pars)

## If you don't have cumulative mortality hazards {#no-mortality-example}

risk_model_2 = list(
  r = x$r_2, 
  Lambda_outcome = x$Lambda_outcome_2,
  Lambda_mortality = NULL,
  groupings = list(
    risk = list(K = 4),
    missing = list(K = 4, variable = x$w)))
ss_no_mortality = score_statistics(e, t, 
  risk_model_1 = risk_model_1, 
  risk_model_2 = risk_model_2)
ss_no_mortality

sr_no_mortality = standardized_residuals(e, t, 
  risk_model_1 = risk_model_1, 
  risk_model_2 = risk_model_2)
plot(sr_no_mortality, grouping_name = "risk")

## Adding `risk_model_boxplots` to our example {#boxplot-example}

list_of_risk_models = list(
  risk_model_1 = x$r_1,
  risk_model_2 = x$r_2)
K = 4
risk_quantile_boxplots(list_of_risk_models, K)

risk_quantile_boxplots(list_of_risk_models, K, risk_max = 0.6)

## A `riskValidate` example {#rv-example}

library(rmap)
x = read.csv(
  file = "datafRandomSample.csv", 
  stringsAsFactors = FALSE)
head(x)

my_e = x$e
my_t = x$t
my_r = x$r
my_design = "randomSample"
my_riskGroup = list(k = x$k)
my_rSummary = "mean"
my_bootstrap = 30

rv = riskValidate(
  e = my_e, t = my_t, r = my_r, design = my_design,
  riskGroup = my_riskGroup, rSummary = my_rSummary,
  bootstrap = my_bootstrap, rvpar = rvparFn())
options(width = 120)
options(digits = 3)
rv

## A complete `riskValidateUngrouped` example {#rvu-example}

set.seed(1)
sampleData = df_randomSample_r1_r2(NTotal = 500)
epsilon = nrow(sampleData)^(-1/3)
tStar = 10

rvu = riskValidateUngrouped(
  e = sampleData$e, t = sampleData$t, r = sampleData$r1,
  design = "randomSample", 
  riskGroup = list(
    ungrouped = list(epsilon = epsilon, tStar = tStar)), 
  bootstrap = 20, rvpar = rvparFn(), 
  multicore = FALSE, verbose = FALSE)
rvu

## How to call `IAD` to show multiple risk models on the same plot {#iad-example}

library(rmap)
set.seed(1)
sampleData = df_randomSample_r1_r2(NTotal = 500)
riskGroup = list(
  ungrouped = list(epsilon = 0.15, tStar = 10))

rvu1b = riskValidateUngrouped(
  e = sampleData$e, t = sampleData$t, r = sampleData$r1,
  design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
  rvpar = FALSE, multicore = FALSE, verbose = FALSE)

rvu2b = riskValidateUngrouped(
  e = sampleData$e, t = sampleData$t, r = sampleData$r2,
  design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
  rvpar = FALSE, multicore = FALSE, verbose = FALSE)

IAD(
  list(rvu1b, rvu2b), 
  rvpar = rvparFn(
    col = c("red", "blue"),
    atX = seq(0, 100, 20), atY = seq(0, 100, 20),
    comment = "20 bootstraps for confidence bands"))

## A `caseRiskPercentiles` example {#crp-example}

data(data_set_score_statistics)
xxx = data_set_score_statistics
tail(xxx)
cutoff = 0.6
tStar = 10
e = xxx$e
t = xxx$t
r = xxx$r_1
rAnother = xxx$r_2
crp = caseRiskPercentiles(
  cutoff, e, t, tStar, r, rAnother, 
  main = "Case risk percentiles",
  xlab = "Risk model 1", ylab = "Risk model 2",
  cex = 1, pch = 16)
crp

F1 = ecdf(crp$crp1)
F2 = ecdf(crp$crp2)
plot(F1, verticals = TRUE, do.points = FALSE, col = "red", 
     xlim = c(0, 1),
     main = "Case Risk Percentiles CDF")
plot(F2, verticals = TRUE, do.points = FALSE, col = "blue", add = TRUE)
legend(x = 0, y = 0.95, legend = c("Risk model 1", "Risk model 2"), 
       col = c("red", "blue"),
       lty = c(1, 1))

crp = caseRiskPercentiles(cutoff, e, t, tStar, r)
crp

## A `performanceDifference` example {#pd-example}

set.seed(1)
twoMod = df_twoStage_r1_r2()
twoMod$N
head(twoMod$d)

pd = performanceDifference(
  e = twoMod$d$e, 
  t = twoMod$d$t,
  rs = list(r1 = twoMod$d$r1, r2 = twoMod$d$r2),
  design = list(c = twoMod$d$c, N = twoMod$N),
  riskGroup = list(K1 = 2, K2 = 2),
  bootstrap = 1000)
pd 

# Composite plots {#composite-plots}

set.seed(1)
sampleData = df_randomSample_r1_r2(NTotal = 200)

rv1 = riskValidate(
  e = sampleData$e, t = sampleData$t, r = sampleData$r1,
  design = "randomSample",
  riskGroup = list(K = 5), rSummary = "mean",
  bootstrap = 20, rvpar = FALSE)

rv2 = riskValidate(
  e = sampleData$e, t = sampleData$t, r = sampleData$r2,
  design = "randomSample",
  riskGroup = list(K = 5), rSummary = "mean",
  bootstrap = 20, rvpar = FALSE)

rvu1 = riskValidateUngrouped(
  e = sampleData$e, t = sampleData$t, r = sampleData$r1,
  design = "randomSample", 
  riskGroup = list(
    ungrouped = list(epsilon = 0.15, tStar = 10)), 
  bootstrap = 20, rvpar = FALSE, 
  multicore = FALSE, verbose = FALSE)

rvu2 = riskValidateUngrouped(
  e = sampleData$e, t = sampleData$t, r = sampleData$r2,
  design = "randomSample", 
  riskGroup = list(
    ungrouped = list(epsilon = 0.15, tStar = 10)), 
  bootstrap = 20, rvpar = FALSE, 
  multicore = FALSE, verbose = FALSE)

setUpTrellisFn(2, 2, 
               main = "Attribute and Individualized Attribute Diagrams")

attributeDiagramRawFn(pos = c(1, 1), rv1, 
                      col = "#FF0000", lightCol = "#FF666640")
addTextToTrellisFn(pos = c(1, 1), "model 1")

attributeDiagramRawFn(pos = c(1, 2), rv2, 
                      col = "#0000FF", lightCol = "#6666FF40")
addTextToTrellisFn(pos = c(1, 2), "model 2")

IAD_RawFn(pos = c(2, 1), rvu1, 
          col = "#FF0000", lightCol = "#FF666640")
addTextToTrellisFn(pos = c(2, 1), "model 1")

IAD_RawFn(pos = c(2, 2), rvu2, 
          col = "#0000FF", lightCol = "#6666FF40")
addTextToTrellisFn(pos = c(2, 2), "model 2")

# Simulation functions {#sim-fns}

set.seed(1)
data1 = df_randomSample()
head(data1)

data2 = df_randomSample_r1_r2()
head(data2)

data3 = df_twoStage()
data3$N

head(data3$d)

data4 = df_twoStage_r1_r2()
data4$N

head(data4$d)

