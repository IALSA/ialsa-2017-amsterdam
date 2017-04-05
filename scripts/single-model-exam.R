library(msm)
# load list with multiple models (e.g. each element = differnt covariates)
# models <- readRDS("./data/shared/derived/models/version-3/models_A.rds")
models <- readRDS("./data/shared/derived/models/option-1/models.rds")
names(models)
names(models$msm)

# lapply(models, names) # must be a list, with elements = msm model objects

# load individual models, turn off lines that aren't used
# model <- models$age
# model <- models$age_bl
# model <- models$male
# model <- models$educat
# model <- models$edu
# model <- readRDS("./data/shared/derived/models/version-3/mB1.rds")
# model <- readRDS("./data/shared/derived/models/version-3/mA2.rds")
model <- readRDS("./data/shared/derived/models/option-1/mA4.rds")

# Demonstrate the extraction functions

# intensity matrix
qmatrix.msm(model)
# qmatrix.msm(model, covariates = list(male = 0))
# transition probability matrix
pmatrix.msm(model, t = 10) # t = time, in original metric
# mean sojourn times
sojourn.msm(model)
# probability that each state is next
pnext.msm(model)
# total length of stay
totlos.msm(model)
# expected number of visits to the state
envisits.msm(model)
# ratio of transition intensities
qratio.msm(model,ind1 = c(2,1), ind2 = c(1,2))
# Hazard ratios for transtion
hazard.msm(model)


# ( Alternatively just type "print(model)" )
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p    <- model$opt$par
p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
print(cbind(p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
      quote=FALSE)


# ---- LE-estimation ----------------------------
LE <- elect(
  model          = model,    # fitted msm model
  b.covariates   = covar_list,   # list with specified covarites values
  statedistdata  = ds_alive,     # data for distribution of living states
  time.scale.msm = time_scale,   # time scale in multi-state model ("years", ...)
  h              = grid_par,     # grid parameter for integration
  age.max        = age_max,      # assumed maximum age in years
  S              = replication_n # number of simulation cycles
)



# ----- examine-LE-object -----------------
model <- models$age


summary.elect(
  model$LE, # life expectancy estimated by elect()
  probs = c(.025, .5, .975), # numeric vector of probabilities for quantiles
  digits=2, # number of decimals places in output
  print = TRUE # print toggle
)

plot.elect(
  model$LE, # life expectancy estimated by elect()
  kernel = "gaussian", #character string for smoothing kernal ("gaussian",...)
  col = "red", # color of the curve
  lwd = 2, # line width of the curve
  cex.lab = 1 # magnification to be used for axis-labels
)
