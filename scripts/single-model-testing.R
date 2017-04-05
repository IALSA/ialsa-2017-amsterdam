# ---- load-packages -------------
library(msm)
base::source("http://www.ucl.ac.uk/~ucakadl/ELECT/ELECT.r") # load  ELECT functions

# ---- load-data ------------------
ds_clean <- readRDS("./data/unshared/ds_clean.rds")
head(ds_clean)
# load object "model_specification"
base::source("https://raw.githubusercontent.com/IALSA/ialsa-2016-amsterdam-public/master/model-specification-v3.R")
lapply(model_specification,names)
model_specification$A

# ---- prepare-for-estimation --------------------
(N <- length(unique(ds_clean$id)))
subjects <- as.numeric(unique(ds_clean$id))
# Add first observation indicator
# this creates a new dummy variable "firstobs" with 1 for the first wave
cat("\nFirst observation indicator is added.\n")
offset <- rep(NA,N)
for(i in 1:N){offset[i] <- min(which(ds_clean$id==subjects[i]))}
firstobs <- rep(0,nrow(ds_clean))
firstobs[offset] <- 1
ds_clean <- cbind(ds_clean ,firstobs=firstobs)
head(ds_clean)

# list ids with intermidiate missing states (ims), right censors (rs), or with both
ids_with_ims  <- unique(ds_clean[ds_clean$state == -1, "id"]); length(ids_with_ims)
ids_with_rs   <- unique(ds_clean[ds_clean$state == -2, "id"]); length(ids_with_rs)
ids_with_both <- unique(ds_clean[ds_clean$state == -2 | ds_clean$state == -1, "id"]); length(ids_with_both)

# subset a random sample of individuals if needed
set.seed(42)
ids <- sample(unique(ds_clean$id), 100)
ds <- ds_clean %>% 
  # dplyr::filter(id %in% ids) %>% # make sample smaller if needed 
  # exclude individuals with some missing states
  # dplyr::filter(!id %in% ids_with_ims) %>%
  # dplyr::filter(!id %in% ids_with_rs) %>%
  # dplyr::filter(!id %in% ids_with_both) %>%
  dplyr::mutate(
    male = as.numeric(male), 
    # age      = age,
    # age_bl   = age_bl
    # age    = (age - 80)/20,
    # age_bl = (age_bl - 80)/20
    age    = (age - 75),
    age_bl = (age_bl - 75)
    # age    = age/20,
    # age_bl = age_bl/20
  )
# view data object to be passed to the estimation call
head(ds)
ds %>% dplyr::summarise(unique_ids = n_distinct(id)) # subject count
sf <- ds %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(count = n()) %>%  # basic frequiencies
  dplyr::mutate(pct = round(count/sum(count),2)) %>%  # percentages, use for starter values
  print()
cat("\nState table:"); print(msm::statetable.msm(state,id,data=ds)) # transition frequencies
# these will be passed as starting values
(initial_probabilities <- as.numeric(as.data.frame(sf[!sf$state %in% c(-1,-2),"pct"])$pct))
# save the object to be used during estimation
saveRDS(ds, "./data/unshared/ds_estimation.rds")
# ---- msm-options -------------------
# set estimation options 
digits = 2
cov_names  = "age + age_bl +  male + educat"   # string with covariate names
method_    = "BFGS"  # alternatively, if does not converge "Nedler-Mead" or "BFGS", “CG”, “L-BFGS-B”, “SANN”, “Brent”
constraint_ = NULL    # additional model constraints
fixedpars_  = NULL       # fixed parameters
initprobs_ = initial_probabilities 

covariates_ <- as.formula(paste0("~",cov_names)) # construct covariate list

q <- .01
# ---- model-A --------------
# transition matrix
Q <- rbind( c(0, q, 0, q), 
            c(q, 0, q, q),
            c(0, 0, 0, q), 
            c(0, 0, 0, 0)) 
# misclassification matrix
E <- rbind( c( 0,  0,  0, 0),  
            c( 0,  0, .1, 0), 
            c( 0,  0,  0, 0),
            c( 0,  0,  0, 0) )
# transition names
qnames = c(
  "Healthy - Mild",  # q12
  # "Healthy - Severe", # q13
  "Healthy - Dead",  # q14
  "Mild - Healthy",  # q21  
  "Mild - Severe",   # q23
  "Mild - Dead",     # q24
  # "Severe - Healthy",# q31
  # "Severe - Mild",   # q32
  "Severe - Dead"    # q34
)
# ---- specification-via-common-object --------------
# (Q      <- model_specification[["A"]][["Q"]])
# (E      <- model_specification[["A"]][["E"]])
# (qnames <- model_specification[["A"]][["qnames"]])

# ---- msm-estimation --------------------------
# estimate model
model <- msm(
  formula       = state ~ age 
  ,subject       = id 
  ,data          = ds 
  ,center        = FALSE
  ,qmatrix       = Q 
  ,ematrix       = E
  ,death         = TRUE 
  ,covariates    = covariates_
  ,censor        = c(-1,-2)
  ,censor.states = list(c(1,2,3), c(1,2,3))
  ,method        = method_
  ,constraint    = constraint_
  ,fixedpars     = fixedpars_
  ,initprobs     = initprobs_# c(.67,.16,.11,.07), # initprobs_
  , est.initprobs = TRUE
  # , obstrue       = firstobs
  ,control       = list(trace=0,REPORT=1,maxit=1000,fnscale=10000)
)
 
# examine multistate object
print(model)
# ( Alternatively just type "print(model)" )
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p    <- model$opt$par
p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
print(cbind(p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
      quote=FALSE)

# ---- LE-options ---------------------------
alive_states <- c(1,2,3)
ds_alive <- ds[ds$state %in% alive_states,]

age_min <- 0
age_max <- 50 
age_bl <- 0
male <- 0
edu <- 0

replication_n <- 50 # keep low (50-100) during testing stage
time_scale <- "years"
grid_par <- .5

covar_list <- list(age=age_min)

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
summary.elect(
  LE, # life expectancy estimated by elect()
  probs = c(.025, .5, .975), # numeric vector of probabilities for quantiles
  digits=2, # number of decimals places in output
  print = TRUE # print toggle
)

plot.elect(
  LE, # life expectancy estimated by elect()
  kernel = "gaussian", #character string for smoothing kernal ("gaussian",...)
  col = "red", # color of the curve
  lwd = 2, # line width of the curve
  cex.lab = 1 # magnification to be used for axis-labels
)




