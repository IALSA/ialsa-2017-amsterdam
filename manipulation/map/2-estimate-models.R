#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-packages -----------------------------------------------------------
library(magrittr)                         # Pipes
library(msm)                              # multi-state modeling
requireNamespace("ggplot2", quietly=TRUE) # graphing
requireNamespace("dplyr", quietly=TRUE)   # data manipulation
requireNamespace("testit", quietly=TRUE)  # condition testing

# ---- load-sources ------------------------------------------------------------
# base::source("http://www.ucl.ac.uk/~ucakadl/ELECT/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT-utility-functions.R") # ELECT utility functions

# ---- declare-globals ---------------------------------------------------------
path_folder <- "./data/shared/derived/models/model-b/"
digits = 2
cat("\n Objected with fitted models will be saved in the folder : \n")
cat("`",path_folder,"`")

# ---- load-data ---------------------------------------------------------------
# first, the script `0-ellis-island.R` imports and cleans the raw data
# second, the script `1-encode-multistate.R` augments the data with multi-states
# load this data transfer object (dto)
dto <- readRDS("./data/unshared/derived/dto.rds")
names(dto)
names(dto$ms_mmse) 
ds_miss <- dto$ms_mmse$missing # data after encoding missing states (-1, -2)
ds_ms <- dto$ms_mmse$multi     # data after encoding multistates (1,2,3,4)

# ---- inspect-data -------------------------------------------------------------
names(dto)
names(dto[["unitData"]])       # 1st element - unit(person) level data
names(dto[["metaData"]])       # 2nd element - meta data, info about variables
names(dto[["ms_mmse"]])        # 3rd element - data for MMSE outcome
ds_miss <- dto$ms_mmse$missing # data after encoding missing states (-1, -2)
ds_ms <- dto$ms_mmse$multi     # data after encoding multistates (1,2,3,4)

###############################################
##  Part I: data preparation                ##
###############################################

# ---- inspect-created-multistates ----------------------------------
# compare before and after ms encoding
view_id <- function(ds1,ds2,id){
  cat("Before ms encoding:","\n")
  print(ds1[ds1$id==id,])
  cat("\nAfter ms encoding","\n")
  print(ds2[ds2$id==id,])
}
ids <- sample(unique(ds_miss$id),1) # view a random person for sporadic inspections
# 50402431 , 37125649, 50101073, 6804844, 83001827 , 56751351, 13485298, 56751351, 75507759)
ids <- c(50402431) #96351191

var_miss <- c("id", "fu_year","age_at_visit","age_death", "mmse" )
var_ms   <- c("id", "fu_year","age", "mmse","state" )
cat("\n Demonstrate the mechanics of encoding states: \n")
view_id(  
  ds_miss %>% dplyr::select_(.dots=var_miss), 
  ds_ms%>% dplyr::select_(.dots=var_ms),  
  ids) 

# ---- remove-invalid-cases --------------------------------------------------------------
####### 1) Remove observations with missing age
# Initial number of observations with missing age : 
sum(is.na(ds_ms$age))
ds_clean <- ds_ms %>% 
  dplyr::filter(!is.na(age))
# Resultant number of observations with missing age
sum(is.na(ds_clean$age))


####### 2) Remove subjects with only ONE observed data point
# Initial number of subjects who have *n* observed data points
ds_clean %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::group_by(n_data_points) %>% 
  dplyr::summarize(n_people=n()) %>% 
  print()
# Determine which ids have only a single observation
remove_ids <- ds_clean %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::arrange(n_data_points) %>% 
  dplyr::filter(n_data_points==1) %>% 
  dplyr::select(id)
remove_ids <- remove_ids$id
# How many subjects to be removed from the data set: 
length(remove_ids)
ds_clean <- ds_clean %>% 
  dplyr::filter(!(id %in% remove_ids))
# Resultant number of subjects who have *n* observed data points
ds_clean %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarize(n_data_points = n()) %>% 
  dplyr::group_by(n_data_points) %>% 
  dplyr::summarize(n_people=n()) %>% 
  print()


####### 3) Remove subjects with IMS at the first observation
# Initial view of subjects with intermediate missing state at first observation:
ids_firstobs_ims <- ds_clean %>% 
  dplyr::filter(firstobs == TRUE & state == -1) %>% 
  dplyr::select(id) %>% print()
ids_firstobs_ims <- ids_firstobs_ims[,"id"]
# Establish the focal dataset
ds_clean <- ds_clean %>% 
  dplyr::filter(!id %in% ids_firstobs_ims)
# Resultant view of subjects with intermediate missing state at first observation:
ds_clean %>% 
  dplyr::filter(firstobs == TRUE & state == -1) %>% 
  dplyr::select(id) %>% print()

# ---- split-income-1 ------------------------
# Initial frequencies
ds_clean %>% 
  dplyr::group_by(income_40) %>%
  dplyr::summarize(n=n()) %>% print(n=50)
# Defining categorical variable
ds_clean$sescat <- car::Recode(ds_clean$income_40,"
                                   1:3  = '-1'; 
                                   4:7  =  '0';
                                   8:10 =  '1';
                                  ")
table(ds_clean$sescat, useNA = "always")

# ---- split-income-2 ------------------------
ds_clean$sescatF <- factor(
  ds_clean$sescat, 
  levels = c( -1,      0,       1), 
  labels = c("Low", "Medium", "High"))
cat("\n How income at age 40 was categorized: \n")
ds_clean %>% 
  dplyr::group_by(sescatF, sescat) %>% 
  dplyr::summarize(n_data_points= n()) %>% 
  as.data.frame() %>% 
  print(nrow=100)
cat("\n Create dummy variables for testing effects of income: \n")

# ref  D1  D2
#  -1  0   0
#  0   1   0
#  1   0   1
ds_clean <- ds_clean %>% 
  dplyr::mutate(
    ses_low_med  = ifelse(sescat == 0, 1, 0 ), 
    ses_low_high = ifelse(sescat == 1, 1, 0 ) 
  )
table(ds_clean$sescat, ds_clean$ses_low_med)
table(ds_clean$sescat, ds_clean$ses_low_high)

# ---- split-education-1 ----------------------
# Initial frequencies
ds_clean %>% 
  dplyr::group_by(edu) %>%
  dplyr::summarize(n=n()) %>% print(n=50)

# Defining categorical variable
ds_clean$educat <- car::Recode(ds_clean$edu,"
                                0:9  = '-1'; 
                               10:11 =  '0';
                               12:30 =  '1';
                               ")
table(ds_clean$educat, useNA = "always")

# Defining dummy variables
# educat   low_med  low_high
#  -1          0        0
#   0          1        0
#   1          0        1
ds_clean <- ds_clean %>% 
  dplyr::mutate(
    edu_low_med  = ifelse(educat == 0, 1, 0 ), 
    edu_low_high = ifelse(educat == 1, 1, 0 ) 
  )
# Verify how `Low vs Med` was encoded:
table(ds_clean$educat, ds_clean$edu_low_med)
# Verify how `Low vs High` was encoded:
table(ds_clean$educat, ds_clean$edu_low_high)

# ---- split-education-2 ----------------------
# additional tranformations 
ds_clean$educatF <- factor(
  ds_clean$educat, 
  levels = c(-1,             0,             1), 
  labels = c("0-9 years", "10-11 years", ">11 years"))
ds_clean %>% 
  dplyr::group_by(educatF, edu) %>% 
  dplyr::summarize(n = n()) %>% 
  as.data.frame() %>% 
  print(nrow=100)
cat("\n Frequencies of categorized education :")
ds_clean %>% 
  dplyr::group_by(educatF) %>% 
  dplyr::summarize(n = n())



# ---- keep-only-legal-states --------------------------------
# list ids with intermidiate missing (im) or right censored (rc) states
ids_with_im    <- unique(ds_clean[ds_clean$state == -1, "id"]) 
cat("\n Number of subjects with intermediate missing state (-1) : ",length(ids_with_im) )
ids_with_rc     <- unique(ds_clean[ds_clean$state == -2, "id"])
cat("\n Number of subjects with right censored state (-2) : ",length(ids_with_rc) )
ids_with_either <- unique(c(ids_with_im, ids_with_rc))
cat("\n Number of subjects with either IMS or RC state(s) : ",length(ids_with_either) )
ids_with_both   <- dplyr::intersect(ids_with_im, ids_with_rc)
cat("\n Number of subjects with both IMS and RC state(s) : ",length(ids_with_both) )
# subset a random sample of individuals if needed
set.seed(42)
ids <- sample(unique(ds_clean$id), 100)


# save clean data object for records and faster access
saveRDS(ds_clean, "./data/unshared/ds_clean.rds")

# ---- describe-age-composition -----------
# # Time intervals in data:
# # the age difference between timepoint for each individual
# intervals <- matrix(NA,nrow(ds_clean),2)
# for(i in 2:nrow(ds_clean)){
#   if(ds_clean$id[i]==ds_clean$id[i-1]){
#     intervals[i,1] <- ds_clean$id[i]
#     intervals[i,2] <- ds_clean$age[i]-ds_clean$age[i-1]
#   }
#   intervals <- as.data.frame(intervals)
#   colnames(intervals) <- c("id", "interval")
# }
# cat("\n Minimum interval length : ",min(intervals[,2], na.rm=T)) 
# cat("\n Maximum interval length : ", max(intervals[,2], na.rm=T))
# # the age difference between timepoint for each individual
# intervals <- intervals[!is.na(intervals[,2]),] # Remove NAs:
# cat("\nTime intervals between observations within individuals:\n")
# print(round(quantile(intervals[,2]),digits))
# 
# # Info on age and time between obser4vations:
# cat("\n Graphs of age distribution :\n")
# opar<-par(mfrow=c(1,3), mex=0.8,mar=c(5,5,3,1))
# hist(ds_clean$age[ds_clean$firstobs==1],col="red",xlab="Age at baseline in years",main="")
# hist(ds_clean$age,col="blue",xlab="Age in data in years",main="")
# hist(intervals[,2],col="green",xlab="Time intervals in data in years",main="")
# opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))



# ---- centering-decisions -----------------------
# centering decisions
age_center = 75;
year_center = 1900

# ---- prepare-for-estimation --------------------

# define the data object to be passed to the estimation call
ds <- ds_clean %>% 
  # dplyr::filter(id %in% ids) %>% # make sample smaller if needed 
  # exclude individuals with missing states
  # dplyr::filter(!id %in% ids_with_im) %>%
  # dplyr::filter(!id %in% ids_with_rc) %>%
  dplyr::mutate(
    male = as.numeric(male), 
    age    = (age - age_center), # centering
    # age_bl = (age_bl - age_bl_center), # centering
    birth_year  = as.integer(birth_year - year_center) # centering (for numerical reasons)
) %>% 
  dplyr::select(
     id            # person identifier
    # ,age_bl        # age at baseline     
    ,birth_year    # year of birth         
    ,male          # sex
    ,sescat        # income at age 40  
    ,edu           # years of education
    ,educat        # education categorized: low, med, high
    # ,educatF       # education categorized: low, med, high (factor)             
    ,edu_low_med   # dummy, edu low as compared to edu medium           
    ,edu_low_high  # dummy, edu low as compared to edu high           
    ,fu_year       # follow-up year       
    ,firstobs      # baseline indicator        
    ,age           # age at visit
    ,state         # outcome state encoded from mmse
  )
# save the object to be used during estimation
saveRDS(ds, "./data/unshared/ds_estimation.rds")

# ---- inspect-before-estimation --------------------
ds <- readRDS("./data/unshared/ds_estimation.rds")
# view data object to be passed to the estimation call
cat("\n\n The following dataset will be passed to msm call (view for one person): \n")
set.seed(44)
ids <- sample(unique(ds$id), 1)
ds %>% dplyr::filter(id %in% ids) %>% knitr::kable()
cat("\n Subject count : ",length(unique(ds$id)),"\n")
cat("\n Frequency of states at baseline\n")
sf <- ds %>% 
  dplyr::filter(firstobs==TRUE) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(count = n()) %>%  # basic frequiencies
  dplyr::mutate(pct = round(count/sum(count),2)) %>%  # percentages, use for starter values
  print()
cat("\n State table: \n") 
print(msm::statetable.msm(state,id,data=ds)) # transition frequencies
# these will be passed as starting values
initial_probabilities <- as.numeric(as.data.frame(sf[!sf$state %in% c(-1,-2),"pct"])$pct) 
initial_probabilities <- c(initial_probabilities,0) # no death state at first observation
cat('\n The inital values for estimation : ', paste0(initial_probabilities, collapse = ", "))


###############################################
## Part II : msm computations                ##
###############################################

# ----- define-estimation-function --------------------------
estimate_multistate <- function(
  model_name            # code name for the model to be estimated (e.g. mB_v1)
  ,ds                   # data object, clean and ready for estimation
  ,Q                    # Q-matrix of transitions
  ,E                    # misspecification matrix
  ,qnames               # names of the rows in the Q matrix
  ,cf                   # string with covariate names for forward transitions
  ,cb                   # string with covariate names for backward transitions
  ,cd                   # string with covariate names for death transitions
){
  cov_forward  <- as.formula(paste0("~",cf))
  cov_backward <- as.formula(paste0("~",cb))
  cov_death    <- as.formula(paste0("~",cd))
  # covariates_ <- as.formula(paste0("~",cov_names))
  covariates_    = list(
    "1-2"       = cov_forward,
    "2-3"       = cov_forward,
    "2-1"       = cov_backward,
    "1-4"       = cov_death,
    "2-4"       = cov_death,
    "3-4"       = cov_death
  )  
  model <- msm(
    formula       = state ~ age,
    subject       = id,
    data          = ds,
    center        = FALSE,
    qmatrix       = Q,
    ematrix       = E,
    death         = TRUE,
    covariates    = covariates_,
    censor        = c(-1,-2),
    censor.states = list(c(1,2,3), c(1,2,3)),
    method        = method_,
    constraint    = constraint_,
    fixedpars     = fixedpars_,
    initprobs     = initprobs_,
    est.initprobs = TRUE,
    control       = list(trace=0,REPORT=1,maxit=1000,fnscale=10000)
  )
  # model <- paste0("test", covariates_)
  saveRDS(model, paste0(path_folder,model_name,".rds"))
  return(model)
} 

  
# ---- define-support-functions ----------------------
get_crude_Q <- function(
  ds
  ,Q
  ,cov_names
){
  formula_ <- as.formula(paste0("state ~ ",cov_names))
  Q_crude <- crudeinits.msm(
    formula = formula_, 
    subject = id, 
    qmatrix = Q, 
    data = ds,     
    censor        = c(-1,-2),
    censor.states = list(c(1,2,3), c(1,2,3)) 
  )
  return(Q_crude)
}

msm_summary <- function(
  model
){
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p    <- model$opt$par
p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
print(cbind(p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
      quote=FALSE)
}

msm_details <- function(
  model
){ 
  # intensity matrix
  cat("\n Intensity matrix : \n")
  print(qmatrix.msm(model)) 
  # qmatrix.msm(model, covariates = list(male = 0))
  # transition probability matrix
  t_ <- 2
  cat("\n Transition probability matrix for t = ", t_," : \n")
  print(pmatrix.msm(model, t = t_)) # t = time, in original metric
  # misclassification matrix
  cat("\n Misclassification matrix : \n")
  suppressWarnings(print(ematrix.msm(model), warnings=F))
  # hazard ratios
  # cat("\n Hazard ratios : \n")
  # print(hazard.msm(model))
  # mean sojourn times
  cat("\n Mean sojourn times : \n")
  print(sojourn.msm(model))
  # probability that each state is next
  cat("\n Probability that each state is next : \n")
  suppressWarnings(print(pnext.msm(model)))
  # total length of stay
  cat("\n  Total length of stay : \n")
  print(totlos.msm(model))
  # expected number of visits to the state
  cat("\n Expected number of visits to the state : \n")
  suppressWarnings(print(envisits.msm(model)))
  # ratio of transition intensities
  # qratio.msm(model,ind1 = c(2,1), ind2 = c(1,2))
}

# ---- specify-model --------------------------
q <- .01
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
  "Healthy - Mild",   # q12
  # "Healthy - Severe", # q13
  "Healthy - Dead",  # q14
  "Mild - Healthy",  # q21  
  "Mild - Severe",   # q23
  "Mild - Dead",     # q24
  # "Severe - Healthy",# q31
  # "Severe - Mild",   # q32
  "Severe - Dead"    # q34
)

# ---- specify-msm-options --------------------------
digits = 2
method_  = "BFGS"     # alternatively, if does not converge "Nedler-Mead" 
constraint_ = NULL    # additional model constraints
fixedpars_ = NULL     # fixed parameters
initprobs_ = initial_probabilities 

(Q_crude <- get_crude_Q(ds, Q, "age")) # starting values for estimation

# ---- estimate-msm-models ------------------------
# turn this chunk OFF when printing the report
# compile model objects with msm() call
# each model will be saved in the specified folder, namely path_folder

# model B version 1 : using categorized education
# estimate_multistate("mB_v1", ds, Q_crude, E, qnames,
#                     cf = "age + male  + educat + sescat",
#                     cb = "age",
#                     cd = "age + male  + educat ")

# model B version 2 : used dummies for education
# estimate_multistate("mB_v2", ds, Q_crude, E, qnames,
#                     cf = "age + male  + edu_low_med + edu_low_high + sescat",
#                     cb = "age",
#                     cd = "age + male  + edu_low_med + edu_low_high")

# ---- inspect-estimated-model -----------------------------
# call in the model object for inspection
# msm_model <- readRDS(paste0(path_folder, "mB_v1.rds")) # educat
msm_model <- readRDS(paste0(path_folder, "mB_v2.rds"))# edu_low_med, edu_low_high
print_hazards(msm_model)
msm_summary(msm_model)
msm_details(msm_model)

###############################################
## Part III : ELECT computations             ##
###############################################

# ---- define-le-functions ---------------
# wrapper function to compute a single conditional Life Expectancy
compute_one_condition <- function(
  msm_model
  ,age_min
  ,age_max
  ,ds_levels
  ,condition_n
){
  # assemble the levels of the covariates
  covar_list <- list(age    = age_min)
  for(i in colnames(ds_levels)){
    covar_list[[i]] = ds_levels[condition_n, i]
  }
  # estimate Life Expectancies
  LE <- elect(
    model          = msm_model,    # fitted msm model
    b.covariates   = covar_list,   # list with specified covarites values
    statedistdata  = ds_alive,     # data for distribution of living states
    time.scale.msm = time_scale,   # time scale in multi-state model ("years", ...)
    h              = grid_par,     # grid parameter for integration
    age.max        = age_max,      # assumed maximum age in years
    S              = replication_n # number of simulation cycles
  )
  return(LE)
}

# wrapper function to compute ALL conditional LEs for a given set of covariate conditions
# uses compute_one_condition()
# uses organize_sim_results()
compute_conditional_les <- function(
  folder,
  model_name,
  age_min,
  age_max,
  ds_levels,
  condition_n = "all"
){
  (model_path_in  <- paste0(folder,model_name,   '.rds')) # msm object
  (model_path_out <- paste0(folder,model_name,'_',age_min+age_center,'_',age_max+age_center,'.rds')) # msm + elect objects
  model <- list() # initiate the list object
  model[["msm"]] <- readRDS(model_path_in) # import msm model object
  # store conditional levels of covariates for future reference
  model[["levels"]] <- ds_levels
  # loop through all conditional values of covariates
  if(condition_n == "all"){
    tested_conditions <- 1:nrow(ds_levels) 
  }else{
    tested_conditions <- condition_n
  }
  for(i in tested_conditions){  
    (condition_number <- paste0(as.character(i)))
    model[["le"]][[condition_number]] <- compute_one_condition(
      msm_model   = model[["msm"]], 
      age_min     = age_min, 
      age_max     = age_max,
      ds_levels   = ds_levels, 
      condition_n = i
    ) 
  }
  # return(model)
  model <- organize_sim_results(model)
  saveRDS(model,model_path_out)
}

# simplified verision of summary.elect()
# this function only a temp hack before such functionality is added to summary.elect()
describe_reps<- function(
  LEs
  ,probs = c(.025,0.5,.975)
){
  (pnt <- LEs$pnt)
  (e_names <- attr(LEs$pnt, "names"))
  sim <- LEs$sim
  (mn <- apply(LEs$sim,2,mean))
  (se <- apply(LEs$sim,2,sd))
  (quants <- matrix(NA,ncol(LEs$sim),length(probs)))
  for(i in 1:ncol(LEs$sim)){
    for(j in 1:length(probs)){
      quants[i,j] <- quantile(LEs$sim[,i],probs=probs[j])
    }   
  }     
  out <- as.data.frame(cbind(pnt,mn,se,quantiles=quants))
  for(j in 4:(3+length(probs))){
    names(out)[j]<-paste(probs[j-3],"q",sep="")
  }
  return(out)
}
# describe_reps(model$le)

# organize the results of replication
# uses describe_reps()
organize_sim_results <- function(
  # model_path
  model
){
  # model <- readRDS(model_path)
  # lapply(model, names)
  for(i in seq_along(model$le)){
    
    (d0 <- model$levels[i,])
    (d1 <- describe_reps(model$le[[i]]))
    (d2 <- cbind(d1,d0))
    (d2$e_name <- attr(model$le[[i]]$pnt, "names") )
    (d2$condition_n <- i)
    model[["descriptives"]][[paste0(i)]] <- d2
  }
  # lapply(model, names) 
  # model$descriptives$`1`
  
  d <- do.call("rbind", model$descriptives)
  rownames(d) <- NULL
  (covariate_names <- colnames(model$levels))
  (stem_columns <- c("condition_n", "e_name", covariate_names))
  (standard_columns <-c("pnt", "mn", "se", "`0.025q`", "`0.5q`","`0.975q`" ))
  (ordered_columns <- c(stem_columns, standard_columns))
  d <- d %>% 
    dplyr::select_(.dots= ordered_columns) %>%
    dplyr::mutate(
      pnt      = sprintf("%0.2f", as.numeric(pnt)),
      mn       = sprintf("%0.2f", as.numeric(mn)),
      se       = sprintf("%0.2f", as.numeric(se)),
      `0.025q` = sprintf("%0.2f", as.numeric(`0.025q` )),
      `0.5q`   = sprintf("%0.2f", as.numeric(`0.5q` )),
      `0.975q` = sprintf("%0.2f", as.numeric(`0.975q` ))
      
    )
  model[["descriptives"]] <- d
  # saveRDS(model, model_path)
  return(model)
}
# organize_sim_results(
# model_path = "./data/shared/derived/models/retired/model-b-mod-2/mB_mod2_3_80_110.rds"
# )

# ---- specify-elect-options --------------------------
alive_states <- c(1,2,3)
ds_alive <- ds[ds$state %in% alive_states,]

grid_par <- .5
time_scale <- "years"
replication_n <- 500

# ---- compute-life-expectancies-version-1 -------------------

# assemble covariate levels
ds_levels <- tidyr::crossing(
   male   = c(0, 1)   
  ,educat = c(-1, 0, 1)
  ,sescat = c(-1, 0, 1)
) %>% as.data.frame() 
ds_levels %>% print()
# conduct simulation - turn off when printing report
# for(i in c(-15,-10,-5,0,5,10)){ # centered at 75
#   compute_conditional_les(
#     folder      =  "./data/shared/derived/models/model-b/",
#     model_name  = "mB_v1",
#     age_min     = i, 
#     age_max     = 35,
#     ds_levels   = ds_levels,
#     condition_n = "all"
#   )
# }

# ---- compute-life-expectancies-version-2 -------------------

# assemble covariate levels
ds_levels <- tidyr::crossing(
   male         = c(0, 1)   
  ,edu_low_high = c(0,1)
  ,edu_low_med  = c(0,1)
  ,sescat       = c(-1,0,1)
) %>% 
  dplyr::filter(!(edu_low_med==1 & edu_low_high==1)) %>% # exclude impossible levels
  as.data.frame() 
ds_levels %>% print()
# conduct simulation - turn off when printing report
for(i in c(-15,-10,-5,0,5,10)){ # centered at 75
  compute_conditional_les(
    folder      =  "./data/shared/derived/models/model-b/",
    model_name  = "mB_v2",
    age_min     = i , # centered at 75
    age_max     = 35 ,
    ds_levels   = ds_levels ,
    condition_n = "all"
  )
}

# ---- recap-1 -----------------
list.files(file.path(path_folder),full.names=T) %>% as.data.frame() %>% format(justify="left")

# ---- recap-2 ------------------
model_path = "./data/shared/derived/models/model-b/mB_v1_60_110.rds"
model <- readRDS(model_path)
lapply(model, names)
model$levels
print_hazards(model$msm)
condition_n = 1
le <- model$le[[condition_n]]
summary.elect(le)
plot.elect(le)




