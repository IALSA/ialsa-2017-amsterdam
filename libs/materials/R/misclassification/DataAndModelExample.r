# Data input and estimation of a four-state model with misclassification

# Ardo, May 2016

# Data & modelling are for illustration only 

# Prelim:
library(msm)
digits <- 3

# Load data:
load(file="FourStateExample.RData")
# Transform age to prevent numerical problems with estimation:
dta$age <- dta$age/20 

# Data info:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
cat("Sample size =",N,"\n")
cat("Frequencies observed state:")
print(table(dta$state))
cat("State table:")
print(statetable.msm(state,id,data=dta)) 

# More data info:
cat("\nFrequencies for number of observations per individual (incl. death):")
print(table(table(dta$id)))
  
# Mean length of intervals (always good to check this):
len <- rep(NA,nrow(dta))
ind <- 1
for(i in 2:nrow(dta)){
   if(dta$id[i]==dta$id[i-1]){
     int<-dta$age[i]-dta$age[i-1]
     if(int>0){len[ind]<-int;ind<-ind+1}
  }
}
len <- len[!is.na(len)]
cat("\nMin length of intervals = ",min(len),"\n")
cat("Median length of intervals = ",median(len),"\n")
cat("Max length of intervals = ",max(len),"\n")


#####################################
# MODEL:

# With misclassification (MC <- TRUE) of without (MC <- FALSE):
MC <- TRUE

##########
# With MC:
if(MC){ 

# Generator matrix Q:
q <- 0.1
Q <- rbind( c(0,q,0,q),
            c(q,0,q,q),
            c(0,0,0,q),
            c(0,0,0,0))
 
# Choose model:
Model <- 1
# Model formulation:
if(Model==1){
  # Covariates:
  covariates  <- as.formula("~age")
  constraint  <- NULL
  fixedpars   <- 8
  # Control:
  method <- "BFGS"
}

# Misclassification matrix for msm function call:
ematrix <- rbind(c(0, 0, 0,0), c(0, 0, 0.1,0), 
                 c(0, 0, 0,0), c(0, 0, 0,0))

# Fit model:
model <- msm(state ~ age, subject = id, data = dta,
           qmatrix = Q, ematrix = ematrix, covariates=covariates, 
           constraint=constraint, fixedpars=fixedpars,  center=FALSE,
           initprobs = c(.5,.4,.1,0), est.initprobs =TRUE,
           death = TRUE, method = method, control=list(maxit=3000))

# Generate output in user-written format:
# ( Alternatively just type "print(model)" )
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p    <- model$opt$par
p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
print(cbind(p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
            quote=FALSE)

# Estimated misclassification matrix:
cat("\nMisclassification matrix:\n")
E.msm <- ematrix.msm(model, covariates="mean", ci="delta")
print(E.msm)
}


#############
# Without MC:
if(!MC){

# Generator matrix Q:
q <- exp(-5)
Q <- rbind( c(0,q,0,q),
            c(q,0,q,q),
            c(0,q,0,q),
            c(0,0,0,0))
 
# Choose model:
Model <- 1
# Model formulation:
if(Model==1){
  # Covariates:
  covariates  <- as.formula("~age")
  constraint  <- NULL
  fixedpars   <- c(10,13)
  # Control:
  method <- "BFGS"
}

# Fit model:
model <- msm(state ~ age, subject = id, data = dta,
           qmatrix = Q, covariates=covariates,
           constraint=constraint, fixedpars=fixedpars, 
           deathexact = 4, method = method, control=list(maxit=3000))

# Generate output in user-written format:
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("AIC =", model$minus2loglik+2*length(model$opt$par),"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p    <- model$opt$par
p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
print(cbind(p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
            quote=FALSE)


}