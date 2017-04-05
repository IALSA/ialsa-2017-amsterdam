# A three-state progressive survival model: exploring the likelihood,
# and the computation involved

# Example with toy data. Not the quickest code, but illustrative of how
# the likelihood is defined. 

# Ardo, UCL 2016

# Requirements:
# - R package msm should be installed
# - Working directory contains the files <dataEx1.RData> 
#   (You can set the working directory in R by using the pull-down menu)

# Preliminaries:
library(msm)
# Define rounding in output:
digits <- 3

# Load data <dta>: 
load("dataEx1.RData")

# Remove records with censoring (just to simplify the illustration):
dta <- dta[dta$state!= -2,]

# Data info:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
cat("\nSample size:",N,"\n")
cat("\nFrequencies observed state:"); print(table(dta$state))
cat("\nFrequencies number of observations:"); print(table(table(dta$id)))
cat("\nState table:"); print(statetable.msm(state,id,data=dta))

# Prepare data for quick access individual data:
dta.split <- split(dta,dta$id)
cat("\nData pre-formatted.\n")

# Choose model:
Model <- 1
covariates <- as.formula("~age")
# Generator matrix Q:
q <- 0.01; Q <- rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames <- c("q12","q21","q13","q23")
# Optimisation method ("BFGS" or "Nelder-Mead"):
method <- "BFGS"

# Model fit using msm:
if(1){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates, method=method,
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}

# Generate output:
cat("\n---------------------------------------\n")
cat("\nModel",Model," with covariates: "); print(covariates)
cat("Convergence code =", model$opt$convergence,"\n")
minus2LL <-  model$minus2loglik
cat("-2loglik =", minus2LL,"\n")
p <- model$estimates
p.se <- sqrt(diag(model$covmat))
# Results to screen:
cat("\nParameter estimats and SEs:\n")
print(cbind(q=qnames,p=round(p,digits),se=round(p.se,digits)),quote=FALSE)
cat("\n---------------------------------------\n")

##############################################
# Functions for P matrix:
# Using eigenvalue decomposition explicitly:
Pmatrix1 <- function(t1,t2,Q){
  t <- t2-t1
  eigen.Q <- eigen(x=Q*t,symmetric=FALSE)  
  exp.D <- diag(exp(eigen.Q$values))
  U <- cbind(eigen.Q$vectors)
  U%*%exp.D%*%solve(U)
}
# P matrix analytically:
# See Welton and Ades (2004) and Van den Hout (2016, Appendix A)
# Eqns for P[1,2] is a simplified version of the one in Welton and Ades (2004)
Pmatrix2 <- function(t1,t2,Q){
  t <- t2-t1
  P <- diag(3)
  la1 <- Q[1,2]+Q[1,3]
  la2 <- Q[2,1]+Q[2,3]
  h <- sqrt((la1-la2)^2+4*Q[1,2]*Q[2,1])
  P[1,1] <- ((-la1+la2+h)*exp(-(la1+la2-h)*t/2)+(la1-la2+h)
            *exp(-(la1+la2+h)*t/2))/(2*h)
  P[1,2] <- Q[1,2]*(exp(-(la1+la2-h)*t/2)-exp(-(la1+la2+h)*t/2))/h
  P[1,3] <- 1-P[1,1]-P[1,2]
  P[2,1] <- (Q[2,1]*(exp(-(la1+la2-h)*t/2)-exp(-(la1+la2+h)*t/2)))/h
  P[2,2] <- ((la1-la2+h)*exp(-(la1+la2-h)*t/2)+(-la1+la2+h)
             *exp(-(la1+la2+h)*t/2))/(2*h)
  P[2,3] <- 1-P[2,1]-P[2,2]
  P[3,] <- c(0,0,1)
  P
}
# Using the msm function for P matrix:
Pmatrix3 <- function(t1,t2,Q){ MatrixExp(mat=Q,t=t2-t1) }

# Choose your favourite computation to be used in likelihood function:
Pmatrix <- Pmatrix2 

# Define dead state as D:
D <- 3

# Coding of $-1\times$log-likelihood:
loglikelihood <- function(p){
  # Model parameters:
  beta <- p[1:4]; xi <- p[5:8]
  # Initialise loglikehood:
  loglik <- 0
  # Loop over individual data:
  for(i in 1:N){
   # Data for subject i:
   dta.i <- dta.split[[i]]
   O <- dta.i$state; t <- dta.i$age
   # Loop over observations for subject i:
   for(j in 2:length(O)){
     # Q and P matrix:
     Q <- matrix(0,D,D)
     t1 <- t[j-1]; t2 <- t[j]
     Q[1,2]<- exp(beta[1]+xi[1]*t1)
     Q[1,3]<- exp(beta[2]+xi[2]*t1); Q[1,1]<- -sum(Q[1,])
     Q[2,1]<- exp(beta[3]+xi[3]*t1)
     Q[2,3]<- exp(beta[4]+xi[4]*t1); Q[2,2]<- -sum(Q[2,])
     P <- Pmatrix(t1=t1,t2=t2,Q)
     # Likelihood contribution:
     death <- as.numeric(O[j]==D)
     loglik <- loglik+log((1-death)*P[O[j-1],O[j]] +
               (death)*(P[O[j-1],1]*Q[1,D]+P[O[j-1],2]*Q[2,D]))
   }
 }
 # monitor:
 #cat(2*loglik,"\n")
 
 # Function return:
 return(-loglik)
}

# Check likelihood function with msm:
cat("Check coded loglikelihood function with msm:\n")
cat("-2Loglikelihood at msm estimate:",2*loglikelihood(model$opt$par),"\n")

# Starting values:
p.start <- c(-3,-3,-3,-3,0,0,0,0)

# Maximise (this will take a while...):
cat("\n\nMAXIMISING ...........\n")
#max <- optim(par=p.start, fn=loglikelihood, method = "Nelder-Mead", 
#              hessian=TRUE,control=list(trace=F,maxit=5000))
cat("\n-2loglik =", 2*max$value,"\n")
conv <- max$opt$convergence; cat("Convergence code =", conv,"\n")
p <- max$par
if(det(max$hessian)!=0){
    variance <- diag(solve(max$hessian))
    }else{
    variance <- rep(NA,length(p))
}
p.se <- sqrt(variance)
print(round(cbind(p,p.se),digits))

# Notice difference in point estimates and maximum, and similarity in
# estimated SEs.



# Models with parameter restrictions:
# Choose model:
Model <- 2
covariates <- as.formula("~age+x")
fixedpars <- c(10:12)
# Generator matrix Q:
q <- 0.01; Q <- rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames <- c("q12","q21","q13","q23")
# Optimisation method ("BFGS" or "Nelder-Mead"):
method <- "BFGS"

# Model fit using msm:
if(1){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates, method=method,
             fixedpars=fixedpars,
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}
# Generate output:
cat("\n---------------------------------------\n")
cat("\nModel",Model," with covariates: "); print(covariates)
cat("\nFixedpars = ",fixedpars,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
minus2LL <-  model$minus2loglik
cat("-2loglik =", minus2LL,"\n")
p <- model$estimates
p.se <- sqrt(diag(model$covmat))
# Results to screen:
cat("\nParameter estimats and SEs:\n")
print(cbind(q=qnames,p=round(p,digits),se=round(p.se,digits)),quote=FALSE)
cat("\n---------------------------------------\n")


# Coding of $-1\times$log-likelihood:
loglikelihood <- function(p){
  # Model parameters:
  beta <- p[1:4]; xi <- p[5:8]; gamma <- p[9]
  # Initialise loglikehood:
  loglik <- 0
  # Loop over individual data:
  for(i in 1:N){
   # Data for subject i:
   dta.i <- dta.split[[i]]
   O <- dta.i$state; t <- dta.i$age; x <- dta.i$x[1]
   # Loop over observations for subject i:
   for(j in 2:length(O)){
     # Q and P matrix:
     Q <- matrix(0,D,D)
     t1 <- t[j-1]; t2 <- t[j]
     Q[1,2]<- exp(beta[1]+xi[1]*t1+gamma[1]*x)
     Q[1,3]<- exp(beta[2]+xi[2]*t1); Q[1,1]<- -sum(Q[1,])
     Q[2,1]<- exp(beta[3]+xi[3]*t1)
     Q[2,3]<- exp(beta[4]+xi[4]*t1); Q[2,2]<- -sum(Q[2,])
     P <- Pmatrix(t1=t1,t2=t2,Q)
     # Likelihood contribution:
     death <- as.numeric(O[j]==D)
     loglik <- loglik+log((1-death)*P[O[j-1],O[j]] +
               (death)*(P[O[j-1],1]*Q[1,D]+P[O[j-1],2]*Q[2,D]))
   }
 }
 # monitor:
 #cat(2*loglik,"\n")
 
 # Function return:
 return(-loglik)
}

# Check likelihood function with msm:
cat("Check coded loglikelihood function with msm:\n")
cat("-2Loglikelihood at msm estimate:",2*loglikelihood(model$opt$par),"\n")

# Starting values:
p.start <- c(-3,-3,-3,-3,0,0,0,0,0)

# Maximise (this will take a while...):
cat("\n\nMAXIMISING ...........\n")
max <- optim(par=p.start, fn=loglikelihood, method = "Nelder-Mead", 
              hessian=TRUE,control=list(trace=F,maxit=5000))
cat("\n-2loglik =", 2*max$value,"\n")
conv <- max$opt$convergence; cat("Convergence code =", conv,"\n")
p <- max$par
if(det(max$hessian)!=0){
    variance <- diag(solve(max$hessian))
    }else{
    variance <- rep(NA,length(p))
}
p.se <- sqrt(variance)
print(round(cbind(p,p.se),digits))