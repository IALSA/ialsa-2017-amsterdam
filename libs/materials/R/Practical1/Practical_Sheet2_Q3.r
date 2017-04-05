# A three-state progressive survival model: GOF given msm model

# Ardo, UCL 2016


###############################
# SHEET 2
# Q3:

# Preliminaries:
library(msm)
library(survival)
# Define rounding in output:
digits <- 3

# Load data <dta>: 
load("dataEx1.RData")

# Data info:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
cat("\nSample size:",N,"\n")
cat("\nFrequencies observed state:"); print(table(dta$state))
cat("\nFrequencies number of observations:"); print(table(table(dta$id)))
cat("\nState table:"); print(statetable.msm(state,id,data=dta))

# Function to generate model output:
output.model <- function(model){
  # Generate output:
  cat("\n---------------------------------------\n")
  cat("\nModel with covariates: "); print(covariates)
  cat("Convergence code =", model$opt$convergence,"\n")
  minus2LL <-  model$minus2loglik
  AIC <- minus2LL + 2*length(model$opt$par)
  cat("-2loglik =", minus2LL,"\n")
  cat("AIC =", AIC,"\n")
  p <- model$estimates
  p.se <- sqrt(diag(model$covmat))
  # Univariate Wald tests:
  wald <- round((p/p.se)^2,digits)
  pvalue <- round(1-pchisq((p/p.se)^2,df=1),digits)
  # Do not test intercepts:
  wald[1:sum(names(p)=="qbase")] <- "-"
  pvalue[1:sum(names(p)=="qbase")] <- "-"
  # Results to screen:
  cat("\nParameter estimats and SEs:\n")
  print(cbind(q=qnames,p=round(p,digits),
        se=round(p.se,digits),"Wald ChiSq"=wald,
        "Pr>ChiSq"=pvalue),quote=FALSE)
  cat("\n---------------------------------------\n")
  # Out:
  list(p=p,p.se=p.se,minus2LL=minus2LL,AIC=AIC)
 }

##########################################
# Using Model 1
# Generator matrix Q:
q <- 0.01; Q <- rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames <- c("q12","q21","q13","q23")
# Optimisation method ("BFGS" or "Nelder-Mead"):
method <- "BFGS"
# Covariates:
covariates <- as.formula("~age+x")

# Fit model
if(1){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates,
             censor= -2, censor.states=c(1,2), method=method, 
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}
output.model(model)


####################################
# COde for GOF w.r.t. survival only

# Number of years for prediction:
L <- 14
# Dead state number = total number of states:
D <- 3

#############################
# State specific survival:
opar <- par(mfrow=c(1,2), mex=0.8,mar=c(5,5,2,.2)+.1)

# Loop over living states:
for(State in c(1,2)){

# Baseline data:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
b.index <- rep(NA,N)
for(i in 1:N){ b.index[i] <- min(which(dta$id==subjects[i]))}
b.dta <- dta[b.index,]

# Select subjects in State at baseline:
b.dta <- b.dta[b.dta$state==State,]
subjects <- as.numeric(names(table(b.dta$id)))
N <- length(subjects)

# Univariate survival data:
eventD <-rep(NA,N)
timeD  <-rep(NA,N)
for(i in 1:N){ 
 # Baseline covariate values for prediction:
 data.i  <-dta[dta$id==subjects[i],]
 state.i <-data.i$state
 time.i  <-data.i$age
 eventD[i] <- as.numeric(state.i[length(state.i)]==D)
 timeD[i]  <- time.i[length(time.i)]-time.i[1]
}
s.dta <- cbind(b.dta,timeD=timeD,eventD=eventD)

# Transition matrices per individual:
P  <- array(NA,c(N,L,D,D))
PM <- array(NA,c(N,L,D,D))

# Loop over individuals:
for(i in 1:N){ 
# Baseline covariate values for prediction:
data.i <- b.dta[b.dta$id==subjects[i],]
age0  <- data.i$age
state <- data.i$state
x <- data.i$x
# Age grid in years:
age.grid <- seq(age0,age0+L,by=1)

# P-matrices per time interval:
P[i,1,1:D,1:D] <- diag(D)
for(j in 2:L){
  t <- age.grid[j]-age.grid[j-1]
  age <- age.grid[j-1]
  # P matrix:
  P[i,j,1:D,1:D] <- pmatrix.msm(model,t=t, covariates=list(age=age,x=x))
}
# P-matrices for whole time grid. And their rows:
PM[i,1,1:D,1:D] <- P[i,1,1:D,1:D]
for(l in 2:L){
  PM[i,l,1:D,1:D] <- PM[i,l-1,1:D,1:D]%*%P[i,l,1:D,1:D]
}
}
# Expected survival at grid points:
mn.prob <- rep(NA,L)
for(l in 1:L){ mn.prob[l] <- median(PM[,l,State,D])}
Survvl <- 1-mn.prob

# Time grid in years:
times <- seq(0,L-1,by=1)
# Plotting frame: 
text <- paste("Survival from state ",as.character(State),
              " (N=",as.character(N),")",sep="")
xlab <- "Years since baseline"
plot(c(0,L-1),c(0,1),type="n",ylab=text,xlab=xlab,cex.lab=1.2)
# Model-based individual curves:
for(i in 1:N){
  S.prob <- rep(NA,L); for(l in 1:L){ S.prob[l] <- PM[i,l,State,D]}
  lines(times,1-S.prob,lwd=1.5,col="gray",lty=1)
}  
# Model-based median:
lines(times,Survvl,lwd=3,col="blue",lty=1)
# Kaplan-Meier based:
s.model <- survfit(Surv(timeD, eventD) ~ 1, data=s.dta)
lines(s.model,lwd=1.5,lty=2,conf.int="only")
lines(s.model,lwd=2)
} 


