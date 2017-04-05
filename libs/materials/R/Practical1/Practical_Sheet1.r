# A three-state progressive survival model: exploring msm and ELECT
# Example with toy data 

# Ardo, UCL 2016

# Requirements:
# - R package msm should be installed
# - Working directory contains the files <dataEx1.RData> and <ELECT.r>
#   (You can set the working directory in R by using the pull-down menu)

###############################
# SHEET 1
# Q1:

# Preliminaries:
library(msm)
source("./libs/materials/R/Practical1/ELECT.r")
# Define rounding in output:
digits <- 3

# Load data <dta>: 
load("./libs/materials/R/Practical1/dataEx1.RData")

# Data info:
subjects <- as.numeric(names(table(dta$id)))
N <- length(subjects)
cat("\nSample size:",N,"\n")
cat("\nFrequencies observed state:"); print(table(dta$state))
cat("\nFrequencies number of observations:"); print(table(table(dta$id)))
cat("\nState table:"); print(statetable.msm(state,id,data=dta))

# Add first observation indicator:
cat("\nFirst observation indicator is added.\n")
offset <- rep(NA,N)
for(i in 1:N){offset[i] <- min(which(dta$id==subjects[i]))}
firstobs <- rep(0,nrow(dta))
firstobs[offset] <- 1
dta <- cbind(dta,firstobs=firstobs)

# Time intervals in data:
intervals <- matrix(NA,nrow(dta),2)
for(i in 2:nrow(dta)){
  if(dta$id[i]==dta$id[i-1]){
     intervals[i,1] <- dta$id[i]
     intervals[i,2] <- dta$age[i]-dta$age[i-1]
  }
}
# Remove the N NAs:
intervals <- intervals[!is.na(intervals[,2]),]
cat("\nTime intervals between observations within individuals:\n")
print(round(quantile(intervals[,2]),digits))

# Info on age and time between observations:
opar<-par(mfrow=c(1,3), mex=0.8,mar=c(5,5,3,1))
hist(dta$age[dta$firstobs==1],col="red",xlab="Age at baseline in years",main="")
hist(dta$age,col="blue",xlab="Age in data in years",main="") 
hist(intervals[,2],col="green",xlab="Time intervals in data in years",main="") 
opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))

###############################
# SHEET 1
# Q2:

# Choose model (0/1/2):
Model <- 2

# Generator matrix Q:
q <- 0.01; Q <- rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames <- c("q12","q21","q13","q23")
# Optimisation method ("BFGS" or "Nelder-Mead"):
method <- "BFGS"
    
# Model formulation:
if(Model==0){
  # Covariates:
  covariates <- as.formula("~1")
  constraint <- NULL 
  fixedpars <- NULL
}
if(Model==1){
  # Covariates:
  covariates <- as.formula("~age")
  constraint <- NULL 
  fixedpars <- NULL
}
if(Model==2){
  # Covariates:
  covariates <- as.formula("~age+x")
  constraint <- NULL 
  fixedpars <- NULL
}

# Model fit:
if(1){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates,
             censor= -2, censor.states=c(1,2), method=method,
             constraint=constraint,fixedpars=fixedpars, 
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}

# Generate output:
cat("\n---------------------------------------\n")
cat("\nModel",Model," with covariates: "); print(covariates)
cat("and constraints:\n"); print(constraint)
cat("and fixedpars:\n"); print(fixedpars)
cat("Convergence code =", model$opt$convergence,"\n")
minus2LL <-  model$minus2loglik
AIC <- minus2LL + 2*length(model$opt$par)
cat("\n-2loglik =", minus2LL,"\n")
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


###############################
# SHEET 1
# Q3:

# Life expectancies for Model 2:
if(Model==2){


# Point-estimate life expectancies:
sddata <- dta[dta$state%in%c(1,2),]
age0 <- 65
age.max <- 115
x0 <- 0
LEs.pnt <- elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=0.5, age.max=age.max, S=0)
summary.elect(LEs.pnt, digits=2)


# Plot LEs for an age range with CIs:
age.range <- 65:95
probs <- c(.025,.5,.975)
L <- length(age.range)
LEs <- array(NA,c(L,length(LEs.pnt$pnt),length(probs)))
for(i in 1:L){
  age0 <- age.range[i]
  x0 <- 0
  cat("Running simulation for age = ",age0,"and x = ",x0,"\n")
  results<-elect(model=model, b.covariates=list(age=age0,x=x0),
                 statedistdata=sddata, time.scale.msm="years",
                 h=0.5, age.max=age.max, S=50,setseed=12345)
  for(j in 1:7){
   for(k in 1:length(probs)){
     LEs[i,j,k] <- quantile(results$sim[,j],probs=probs[k])
   } 
  }
}
x.axis <- c(min(age.range),max(age.range))
y.axis <- c(0,20)
plot(x.axis,y.axis,ylab="Life Expectancy",xlab="Age",type="n",cex.lab=1.5)
# LEs e11:
lines(age.range,LEs[,1,1],col="red",lwd=1)
lines(age.range,LEs[,1,2],col="red",lwd=3)
lines(age.range,LEs[,1,3],col="red",lwd=1)
# LEs e12:
lines(age.range,LEs[,2,1],col="blue",lwd=1,lty=2)
lines(age.range,LEs[,2,2],col="blue",lwd=3,lty=2)
lines(age.range,LEs[,2,3],col="blue",lwd=1,lty=2)

}