# A three-state progressive survival model: further exploration of msm 

# Ardo, UCL 2016


###############################
# SHEET 2
# Q1:

# Preliminaries:
digits <- 3

# Function to generate model out put:
output.model <- function(model){
  # Generate output:
  cat("\n---------------------------------------\n")
  cat("\nModel",Model," with covariates: "); print(covariates)
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
# Using Model 1: role of age
Model <- 1
# Covariates:
covariates <- as.formula("~age")
if(1){
model <- msm(state~age, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates,
             censor= -2, censor.states=c(1,2), method=method, 
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}
output.model(model)

# Add time since baseline to data:
for(i in 1:N){
  dta.i <- dta[dta$id==subjects[i],]
  time <- dta.i$age - dta.i$age[1]
  dta.i <- cbind(dta.i, time=time )
  # Build new data set:
  if(i==1){new.dta <- dta.i}else{new.dta <- rbind(new.dta,dta.i)}
}
dta <- new.dta

# Fit Model 1 with age ~ time
if(1){
model <- msm(state~time, subject=id, data=dta, center=FALSE, 
             qmatrix=Q, death=TRUE, covariates=covariates,
             censor= -2, censor.states=c(1,2), method=method, 
             control=list(trace=0,REPORT=1,maxit=1000,fnscale=10000))
}
output.model(model)

##########################################
# Using Model 1: transition probabilities
# Preliminaries:
digits <- 5


# Using constant hazard: for one year:
P1 <- pmatrix.msm(model, t=1, covariates=list(age=70))
cat("\nTransition probabilities for 1 year given age 70:\n")
print(round(P1,digits))
P2 <- pmatrix.msm(model, t=1, covariates=list(age=71))
cat("\nTransition probabilities for 1 year given age 71:\n")
print(round(P2,digits))

# Using piecewise-constant hazards: for two years:
cat("\nTransition probabilities for 2 years given age 70:\n")
cat("Method 1:\n")
print(round(P1%*%P2,digits))

# Using msm function for two years:
P3 <- pmatrix.piecewise.msm(model, t1=1, t2=3, times=2, 
           covariates= list(list(age=70),list(age=71)))
cat("Method 2:\n")
print(round(P3,digits))


###############################
# SHEET 2
# Q2:

if(0){
########################
# Graph with intensities:
# Extract parameters:
p <- model$estimates
beta <- p[1:4]
xi <- p[5:8]
# Initialise: 
Q <- matrix(0,3,3)
age.grid <- seq(from=70,to=90,by=1) 
intensities <- matrix(NA,4,length(age.grid))
# Compute hazards (intensities):
for(j in 1:length(age.grid)){
  t1 <- age.grid[j]
  Q[1,2] <- exp(beta[1]+xi[1]*t1)
  Q[1,3] <- exp(beta[2]+xi[2]*t1); Q[1,1] <- -(Q[1,2]+Q[1,3])
  Q[2,1] <- exp(beta[3]+xi[3]*t1)
  Q[2,3] <- exp(beta[4]+xi[4]*t1); Q[2,2] <- -(Q[2,1]+Q[2,3])
  intensities[1,j] <- Q[1,2]
  intensities[2,j] <- Q[1,3]
  intensities[3,j] <- Q[2,1]
  intensities[4,j] <- Q[2,3]
}
# Make a nice graph:
opar <- par(mfrow=c(2,2), mex=0.8,mar=c(5,5,2,1))
UB <- .2
lwd <- 3
cex.lab <- 1.5
cex <- 1.5
labs <- cbind(c(1,1,2,2),c(2,3,1,3))
for(i in 1:4){
  if(i%in%c(3,4)){xlab="Age in years"}else{xlab=""}
  if(i%in%c(1,3)){ylab="Hazard"}else{ylab=""}
  plot(c(min(age.grid),max(age.grid)),c(0,UB),
       type="n",ylab=ylab,xlab=xlab,main="",cex.lab=cex.lab)
  # Add text:
  label <- paste("From",as.character(labs[i,1]))
  label <- paste(label,"to")
  label <- paste(label,as.character(labs[i,2]))
  text(min(age.grid),.9*UB,pos=4,labels=label,cex=cex)
  lines(age.grid, intensities[i,],lwd=lwd,col=2)
  abline(h=0,col="grey")
}
opar <- par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))
}

if(1){
##############################
# Graph with transition probs:
# Extract parameters:
p <- model$estimates
beta <- p[1:4]
xi <- p[5:8]
# Initialise: 
Q <- matrix(0,3,3)
age.grid <- seq(from=70,to=90,by=1) 
Pmatrix <- matrix(NA,6,length(age.grid))
# Compute hazards (intensities):
for(j in 1:length(age.grid)){
  t1 <- age.grid[j]
  if(j==1){
    P <- diag(3)
  }else{
    P <- P%*%pmatrix.msm(model, t=1, covariates=list(age=t1))
  }
  Pmatrix[1,j] <- P[1,1]
  Pmatrix[2,j] <- P[1,2]
  Pmatrix[3,j] <- P[1,3]
  Pmatrix[4,j] <- P[2,1]
  Pmatrix[5,j] <- P[2,2]
  Pmatrix[6,j] <- P[2,3]
}
# Make a nice graph:
opar <- par(mfrow=c(2,3), mex=0.8,mar=c(5,5,2,1))
UB <- 1
lwd <- 3
cex.lab <- 1.5
cex <- 1.5
labs <- cbind(c(1,1,1,2,2,2),c(1,2,3,1,2,3))
for(i in 1:6){
  if(i%in%c(4,5,6)){xlab="Age in years"}else{xlab=""}
  if(i%in%c(1,4)){ylab="Cumulative trans. prob."}else{ylab=""}
  plot(c(min(age.grid),max(age.grid)),c(0,UB),
       type="n",ylab=ylab,xlab=xlab,main="",cex.lab=cex.lab)
  # Add text:
  label <- paste("From",as.character(labs[i,1]))
  label <- paste(label,"to")
  label <- paste(label,as.character(labs[i,2]))
  text(min(age.grid)+2,.9*UB,pos=4,labels=label,cex=cex)
  lines(age.grid, Pmatrix[i,],lwd=lwd,col=2)
  abline(h=0,col="grey")
}
opar <- par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1))
}