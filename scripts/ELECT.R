# Functions to estimate LEs given an msm() model in R
# Version 0.2

# Ardo, Cambridge 2010 - UCL 2016


# Prelim:
# Load library used for multinomial logistic regression:
library(nnet)
 
################################################
################################################
# ELECT function:
elect<-function(model, b.covariates, statedistdata, time.scale.msm="years",
                 h, age.max, S=0, setseed=NULL,  RestrAndConst=NULL, 
                 statedist.covariates="age", method="step"){
    
    # Explanation:
    # <model> = R object defined by msm() after fitting a model 
    # <b.covariates> = list with specified covariates values (ignore intercept) 
    # <time.scale.msm> = time scale in msm() model either in the set {"years",
    #       "months", "weeks") or a  value  in (0,1]. Example: if you divide 
    #       variable age by 10 before fitting the model, then scale = 1/10
    # <h> = grid parameter for integration and piecewise-constant model.
    #       In scale given by <time.scale.msm>
    # <statedistdata> = the data to derive distribution of living states
    # <max.age> = assumed maximum age in years. Use the same scale as in <model>
    # <S> = number of replications in the estimation of the variance
    #       Choose S=0 for no variance estimation
    # <setseed> = the seed for the random number generation in the simulations
    # <RestrAndConst> = vector which indexes the independent parameters in 
    #        model$opt$par w.r.t. to the model parameters
    # <statedist.covariates> = names of covariates for model state prevalance
    # <method> = approximation of integral: "step" for simple step function;
    #         "MiddleRiemann" or "Simpson"
    
    # Covariate age has to be the first covariate in the list <b.covariates>.
    # Use the same order of covariates as in <model>
    # In the msm() model: center = FALSE, death = TRUE 
    
       
    
##########################
# Input checks:
if(model$center==TRUE){
    stop("\nERROR. LEs not computed. In msm() model use 
       argument <center = FALSE>.\n\n")
}
if(model$call$death!=TRUE){
    stop("\nERROR. LEs not computed. msm() model should be 
       illness-death model with the death state as final state with exact times.
       Use argument <death = TRUE>.\n\n")
}
if(model$call$formula[3]!="age()"){
    stop("\nERROR. LEs not computed. First covariate in msm() model 
       should be <age>.\n\n")
}
if(is.character(time.scale.msm)){
  if(!time.scale.msm%in%c("years","months","weeks")){
    stop("\nERROR. LEs not computed. Choose time scale to be <years>, 
       <months>, <weeks>, or a numeric value.\n\n")
  }
}
if(is.numeric(time.scale.msm)){  
  if(time.scale.msm<0 | 1<time.scale.msm){
    stop("\nERROR. LEs not computed. Numeric for time scale has to be between 0
       and 1.\n\n")
  }
}
if(h<=0){
    stop("\nERROR. LEs not computed. Choose h > 0.\n\n")
} 
if(round(S)!=S | S<0){
    stop("\nERROR. LEs not computed. <S> should be non-negative
       integer. Choose <S=0> for no simulations.\n\n")
}
if(age.max<=b.covariates$age){
    stop("\nERROR. LEs not computed. <max.age> should be larger than
       starting age in <b.covariates>.\n\n")
}
if(!method%in%c("step","Simpson","MiddleRiemann")){
    stop("\nERROR. LEs not computed. <method> should be <step>, <MiddleRiemann>
       or <Simpson>.\n\n")
} 
if(!"state"%in%names(statedistdata)){
    stop("\nERROR. LEs not computed. <state> should be 
       variable in <statedistdata>.\n\n")
}
if(!"age"%in%names(statedistdata)){
    stop("\nERROR. LEs not computed. <age> should be 
       variable in <statedistdata>.\n\n")
}
if(!"age"%in%statedist.covariates){
    stop("\nERROR. LEs not computed. <age> should be 
       covariate for model for state prevalence.\n\n")
}
for(i in 1:length(statedist.covariates)){
  if(!statedist.covariates[i]%in%names(statedistdata)){
   stop("\nERROR. LEs not computed. <",statedist.covariates[i],"> should 
       be variable in <statedistdata>.\n\n")
  }
}
if(length(statedist.covariates)>length(b.covariates)){
  stop("\nERROR. Number of covariates for model for state prevalence
       should not exceed number of covariates for msm model.\n\n")
}
if(is.null(model$covmat)){
  stop("\nERROR. Fitted model in msm has no covariance-variance matrix.
       Using ELECT is not recommended.\n\n")
}
 
#######################
# Scale parameter to take into account the time scale:
if(time.scale.msm=="years"){    scale <- 1}
if(time.scale.msm=="months"){   scale <- 12}
if(time.scale.msm=="weeks"){    scale <- 52} 
if(is.numeric(time.scale.msm)){ scale <- 1/time.scale.msm}

# Number of states and corresponding Q matrix:
nstates <- nrow(model$Qmatrices$baseline)
Q.null  <- matrix(as.numeric(model$Qmatrices$baseline!=0),nstates,nstates)
diag(Q.null) <- 0

# Number of modelled transitions:
ntrans <- sum(Q.null)

# Number of covariates (including intercept):
ncovs <- 1+length(b.covariates)

# Intensities parameters (ignoring parameters for misclassification model):
# (actually no. of parameters for transitions; may be different from no. betas)
nbeta <- max(which(names(model$estimates)=="qcov"))
beta  <- matrix(model$estimates[1:nbeta],ntrans,ncovs,byrow=FALSE)


# Baseline age:
age0 <- b.covariates$age
# Rest of the covariates if there are any:
if(length(b.covariates)>1){
   rest.covs <- as.vector( unlist(b.covariates[2:(ncovs-1)]))
}

# Grid for pwc hazards model and integral:
grid <- seq(0,age.max-age0,by=h)
# Shift grid for Middle Riemann rule:
if(method=="MiddleRiemann"){grid <- grid+h/2}

# Internal function for point estimates of LE:
msm.le.internal <- function(beta=beta){
  # Compute integrands:
  e <- matrix(NA,length(grid)-1,(nstates-1)^2)
  P <- diag(nstates)
  for(t in 2:length(grid)){
    # Q and P matrix:
    if(length(b.covariates)>1){
       cova <- c(1,age0+grid[t-1],rest.covs)
    }else{
       cova <- c(1,age0+grid[t-1])
    }
   # Compute Q matrix:
   Q <- Q.null
   index <- 1
   for(i in 1:(nstates-1)){
    for(j in 1:nstates){
      if(Q.null[i,j]){
        Q[i,j] <- exp(cova%*%beta[index,])
        index <- index+1
      }
    }
    Q[i,i] <- -sum(Q[i,])
   }
   # Compute P matrix:
   P <- P%*%MatrixExp(mat=Q,t=h)
   # Integrand:
   e[t-1,] <- as.vector(t(P[1:(nstates-1),1:(nstates-1)]))
  }
  # Expectancy in yrs using <method> for numerical approx to the integral:
  # Note the role of scale:
  LE <- rep(NA,(nstates-1)^2)
  # Simple grid approximation of integral:
  if(method=="step"){
       for(i in 1:length(LE)){LE[i] <- scale*sum(h*e[,i])}
  }
  # Middle Riemann sum (grid has been shifted already):
  if(method=="MiddleRiemann"){
      for(i in 1:length(LE)){LE[i] <- scale*sum(h*e[,i])}
  }
  # Simpson's rule:
  if(method=="Simpson"){
      # Work with even number of intervals (so with uneven number of nnodes):
      L <- length(grid)-1
      nnodes <- ifelse(round(L/2)!=L/2,L,L-1)
      # Do Simpson:
      for(i in 1:length(LE)){
        n <- nnodes-1
        adapt <- 1
        SS <- e[0+adapt,i] 
        for(j in seq(1,n/2-1,by=1)){  SS <- SS + 2 * e[2*j+adapt,i]} 
        for(j in seq(1,n/2,by=1)){    SS <- SS + 4 * e[2*j-1+adapt,i]} 
        S <- S+e[n+adapt,i]
        LE[i] <- scale*h*SS/3
      }
  }
  # Return:
  LE
}

# Estimate LEs given beta:
LE <- msm.le.internal(beta=beta)


####################################
# No marginal LEs if nstates > 5 (not yet implemented):
if(nstates>5){
  LE.pnt <- c(LE=LE)
  LEs <- NA
}


####################################
# Marginal LEs if nstates <= 5:
if(nstates<=5){

# Distribution of states (if needed for marginal LEs):
nstatesBaseline <- length(table(statedistdata$state))
# Use model for baseline states when more than one state:
if(nstatesBaseline>1){ 
  # Data:
  y  <- statedistdata$state-1
  # Build formula for prevalence model:
  formula.txt <- "y~"
  formula.txt <- paste(formula.txt,statedist.covariates[1],sep="")  
  n.sd.covars <- length(statedist.covariates)  
  if(n.sd.covars>1){
    for(i in 2:n.sd.covars){
     add.txt     <- paste("+", statedist.covariates[i],sep="")
     formula.txt <- paste(formula.txt,add.txt,sep="")        
    }
  }
  # print(formula.txt)
  # Multinomial regression model:
  sd.model <- multinom(formula=as.formula(formula.txt),
                       data=statedistdata,trace=F)
 
  # Define new data for prediction with prevalence model <bmodel>:
  newdata <- statedist.covariates[1:n.sd.covars]
  #print(newdata)

  # Specify values of covariates for prevalence model:
  sd.covars.values <- c(1, rep(NA,n.sd.covars) )
  for(i in 1:n.sd.covars){
    sd.covars.values[i+1] <- b.covariates[[i]]
  }
  #print(sd.covars.values)
  #print(summary(sd.model))
  
  # State distribution model: definitions for  later use:
  gamma  <- c(summary(sd.model)$coeff) 
  L2     <- length(gamma)
  SigmaG <- vcov(sd.model) 
  
}else{
 sd.model <- NA
}

####################################
# For simulation: point estimate and covariance for msm model:
# Remember: msm maximises 2LL. This is taken into account by the factor 1/2:
# If MC is fitted, remove the parameters for the MC model:
MCfitted <- any(names(model$opt$par)=="p")
if(!MCfitted){
  Sigma  <- solve(1/2*model$opt$hessian)
  mu     <- model$opt$par
  fixedpars <- model$fixedpars
}else{
  nparLE <-  max(which(names(model$opt$par)=="qcov"))
  Sigma  <- solve(1/2*model$opt$hessian)[1:nparLE,1:nparLE]
  mu     <- model$opt$par[1:nparLE]
  nfixed <- max(which(names(model$fixedpars)=="qcov"))
  fixedpars <- model$fixedpars[1:nfixed]
}
L <- length(mu)
 
##########################
##########################
# 3 States:
if(nstates==3){

# Marginal LEs:  
mLE <- rep(NA,nstates-1)
if(nstatesBaseline>1){
  lp <- gamma%*%sd.covars.values
  inits <- rep(NA,2)
  inits[1] <- 1/(1+exp(lp))
  inits[2] <- exp(lp)*inits[1] 
} 
mLE[1] <- inits%*%LE[c(1,3)] 
mLE[2] <- inits%*%LE[c(2,4)] 
tLE <- sum(mLE)

# Point estimate LEs:
LE.pnt <- c(LE=LE,mLE=mLE,tLE=tLE)

# No simulation:
if(S==0){LEs<-NA}

#######################################
# Estimated variance via ML simulation:
if(S>0){

# Set seed?:
if(!is.null(setseed)){set.seed(setseed)}

# Prelim:
LEs <- matrix(NA,S,length(LE.pnt))

# Simulation:
for(s in 1:S){ 
  # For msm: Sampling beta from a Multivariate Normal
  # Drawing univariate:
  z <- matrix(0,L,1)
  for(j in 1:L){z[j] <- rnorm(1,0,1)};
  # Multivariate draw:
  R <- chol(Sigma)
  p <- as.vector(mu+t(R)%*%z)
  # No restrictions:
  p0 <- p
  # Deal with fixed pars: 
  if(length(fixedpars)!=0 & is.null(RestrAndConst)){
       p0 <- rep(NA,nbeta)
       p0[fixedpars] <- 0
       p0[is.na(p0)] <- p
  }
  # Deal with contraints AND fixedpars: 
  if(!is.null(RestrAndConst)){  
      p0 <- length(p)
      for(i in 1:length(RestrAndConst)){
         if(RestrAndConst[i]==0){
           p0[i] <- 0
         }else{
           p0[i] <- p[RestrAndConst[i]]
         }
      }
    }   
  # Parameters:
  betab <- matrix(p0,ntrans,ncovs,byrow=FALSE)
  
  # For sd.model: Sampling gamma from a Multivariate Normal
  # if needed
  if(nstatesBaseline>1){
    # Drawing univariate:
    z <- matrix(0,L2,1)
    for(j in 1:L2){z[j] <- rnorm(1,0,1)};
    # Multivariate draw:
    R <- chol(SigmaG)
    gammag <- as.vector(gamma+t(R)%*%z)
    # Predict:
    lp <- gammag%*%sd.covars.values
    inits <- rep(NA,2)
    inits[1] <- 1/(1+exp(lp))
    inits[2] <- exp(lp)*inits[1]
  }
    
  # LEs:
  e <- msm.le.internal(beta=betab)
  e1 <- inits%*%e[c(1,3)]
  e2 <- inits%*%e[c(2,4)]
  LEs[s,] <- c(e,e1,e2,e1+e2)
}
}
}

###########################
###########################
# 4 States:
if(nstates==4){

# Marginal LEs:  
mLE <- rep(NA,nstates-1)
if(nstatesBaseline>1){
   # Derive inits probs:
   gamma.index <- seq(1,2*length(sd.covars.values),by=2)
   lp1 <- gamma[gamma.index]%*%sd.covars.values
   lp2 <- gamma[gamma.index+1]%*%sd.covars.values
   inits <- rep(NA,3)
   inits[1] <- 1/(1+exp(lp1)+exp(lp2))
   inits[2] <- exp(lp1)*inits[1]
   inits[3] <- exp(lp2)*inits[1]
}else{
   inits <- c(1,0,0)
}
mLE[1] <- inits%*%LE[c(1,4,7)]
mLE[2] <- inits%*%LE[c(2,5,8)]
mLE[3] <- inits%*%LE[c(3,6,9)]
tLE    <- sum(mLE)

# Point estimate LEs:
LE.pnt <- c(LE=LE,mLE=mLE,tLE=tLE)

# No simulation:
if(S==0){LEs<-NA}

#######################################
# Estimated variance via ML simulation:
if(S>0){

# Set seed:
set.seed(setseed)

# Prelim:
LEs <- matrix(NA,S,length(LE.pnt))

# Simulation:
for(s in 1:S){ 
  # For msm: Sampling beta from a Multivariate Normal
  # Drawing univariate:
  z <- matrix(0,L,1)
  for(j in 1:L){z[j] <- rnorm(1,0,1)};
  # Multivariate draw:
  R <- chol(Sigma)
  p <- as.vector(mu+t(R)%*%z)
  # No restrictions:
  p0 <- p
  # Deal with fixed pars:
  if(length(fixedpars)!=0 & is.null(RestrAndConst)){
   p0 <- rep(NA,nbeta)
   p0[fixedpars] <- 0
   p0[is.na(p0)] <- p 
  }
  # Deal with contraints AND fixed pars: 
  if(!is.null(RestrAndConst)){
      p0 <- length(p)
      for(i in 1:length(RestrAndConst)){
         if(RestrAndConst[i]==0){
           p0[i] <-0 
         }else{
           p0[i] <- p[RestrAndConst[i]]
         }
      }
    }  
    
  # Parameters:
  betab <- matrix(p0,ntrans,ncovs,byrow=FALSE)
        
  if(nstatesBaseline>1){
   # For sd.model: Sampling gamma from a Multivariate Normal
   # Drawing univariate:
   z <- matrix(0,L2,1)
   for(j in 1:L2){ z[j] <- rnorm(1,0,1) }
   # Multivariate draw:
   R <- chol(SigmaG)
   gammag <- as.vector(gamma+t(R)%*%z)
   # Derive inits probs:
   lp1 <- gammag[gamma.index]%*%sd.covars.values
   lp2 <- gammag[gamma.index+1]%*%sd.covars.values
   inits <- rep(NA,3)
   inits[1] <- 1/(1+exp(lp1)+exp(lp2))
   inits[2] <- exp(lp1)*inits[1]
   inits[3] <- exp(lp2)*inits[1]
  } 
  
  # LEs:
  e <- msm.le.internal(beta=betab)
  e1 <- inits%*%e[c(1,4,7)]
  e2 <- inits%*%e[c(2,5,8)]
  e3 <- inits%*%e[c(3,6,9)]
  LEs[s,] <- c(e,e1,e2,e3,e1+e2+e3)
}
}
}

###########################
###########################
# 5 States:
if(nstates==5){
  
  # Marginal LEs:  
  mLE <- rep(NA,nstates-1)
  if(nstatesBaseline>1){
   # Derive inits probs:
   gamma.index <- seq(1,3*length(sd.covars.values),by=3)
   lp1 <- gamma[gamma.index]%*%sd.covars.values
   lp2 <- gamma[gamma.index+1]%*%sd.covars.values
   lp3 <- gamma[gamma.index+2]%*%sd.covars.values
   inits <- rep(NA,4)
   inits[1] <- 1/(1+exp(lp1)+exp(lp2)+exp(lp3))
   inits[2] <- exp(lp1)*inits[1]
   inits[3] <- exp(lp2)*inits[1]
   inits[4] <- exp(lp3)*inits[1]
  }else{
   inits <- c(1,0,0,0)
  }
  inits.index <- c(1,5,9,13)
  mLE[1] <-inits%*%LE[inits.index]
  mLE[2] <-inits%*%LE[inits.index+1]
  mLE[3] <-inits%*%LE[inits.index+2]
  mLE[4] <-inits%*%LE[inits.index+3]
  tLE <- sum(mLE)
  # Point estimate LEs:
  LE.pnt <- c(LE=LE,mLE=mLE,tLE=tLE)

  # No simulation:
  if(S==0){LEs <- NA}

  #######################################
  # Simulation:
  # Estimated variance via ML simulation:
  if(S>0){

  # Set seed:
  set.seed(setseed)

  # Prelim:
  LEs<-matrix(NA,S,length(LE.pnt))

  # Simulation:
  for(s in 1:S){ 
    # For msm: Sampling beta from a Multivariate Normal
    # Drawing univariate:
    z <-matrix(0,L,1)
    for(j in 1:L){z[j] <- rnorm(1,0,1)};
    # Multivariate draw:
    R <- chol(Sigma)
    p <- as.vector(mu+t(R)%*%z)
    # No restrictions:
    p0 <- p
    # Deal with fixed pars: 
    if(length(fixedpars)!=0 & is.null(RestrAndConst)){
       p0 <- rep(NA,nbeta)
       p0[fixedpars] <- 0
       p0[is.na(p0)] <- p
    }
    # Deal with contraints AND fixed pars: 
    if(!is.null(RestrAndConst)){
      p0 <- length(p)
      for(i in 1:length(RestrAndConst)){
         if(RestrAndConst[i]==0){
           p0[i] <-0
         }else{
           p0[i] <- p[RestrAndConst[i]]
         }
      }
    }   
  
   # Parameters:
   betab <- matrix(p0,ntrans,ncovs,byrow=FALSE)
        
  if(nstatesBaseline>1){
   # For sd.model: Sampling gamma from a Multivariate Normal
   # Drawing univariate:
   z <- matrix(0,L2,1)
   for(j in 1:L2){z[j] <- rnorm(1,0,1)};
   # Multivariate draw:
   R <- chol(SigmaG)
   gammag <- as.vector(gamma+t(R)%*%z)
   # Derive inits probs:
   lp1 <- gammag[gamma.index]%*%sd.covars.values
   lp2 <- gammag[gamma.index+1]%*%sd.covars.values
   lp3 <- gammag[gamma.index+2]%*%sd.covars.values
   inits <- rep(NA,4)
   inits[1] <- 1/(1+exp(lp1)+exp(lp2)+exp(lp3))
   inits[2] <- exp(lp1)*inits[1]
   inits[3] <- exp(lp2)*inits[1]
   inits[4] <- exp(lp3)*inits[1]
  }
  # LEs:
  e  <- msm.le.internal(beta=betab)
  e1 <- inits%*%e[inits.index]
  e2 <- inits%*%e[inits.index+1]
  e3 <- inits%*%e[inits.index+2]
  e4 <- inits%*%e[inits.index+3]
  LEs[s,] <- c(e,e1,e2,e3,e4,e1+e2+e3+e4)
}
}
}

}

# Names:
if(nstates==3){names(LE.pnt)<-c("e11","e12","e21","e22","e.1","e.2","e")}

if(nstates==4){names(LE.pnt)<-c("e11","e12","e13","e21","e22","e23",
                                "e31","e32","e33","e.1","e.2","e.3","e")}
if(nstates==5){names(LE.pnt)<-c("e11","e12","e13","e14",
                                "e21","e22","e23","e24",
                                "e31","e32","e33","e34",
                                "e41","e42","e43","e44",
                                "e.1","e.2","e.3","e.4","e")}

# Function returns list with R objects:
list(pnt=LE.pnt,sim=LEs,h=h,covars=b.covariates,S=S,sd.model=sd.model)
}
   

#######################################################
# Function to summarise life.exp results:
summary.elect<-function(LEs,probs=c(.025,0.5,.975),digits=3,
                        StartStateTotals=FALSE, print=TRUE, sd.model=FALSE){
   # <LEs> = result from life.exp()
   # <probs> = probs for quantiles
   # <digits> = number of digits in output
   # <StartStateTotals> = TRUE for output on start-state totals e_r (for S>0).
   # <print> = TRUE of output directly to screen
    
   # Use point estimate:
   pnt <- LEs$pnt
   
   # Was simulation undertaken?:
   if(is.na(LEs$sim[1])){ SIM <- FALSE }else{ SIM <- TRUE }
       
   # Use simulation if it took place:
   if(!SIM){ out <- as.data.frame(pnt)}
   if(SIM){ 
     mn <- apply(LEs$sim,2,mean)
     se <- apply(LEs$sim,2,sd)
     quants <- matrix(NA,ncol(LEs$sim),length(probs))
     for(i in 1:ncol(LEs$sim)){
       for(j in 1:length(probs)){
        quants[i,j] <- quantile(LEs$sim[,i],probs=probs[j])
       }   
     }     
     out <- as.data.frame(cbind(pnt,mn,se,quantiles=quants))
     for(j in 4:(3+length(probs))){
       names(out)[j]<-paste(probs[j-3],"q",sep="")
     }
   }
   ##########################
   # 3-state model:
   # Produce start-state totals if asked for:
   if(SIM && StartStateTotals && ncol(LEs$sim)==7){
     summ <- cbind(LEs$sim[,1]+LEs$sim[,2],LEs$sim[,3]+LEs$sim[,4])
     mn <- apply(summ,2,mean)
     se <- apply(summ,2,sd)
     quants <- matrix(NA,ncol(summ),length(probs))
     for(i in 1:ncol(summ)){
       for(j in 1:length(probs)){
            quants[i,j] <- quantile(summ[,i],probs=probs[j])
        }   
      }     
     pnt2 <- c(LEs$pnt[1]+LEs$pnt[2],LEs$pnt[3]+LEs$pnt[4]) 
     out2 <- as.data.frame(cbind(pnt=pnt2,mn,se,quantiles=quants))
     for(j in 4:(3+length(probs))){
          names(out2)[j] <- paste(probs[j-3],"q",sep="")
        }
      row.names(out2) <- c("e1.","e2.")
     }
    
   ##########################
   # 4-state model:
   # Produce start-state totals if asked for:
   if(SIM && StartStateTotals && ncol(LEs$sim)==13){
     summ <- LEs$sim[,1:9]
     mn <- apply(summ,2,mean)
     se <- apply(summ,2,sd)
     quants <- matrix(NA,ncol(summ),length(probs))
     for(i in 1:ncol(summ)){
       for(j in 1:length(probs)){
            quants[i,j] <- quantile(summ[,i],probs=probs[j])
        }   
      }     
     pnt2 <- c(sum(LEs$pnt[1:3]),sum(LEs$pnt[4:6]),sum(LEs$pnt[7:9])) 
     out2 <- as.data.frame(cbind(pnt=pnt2,mn,se,quantiles=quants))
     for(j in 4:(3+length(probs))){
          names(out2)[j] <- paste(probs[j-3],"q",sep="")
        }
      row.names(out2) <- c("e1.","e2.","e3.")
     }
     
   ##########################
   # 5-state model:
   # Produce start-state totals if asked for:
   if(SIM && StartStateTotals && ncol(LEs$sim)==21){
     summ <- LEs$sim[,1:16]
     mn <- apply(summ,2,mean)
     se <- apply(summ,2,sd)
     quants <- matrix(NA,ncol(summ),length(probs))
     for(i in 1:ncol(summ)){
       for(j in 1:length(probs)){
            quants[i,j] <- quantile(summ[,i],probs=probs[j])
        }   
      }     
     pnt2 <- c(LEs$pnt[1:1]+LEs$pnt[2],LEs$pnt[3]+LEs$pnt[4]) 
     out2 <- as.data.frame(cbind(pnt=pnt2,mn,se,quantiles=quants))
     for(j in 4:(3+length(probs))){
          names(out2)[j] <- paste(probs[j-3],"q",sep="")
        }
      row.names(out2) <- c("e1.","e2.")
     }
      
    
    # To print or not to print:
    if(print){
     cat("\n-----------------------------\n") 
     cat("ELECT summary\n")
     cat("-----------------------------\n")  
     cat("Covariates values in the multi-state model:\n")
     print(unlist(LEs$covars))
     # Output the sd.model if asked for (and if fitted):
     sd.model.fitted <-  !(is.na(LEs$sd.model)[1])
     if(sd.model.fitted & sd.model==FALSE){
       # Output covariates used in sd.model:
       sd.covars <- attr(LEs$sd.model$terms,"term.labels")
       cat("Covariates in the state-distribution model:\n  ",sd.covars,"\n\n") 
     }
     if(sd.model.fitted & sd.model==TRUE){
       # Output summary of sd.model:
       cat("\nFitted model for state-distribution model:\n")
       print(summary(LEs$sd.model))
       cat("\n")
     }
     if(!sd.model.fitted & sd.model==TRUE){
       # Output info on no sd.model:
       cat("\nNo state-distribution model was fitted.\n")
     }
     cat("Life expectancies:")
     if(LEs$S>0){
      cat("Using simulation with ",LEs$S,"replications\n")
      cat("\nPoint estimates, and mean, SEs, and quantiles from simulation:\n")
    }else{cat("\nPoint estimates:\n")}
    print(round(out,digits))
    cat("-----------------------------\n")
    # Add totals for baselinestate to output if asked for:
    if(ncol(out)>1){
      if(StartStateTotals & ncol(LEs$sim)==7){
        cat("\nStart-state totals:\n")
        print(round(out2,digits))
        # Extend output:
        out <- rbind(out,out2)}
       }
    }
} 

#######################################################
# Function to plot life.exp results:
plot.elect<-function(LEs,which=NULL,kernel="gaussian",col="red",lwd=2,cex.lab=1){
   
   ##############################################
   # <LEs> = result from elect()
   # <which> = select which of the LEs to plot (order as in summary)
   # <col> = colour for lines
   # <lwd> = lwd for lines
   
   # Plot only if S >0:
   if(LEs$S>0){
   
   ##########################
   # 3-state model:
   if(length(LEs$pnt)==7){
    if(is.null(which)){
     # Without selection:
     opar<-par(mfrow=c(2,4), mex=0.8,mar=c(5,5,2,1)+.1)
     for(i in 1:7){
      plot(density(LEs$sim[,i],kernel=kernel),main=names(LEs$pnt[i]),
          ylab="Density",xlab="Years",col=col,lwd=lwd,cex.lab=cex.lab)
     }
     opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1)+.1)
    }else{  
      # With selection via <which>:
      if(length(which)<= 3){mfrow=c(1,length(which))}
      if(length(which)==4){mfrow=c(2,2)}
      if(length(which)%in%c(5,6)){mfrow=c(2,3)}
      if(length(which)==7){mfrow=c(2,4)}
      opar<-par(mfrow=mfrow, mex=0.8,mar=c(5,5,2,1)+.1)
      for(i in which){
       label <- as.character(i)
       plot(density(LEs$sim[,i],kernel=kernel),main=names(LEs$pnt[i]),
          ylab="Density",xlab="Years",col=col,lwd=lwd,cex.lab=cex.lab)
      }
      opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1)+.1)
     }
   }
   
   ##########################
   # 4-state model:
   if(length(LEs$pnt)==13){
   opar<-par(mfrow=c(3,5), mex=0.8,mar=c(5,5,2,1)+.1)
   for(i in 1:13){
     plot(density(LEs$sim[,i],kernel=kernel),main=names(LEs$pnt[i]),
                  ylab="density",xlab="years",col=col,lwd=lwd)
   }
   opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1)+.1)
   }
   }else{cat("\nNo simulated LEs so no plot. \n\n")}
   
   ###########################
   # 5-state model:
   if(length(LEs$pnt)==21){
   	opar<-par(mfrow=c(3,7), mex=0.8,mar=c(5,5,2,1)+.1)
   for(i in 1:21){
     plot(density(LEs$sim[,i],kernel=kernel),main=names(LEs$pnt[i]),
          ylab="density",xlab="years",col=col,lwd=lwd)
   }
   opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1)+.1)
   }
      
   # No plot without simulation:
   if(LEs$S==0){cat("\nNo simulated LEs so no plot. \n\n")}
} 


#######################################################
# Function to check definition of <RestrAndConst>:

check.RestrAndConst.elect <- function(model, RestrAndConst, PRINT=FALSE){
  # Model parameters (for transitions):
  nbeta <- max(which(names(model$estimates)=="qcov"))
  p <- model$estimates[1:nbeta]
  
  # Check:
  p.RestrAndConst <- rep(NA,length(p))
  for(i in 1:length(RestrAndConst)){
     if(RestrAndConst[i]==0){
       p.RestrAndConst[i] <- 0
     }else{
       p.RestrAndConst[i] <- model$opt$par[RestrAndConst[i]]
     }
   }
   if(PRINT){
     cat("\nCheck of definition <RestrAndConst>:\n\n")
     print(cbind(Model.parameters=p,RestrAndConst.parameters=p.RestrAndConst))
   }
   
   if(any(is.na(p== p.RestrAndConst))){ 
      OK <- FALSE
   }else{
      OK <- all(p== p.RestrAndConst)
   }
   if(OK){
      cat("\n<RestrAndConst> correctly defined.\n")
   }else{
      cat("\n<RestrAndConst> not correctly defined.\n")
      if(!PRINT){cat("Use PRINT=TRUE to see the discrepancies.\n")}
      cat("\n")
   }
   return(OK)
}