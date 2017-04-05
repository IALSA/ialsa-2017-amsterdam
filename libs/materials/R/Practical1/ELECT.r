# Functions to estimate LEs given an msm() model in R
# Version 0.1.3

# Ardo, Cambridge 2010 - UCL 2015

# Prelim:
library(nnet)

################################################
################################################
# ELECT function:
elect<-function(model, b.covariates, statedistdata, time.scale.msm="years",
                 h, age.max, S=0, setseed=NULL, RestrAndConst=NULL){
    
    # Explanation:
    # <model> = estimated msm() model 
    # <b.covariates> = list with specified covariates values (ignore intercept) 
    # <time.scale.msm> = time scale in msm() model ("years","months", "weeks") 
    # <h> = grid parameter for integration and 
    #       piecewise-constant model. In scale given by <time.scale>
    # <statedistdata> = the msm data to derive distribution of living states
    # <max.age> = assumed maximum age in years. Use the same scale as in <model>
    # <S> = number of replications in the estimation of the variance
    #       Choose S=0 for no variance estimation
    # <setseed> = the seed for the random number generation in the simulations
    # <RestrAndConst> = vector which indexes the independent parameters in 
    #        model$opt$par w.r.t. to the model parameters
     
    # Covariate age has to be the first covariate in the list <b.covariates>.
    # Use the same order of covariates as in <model>
    # In the msm() model: center = FALSE, death = TRUE 
    
 
    # W.I.P.: models with quadratic terms. For now <quadratic> = FALSE:
    quadratic<-FALSE
    # If there is a quadratic term for age, then this should be the 2nd covariate 
    # but the term should not be added to the list <b.covariates>
    
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
if(!time.scale.msm%in%c("years","months","weeks")){
    stop("\nERROR. LEs not computed. Choose time scale to be <years>, 
    <months>, or <weeks>.\n\n")
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
 
#######################
# Scale parameter to take into account the time scale:
if(time.scale.msm=="years"){scale<-1}
if(time.scale.msm=="months"){scale<- 1/12}
if(time.scale.msm=="weeks"){scale<-1/52} 

# Number of states and corresponding Q matrix:
nstates<-nrow(model$Qmatrices$baseline)
Q.null<-matrix(as.numeric(model$Qmatrices$baseline!=0),nstates,nstates)
diag(Q.null)<-0

# Number of modelled transitions:
ntrans<-sum(Q.null)

# Number of covariates (including intercept,  and the quadratic term if TRUE):
if(quadratic){ncovs<- 2+length(b.covariates)}else{ncovs<- 1+length(b.covariates)}

# Intensities parameters:
nbeta<-max(which(names(model$estimates)=="qcov"))
beta<-matrix(model$estimates[1:nbeta],ntrans,ncovs,byrow=FALSE)

# Baseline age:
age0<-b.covariates$age
# Rest of the covariates if there are any:
if(length(b.covariates)>1){
   rest.covs<- as.vector( unlist(b.covariates[2:(ncovs-1)]))
}

# Grid for pwc hazards model and integral:
grid<-seq(0,age.max-age0,by=h*scale)

# Internal function for point estimates of LE:
msm.le.internal<-function(beta=beta){
  # Compute integrands:
  e<-matrix(NA,length(grid)-1,(nstates-1)^2)
  P<-diag(nstates)
  for(t in 2:length(grid)){
    # Q and P matrix:
    # For quadratic model:
    if(quadratic){
     if(length(b.covariates)>1){
       cova<-c(1,age0+grid[t-1],(age0+grid[t-1])^2,rest.covs)
     }else{ cova<-c(1,age0+grid[t-1],(age0+grid[t-1])^2) }
    }else{
   # For model without quadratic term:
    if(length(b.covariates)>1){cova<-c(1,age0+grid[t-1],rest.covs)
     }else{ cova<-c(1,age0+grid[t-1]) }
   }
   Q<-Q.null
   index<-1
   for(i in 1:(nstates-1)){
    for(j in 1:nstates){
      if(Q.null[i,j]){
        Q[i,j]<- exp(cova%*%beta[index,])
        index<-index+1
      }
    }
    Q[i,i]<- -sum(Q[i,])
   }
   P<-P %*% MatrixExp(mat=Q,t=h)
   # Integrand:
   e[t-1,]<-as.vector(t(P[1:(nstates-1),1:(nstates-1)]))
  }
  # Expectancy in yrs using the simple numerical approx to the integral. 
  # Note the role of scale:
  LE<-rep(NA,(nstates-1)^2)
  for(i in 1:length(LE)){LE[i]<- sum((h*scale)*e[,i])}
  # Return:
  LE
}

# Estimate LEs given beta:
LE<-msm.le.internal(beta=beta)


####################################
# No marginal LEs if nstates > 4 (not yet implemented):
if(nstates>5){
  LE.pnt<-c(LE=LE)
  LEs<-NA
}

####################################
# Marginal LEs if nstates <= 5:
if(nstates<=5){

# Check if state is a variable in data for states:
if(!"state"%in%names(statedistdata)){
    stop("\nERROR. LEs not computed. <state> should be 
           variable <statedistdata>.\n\n")
}
  
# Distribution of states (when needed for marginal LEs):
nstatesBaseline<-length(table(statedistdata$state))
# Use model for baseline states when more than one state:
if(nstatesBaseline>1){
  # For the moment only using age!
  # Check if age is a variable in data for states:
  if(!"age"%in%names(statedistdata)){
      stop("\nERROR. LEs not computed. <age> should 
            be variable <statedistdata>.\n\n")
  }
  # Data:
  y<-statedistdata$state-1
  x1<-statedistdata$age
  # Multinomial regression model:
  bmodel<-multinom(y~x1,trace=F)
}

##########################
##########################
# 3 States:
if(nstates==3){

# Marginal LEs:  
mLE<-rep(NA,nstates-1)
inits<-rep(NA,nstates-1)
if(nstatesBaseline>1){
  inits[2]<-predict(bmodel,newdata=list(x1=age0),type="probs")
}else{
  inits[2]<-0
} 
inits[1]<-1-inits[2] 
mLE[1]<-inits[1]*LE[1]+inits[2]*LE[3]
mLE[2]<-inits[1]*LE[2]+inits[2]*LE[4]
tLE<-sum(mLE)

# Point estimate LEs:
LE.pnt<-c(LE=LE,mLE=mLE,tLE=tLE)

# No simulation:
if(S==0){LEs<-NA}

#######################################
# Estimated variance via ML simulation:
if(S>0){

# Set seed?:
if(!is.null(setseed)){set.seed(setseed)}

# Covariance estimates for msm and logistic regression model:
# Remember: msm maximises 2LL. This is taken into account by the factor 1/2:
Sigma<- solve(1/2*model$opt$hessian)
mu<-model$opt$par
L<-length(mu)
gamma<-c(summary(bmodel)$coeff)
SigmaG<-vcov(bmodel) 
L2<-length(gamma)

# Prelim:
LEs<-matrix(NA,S,length(LE.pnt))

# Simulation:
for(s in 1:S){ 
  # For msm: Sampling beta from a Multivariate Normal
  # Drawing univariate:
  z<-matrix(0,L,1)
  for(j in 1:L){z[j]<-rnorm(1,0,1)};
  # Multivariate draw:
  R<-chol(Sigma)
  p<-as.vector(mu+t(R)%*%z)
  # Back to orginal format for parameters if some are fixed to zero:
  if(length(model$fixedpars)!=0){
   p0<-rep(NA,length(model$estimates))
   p0[model$fixedpars]<-0; p0[is.na(p0)]<-p
   p<-p0
  }
  # Parameters:
  betab<-matrix(p,ntrans,ncovs,byrow=FALSE)
  
  # For b.model: Sampling gamma from a Multivariate Normal
  # Drawing univariate:
  z<-matrix(0,L2,1)
  for(j in 1:L2){z[j]<-rnorm(1,0,1)};
  # Multivariate draw:
  R<-chol(SigmaG)
  gammag<-as.vector(gamma+t(R)%*%z)
    
  # LEs:
  e<-msm.le.internal(beta=betab)
  lp<-gammag[1]+gammag[2]*age0
  inits2<-exp(lp)/(1+exp(lp))
  inits1<-1-inits2  
  e1<-inits1*e[1]+inits2*e[3]
  e2<-inits1*e[2]+inits2*e[4]
  LEs[s,]<-c(e,e1,e2,e1+e2)
}
}
}

###########################
###########################
# 4 States:
if(nstates==4){

# Marginal LEs:  
mLE<-rep(NA,nstates-1)
if(nstatesBaseline>1){
   inits<-predict(bmodel,newdata=list(x1=age0),type="probs")
}else{
   inits<-c(1,0,0)
}
mLE[1]<-inits%*%LE[c(1,4,7)]
mLE[2]<-inits%*%LE[c(2,5,8)]
mLE[3]<-inits%*%LE[c(3,6,9)]
tLE<-sum(mLE)

# Point estimate LEs:
LE.pnt<-c(LE=LE,mLE=mLE,tLE=tLE)

# No simulation:
if(S==0){LEs<-NA}

#######################################
# Estimated variance via ML simulation:
if(S>0){

# Set seed:
set.seed(setseed)

if(length(model$fixedpars)!=0 ){
   fixedpars.q<-model$fixedpars[1:max(which(names(model$fixedpars)=="qcov"))]
   n.indpar<-nbeta-length(fixedpars)
}else{n.indpar<-nbeta}


# Covariance estimates for msm and (when applicable) logistic regression model:
# Remember: msm maximises 2LL. This is taken into account by the factor 1/2:
Sigma<- solve(1/2*model$opt$hessian)[1:n.indpar,1:n.indpar]
mu<-model$opt$par[1:n.indpar]
L<-length(mu)
if(nstatesBaseline>1){
 gamma<-c(matrix(t(summary(bmodel)$coeff),1,4))
 SigmaG<-vcov(bmodel) 
 L2<-length(gamma)
}
# Prelim:
LEs<-matrix(NA,S,length(LE.pnt))

# Simulation:
for(s in 1:S){ 
  # For msm: Sampling beta from a Multivariate Normal
  # Drawing univariate:
  z<-matrix(0,L,1)
  for(j in 1:L){z[j]<-rnorm(1,0,1)};
  # Multivariate draw:
  R<-chol(Sigma)
  p<-as.vector(mu+t(R)%*%z)
  # Deal with #indpendent parameters:
  # Fixed pars:
  if(length(model$fixedpars)!=0){
   p0<-rep(NA,nbeta)
   p0[fixedpars]<-0; p0[is.na(p0)]<-p
   p<-p0
  }
    
  # Parameters:
  betab<-matrix(p,ntrans,ncovs,byrow=FALSE)
        
  if(nstatesBaseline>1){
   # For b.model: Sampling gamma from a Multivariate Normal
   # Drawing univariate:
   z<-matrix(0,L2,1)
   for(j in 1:L2){z[j]<-rnorm(1,0,1)};
   # Multivariate draw:
   R<-chol(SigmaG)
   gammag<-as.vector(gamma+t(R)%*%z)
   # Derive inits probs:
   lp1<-gammag[1]+gammag[3]*age0
   lp2<-gammag[2]+gammag[4]*age0
   inits<-rep(NA,3)
   inits[1]<-1/(1+exp(lp1)+exp(lp2))
   inits[2]<-exp(lp1)*inits[1]
   inits[3]<-exp(lp2)*inits[1]
  }
  # LEs:
  e<-msm.le.internal(beta=betab)
  e1<-inits%*%e[c(1,4,7)]
  e2<-inits%*%e[c(2,5,8)]
  e3<-inits%*%e[c(3,6,9)]
  LEs[s,]<-c(e,e1,e2,e3,e1+e2+e3)
}
}
}


###########################
###########################
# 5 States:
if(nstates==5){
  
  # Marginal LEs:  
  mLE<-rep(NA,nstates-1)
  if(nstatesBaseline>1){
   inits<-predict(bmodel,newdata=list(x1=age0),type="probs")
  }else{
   inits<-c(1,0,0,0)
  }
  inits.index<- c(1,5,9,13)
  mLE[1]<-inits%*%LE[inits.index]
  mLE[2]<-inits%*%LE[inits.index+1]
  mLE[3]<-inits%*%LE[inits.index+2]
  mLE[4]<-inits%*%LE[inits.index+3]
  tLE<-sum(mLE)
  # Point estimate LEs:
  LE.pnt<-c(LE=LE,mLE=mLE,tLE=tLE)

  # No simulation:
  if(S==0){LEs<-NA}

  #######################################
  # Simulation:
  # Estimated variance via ML simulation:
  if(S>0){

  # Set seed:
  set.seed(setseed)

  # Derive number of independent parameters for transition intensities:
  n.indpar<-length(model$opt$par)
  
  # Covariance estimates for msm and logistic regression model:
  # Remember: msm maximises 2LL, taken into account by the factor 1/2:
  Sigma<- solve(1/2*model$opt$hessian)[1:n.indpar,1:n.indpar]
  mu<-model$opt$par[1:n.indpar]
  L<-length(mu)
  if(nstatesBaseline>1){
    gamma<-c(summary(bmodel)$coeff)
    SigmaG<-vcov(bmodel) 
    L2<-length(gamma)
   }
  # Prelim:
  LEs<-matrix(NA,S,length(LE.pnt))

  # Simulation:
  for(s in 1:S){ 
    # For msm: Sampling beta from a Multivariate Normal
    # Drawing univariate:
    z<-matrix(0,L,1)
    for(j in 1:L){z[j]<-rnorm(1,0,1)};
    # Multivariate draw:
    R<-chol(Sigma)
    p<-as.vector(mu+t(R)%*%z)
    # Deal with fixed pars: 
    if(length(model$fixedpars)!=0 & is.null(RestrAndConst)){
       p0<-rep(NA,nbeta)
       p0[fixedpars]<-0
       p0[is.na(p0)]<-p
       p <- p0
    }
    # Deal with contraints AND fixed pars: 
    if(!is.null(RestrAndConst)){
      p0<-length(p)
      for(i in 1:length(RestrAndConst)){
         if(RestrAndConst[i]==0){
           p0[i]<-0
         }else{
           p0[i]<-p[RestrAndConst[i]]
         }
      }
      p <- p0
    }   
  
   # Parameters:
   betab<-matrix(p,ntrans,ncovs,byrow=FALSE)
        
  if(nstatesBaseline>1){
   # For b.model: Sampling gamma from a Multivariate Normal
   # Drawing univariate:
   z<-matrix(0,L2,1)
   for(j in 1:L2){z[j]<-rnorm(1,0,1)};
   # Multivariate draw:
   R<-chol(SigmaG)
   gammag<-as.vector(gamma+t(R)%*%z)
   # Derive inits probs:
   lp1<-gammag[1]+gammag[4]*age0
   lp2<-gammag[2]+gammag[5]*age0
   lp3<-gammag[3]+gammag[6]*age0
   inits<-rep(NA,4)
   inits[1]<-1/(1+exp(lp1)+exp(lp2)+exp(lp3))
   inits[2]<-exp(lp1)*inits[1]
   inits[3]<-exp(lp2)*inits[1]
   inits[4]<-exp(lp3)*inits[1]
  }
  # LEs:
  e<-msm.le.internal(beta=betab)
  e1<-inits%*%e[inits.index]
  e2<-inits%*%e[inits.index+1]
  e3<-inits%*%e[inits.index+2]
  e4<-inits%*%e[inits.index+3]
  LEs[s,]<-c(e,e1,e2,e3,e4,e1+e2+e3+e4)
}
}
}

}

# Names:
if(nstates==3){names(LE.pnt)<-c("e11","e12","e21","e22","e1","e2","e")}
if(nstates==4){names(LE.pnt)<-c("e11","e12","e13","e21","e22","e23",
                                "e31","e32","e33","e1","e2","e3","e")}
if(nstates==5){names(LE.pnt)<-c("e11","e12","e13","e14",
                                "e21","e22","e23","e24",
                                "e31","e32","e33","e34",
                                "e41","e42","e43","e44",
                                "e1","e2","e3","e4","e")}

# Out:
list(pnt=LE.pnt,sim=LEs,h=h,covars=b.covariates,S=S)
}
   

#######################################################
# Function to summarise life.exp results:
summary.elect<-function(LEs=LEs,probs=c(.025,0.5,.975),digits=3, print=TRUE){
   # <LEs> = result from life.exp()
   # <probs> = probs for quantiles
   # <digits> = number of digits in output
   # <print> = TRUE of output directly to screen
   
   pnt<-LEs$pnt
   if(is.null(nrow(LEs$sim))){
    out<-as.data.frame(pnt)
   }else{
     mn<-apply(LEs$sim,2,mean)
     se<-apply(LEs$sim,2,sd)
     quants<-matrix(NA,ncol(LEs$sim),length(probs))
     for(i in 1:ncol(LEs$sim)){
       for(j in 1:length(probs)){
        quants[i,j]<-quantile(LEs$sim[,i],probs=probs[j])
       }   
     }     
     out<-as.data.frame(cbind(pnt,mn,se,quantiles=quants))
     for(j in 4:(3+length(probs))){
       names(out)[j]<-paste(probs[j-3],"q",sep="")
     }
   }
   # To print or not to print:
   if(print){
    cat("\nFor covariates values specified as:\n"); print(unlist(LEs$covars))
    if(LEs$S>0){
      cat("Using simulation with ",LEs$S,"replications\n")
      cat("\nPoint estimates, and mean, SEs, and quantiles from simulation:\n")
    }else{cat("\nPoint estimates:\n")}
    print(round(out,digits))
   }else{return(out)}
} 

#######################################################
# Function to plot life.exp results:
plot.elect<-function(LEs=LEs,kernel="gaussian",col="red",lwd=2,cex.lab=1){
   # <LEs> = result from life.exp
   # <col> = colour for lines
   # <lwd> = lwd for lines
   
   if(LEs$S>0){
   ##########################
   # 3-state model:
   if(length(LEs$pnt)==7){
   opar<-par(mfrow=c(2,4), mex=0.8,mar=c(5,5,2,1)+.1)
   for(i in 1:7){
     plot(density(LEs$sim[,i],kernel=kernel),main=names(LEs$pnt[i]),
          ylab="Density",xlab="Years",col=col,lwd=lwd,cex.lab=cex.lab)
   }
   opar<-par(mfrow=c(1,1), mex=0.8,mar=c(5,5,2,1)+.1)
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


