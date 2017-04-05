# Simulating data 

# Ardo, 2016

# Set up using:
#  - Fix parameters 
#  - Simulate exact transition times
#  - Embed exact times in observed times 
#  - Check data for very short time intervals and tweak
#  - Fit model to check simulation

# Prelim:
library(msm)
digits<-3
set.seed(12345)

# True parameter vector:
p.true<-c(-3.5, -4.0, -2, -3,  0.015, 0.01, 0,  0.015, 0.5, 0, 0, 0)
beta<-cbind(p.true[1:4],p.true[5:8],p.true[9:12])

# Baseline data:
N<-200
cat("Sample size is ",N,"\n")
cat("Creating baseline data...\n")
b.data<-as.data.frame(cbind(id=1:N,time=0,state=NA,age=NA,x=NA))
b.data$age<-round(rtnorm(N,mean=75,sd=5,lower=65,upper=90),1)
b.data$x<- rbinom(N,size=1,prob=.25)
b.data$state<-1+rbinom(N, size=1, prob=0.1)

# Simulation parameters:
time.window<-12; time.interval<-2
h<-0.5
times<-0:15

# Simulating exact transition times:
cat("Simulating exact transition times...\n")
for(i in 1:N){
  # Data for individual i:
  data.i<-b.data[b.data$id==i,]
  age0<- data.i$age
  x<- data.i$x
  state<-data.i$state
  id<-data.i$id
  
  # Simulated trajectory for i:
  flag<-TRUE
  states<-state
  ages<-age0
  time<-0
  while(flag){
   
  # Piecewise-constant approach:
  lp<-c(1,age0+time,x)
  
  # Intensities:
  q12<-exp(beta[1,]%*%lp)
  q13<-exp(beta[2,]%*%lp)
  q21<-exp(beta[3,]%*%lp)
  q23<-exp(beta[4,]%*%lp)
  
  # From state 1:
  jump<-0
  if(state==1){
    rate<-q12+q13
    t<-rexp(1, rate = rate)
    if(t<=h){
       state<-sample(c(2,3), 1, prob = c(q12/rate, q13/rate))
       time<-time+t
       ages<-c(ages,age0+time); states<-c(states,state)
       jump<-1
       }
  }
  # From state 2:
  if(state==2 & jump==0){
    rate<-q21+q23
    t<-rexp(1, rate = rate)
    if(t<=h){
       state<-sample(c(1,3), 1, prob = c(q21/rate, q23/rate))
       time<-time+t
       ages<-c(ages,age0+time); states<-c(states,state)
       } 
  }
  # If these was no transition:
  if(!jump){time=time+h}
         
  # Stop individual track if beyond follow-up or death:
  if(state==3){flag<-FALSE}
  if(state!=3 & (time+h)>=time.window){
     ages<-c(ages,age0+time.window); states<-c(states,-2)
     flag<-FALSE
  }
  }
  
  # Save data:
  n.data<-as.data.frame(cbind(id=id,age=ages,state=states,x=x))
  if(i==1){s.data<-n.data}else{s.data<-rbind(s.data,n.data)}
}

# Impose interview waves: data -> design data (ddata).
cat("Imposing a design...\n")
# Fixed time grid for waves in years:
grid<-seq(0,time.window,by=time.interval)
for(i in 1:N){
   # Data of individual i:
   data.i<-s.data[s.data$id==i,]
   ages<-data.i$age; if((ages[length(ages)]-ages[1])>time.window){ardo}
   states<-data.i$state
   x<-data.i$x[1]
   age.grid<-data.i$age[1]+grid 
   death<-any(states==3)
   
   # Impose interview times:
   ddata.i<-as.data.frame(cbind(id=i,state=NA,age=age.grid,x=x[1]))
   # Baseline:
   ddata.i$state[1]<-states[1]
   # Follow-up:
   for(j in 2:length(age.grid)){
      ind<-1
      for(k in 2:length(ages)){ if(ages[k]<= age.grid[j]){ind<-ind+1} }
      ddata.i$state[j]<-states[ind]
   }

   # Dealing with death:
   if(death){
     for(j in 1:length(age.grid)){
     if(ddata.i$state[j]==3){
        ddata.i<-ddata.i[1:(j-1),]
	    # Take exact time of death:
        ddata.i<-rbind(ddata.i,data.i[length(ages),])
        if((tail(ddata.i$age,1)-head(ddata.i$age,1)>time.window)){ardo}
	    break
      }
     }
   }

   # Build data file:
   if(i==1){ddata<-ddata.i}else{ddata<-rbind(ddata,ddata.i)}
}
data<-ddata

# Round age:
data$age<-round(data$age,2)

# Check for extreme small time intervals and adjust:
mintime<-0.1
count<-0
for(i in 2:nrow(data)){
  if(data$state[i-1]!=3 & data$state[i-1]!= -2){
    if( (data$age[i]-data$age[i-1])< mintime){
            data$age[i-1]<-data$age[i-1]-mintime
            count<-count+1
       }
  }
}
cat(count,"very short time interval(s) adjusted.\n")

# Shoot a few holes in the data?:
if(0){
cat("Impute missing states...\n")
nholes<-10
i<-1
count<-0
while(count<nholes){
  data.i<-data[data$id==i,]
  if(any(data.i$state==2)){
    data.i$state[min(which(data.i$state==2))]<- -1  
    data[data$id==i,]<-data.i
    cat("Missing state created for id =",i,"\n")
    count<-count+1
  }
  i<-i+1
}
}

# Print state table:
cat("State table data:\n")
print(statetable.msm(state, id, data=data))

# Fit model:
cat("Fit model...\n")
# Determine start Q:
q<-0.01
Q<-rbind(c(0,q,q), c(q,0,q),c(0,0,0))
qnames<-c("q12","q13","q21","q23")

# Estimate model:
model<-msm(state~age, subject=id, data=data, 
    center=FALSE, qmatrix=Q, death=TRUE, 
    covariates=~age+x, censor=-2,
    censor.states=c(1,2), 
    method="BFGS", 
    control=list(trace=1, REPORT=1,maxit=1000, fnscale=10000))

# Generate output:
cat("\n-2loglik =", model$minus2loglik,"\n")
cat("Convergence code =", model$opt$convergence,"\n")
p<-model$estimates
if(is.null(model$covmat)){p.se<-NA}else{p.se<-sqrt(diag(model$covmat))}
print(cbind(q=qnames,p.true=p.true,p=round(p,digits),
            se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
            "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),quote=FALSE)
            

dta <- data
save(dta,file="dataEx1.RData")
