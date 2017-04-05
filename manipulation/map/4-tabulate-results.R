# Tabulates estimated solutions for each model (WITHIN tables)
# Compiles tables for comparisons across models (BETWEEN tables)

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# base::source("http://www.ucl.ac.uk/~ucakadl/ELECT/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT-utility-functions.R") # ELECT utility functions
source("./scripts/graph-presets.R") # fonts, colors, themes 
source("./scripts/general-graphs.R")
source("./scripts/specific-graphs.R")
# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(msm)
requireNamespace("ggplot2", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE) 
requireNamespace("testit", quietly=TRUE)

# ---- declare-globals ---------------------------------------------------------
pathSaveFolder <- "./data/shared/derived/models/model-b-mod-2/"
digits = 2
cat("\n Save fitted models here : \n")
print(pathSaveFolder)


# ---- load-data ---------------------------------------------------------------
ds_clean <- readRDS("./data/unshared/ds_clean.rds")
ds <- readRDS("./data/unshared/ds_estimation.rds") # same ids but fewer variables

# folder with model objects
pathSaveFolder <- "./data/shared/derived/models/model-b-mod-2/"

model <- readRDS("./data/shared/derived/models/model-b-mod-2/mB_mod2_3_le.rds")
# lapply(model, names)
# model$levels
# le <- model$le[[1]]
# str(le)
# summary.elect(le)
# plot.elect(le)

# assemble model object
e_names <- attr(le$pnt, "names")
d_sim <- le$sim
# LEs <- le

# this function only a temp hack before such functionality is added to summary.elect()
describe_reps<- function(LEs, probs = c(.025,0.5,.975)){
  (pnt <- LEs$pnt)
  (e_names <- attr(le$pnt, "names"))
  sim <- le$sim
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
# descriptives <- describe_reps(le)
lapply(model, names)

i <- 1
for(i in seq_along(model$le)){
  (d0 <- model$levels[i,])
  (d1 <- describe_reps(model$le[[i]]))
  (d2 <- cbind(d1,d0))
  (d2$e_name <- attr(le$pnt, "names") )
  (d2$condition_n <- i)
  model[["descriptives"]][[paste0(i)]] <- d2
}
lapply(model, names) 
model$descriptives$`1`

ds <- do.call("rbind", model$descriptives)
rownames(ds) <- NULL
ds <- ds %>% 
  dplyr::select(condition_n, e_name, male, educat, sescat, dplyr::everything()) %>% 
  dplyr::mutate(
    pnt      = sprintf("%0.2f", as.numeric(pnt)),
    mn       = sprintf("%0.2f", as.numeric(mn)),
    se       = sprintf("%0.2f", as.numeric(se)),
    `0.025q` = sprintf("%0.2f", as.numeric(`0.025q` )),
    `0.5q`   = sprintf("%0.2f", as.numeric(`0.5q` )),
    `0.975q` = sprintf("%0.2f", as.numeric(`0.975q` ))
    
  )
head(ds)
str(ds)

ds %>% 
  dplyr::filter(condition_n == 1) %>% 
  print()

library(DT)
ds %>% DT::datatable(
  class   = 'cell-border stripe',
  caption = "",
  filter  = "top",
  options = list(pageLength = 6, autoWidth = TRUE)
)


d_sim <- as.data.frame(le$sim)
names(d_sim) <- e_names
head(d_sim)

d_sim %>% histogram_continuous("e11",bin_width = .1,rounded_digits = 3)
d_sim %>% histogram_continuous("e13")

plot.elect(le)
print_hazards(model$msm)

# ---- basic-table -------------------
str(le)




sim <- le$sim; str(sim)

(a <- summary.elect(le, print=F))





