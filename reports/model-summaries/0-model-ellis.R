rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(msm)
requireNamespace("readxl")

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/ELECT-utility-functions.R") # ELECT utility functions

# ---- declare-globals ---------------------------------------------------------


# ---- load-data-odds ---------------------------------------------------------------
h70  <- readRDS("./data/unshared/raw/models/msm-model-H70.rds")
lasa <- readRDS("./data/unshared/raw/models/msm-model-LASA.rds")
map  <- readRDS("./data/unshared/raw/models/msm-model-MAP.rds")
octo <- readRDS("./data/unshared/raw/models/msm-model-OCTO.rds")
lbc  <- NULL #readRDS("./data/unshared/raw/models/msm-model-lbc1921.rds")
wh   <- readRDS("./data/unshared/raw/models/msm-model-Whitehall.rds")
# create list objects containing models with full msm estimation info
ls_models <- list(
  "octo" = octo,
  "lasa" = lasa,
  "wh"   = wh,
  "h70"  = h70,
  # "lbc"  = lbc,
  "map"  = map
)

saveRDS(ls_models,"./data/unshared/derived/models/ls_models.rds")

# ---- define-utility-functions --------------------
# define function to extract sample size from msm object
get_sample_size <- function(msm_object){ 
  # msm_object <- map
  N <- length(unique(msm_object$data$mf$`(subject)`))
  return(N)
}
# ---- tweak-data-odds -----------------------------
# create list object with tables of hazard ratios
ls_odds <- list()
for(i in names(ls_models)){
  # i <- 1
  ls_odds[[i]] <- print_hazards( ls_models[[i]], dense = F) 
} 
# review the spellings of covariates 
unique(ls_odds$octo$predictor) %>% as.character()
unique(ls_odds$lasa$predictor) %>% as.character()
unique(ls_odds$wh$predictor)%>% as.character()
unique(ls_odds$h70$predictor)%>% as.character()
# unique(ls_odds$lbc$predictor)%>% as.character()
unique(ls_odds$map$predictor)%>% as.character()
# construct listing of possible names for each predictor
covar_age     <- c("age")
covar_male    <- c("male","gender","sex")
covar_edu     <- c("eduyrs", "education","tedtotyr", "EDUYR", "edu")
covar_network <- c("numfr", "network","netw","socsize","soc_net")
covar_cogact  <- c("cogact", "cogpartic","leisure","cogcomp","cogact_old")
# force all models to have the same spelling of covariates
for(i in names(ls_odds)){
  ls_odds[[i]] <- ls_odds[[i]] %>% 
    dplyr::mutate(
      predictor = ifelse(predictor %in% covar_age,     "age",
                  ifelse(predictor %in% covar_male,    "male",
                  ifelse(predictor %in% covar_edu,     "edu",
                  ifelse(predictor %in% covar_network, "network",
                  ifelse(predictor %in% covar_cogact,  "cogact", NA )))))
    )
}
# combine into a single dataset
ds_odds <- plyr::ldply(ls_odds, .id = "study")
# create a dense column
ds_long <- ds_odds %>% 
  dplyr::mutate(
    HR = ifelse(HR==1L & L==1L & U==1L,NA,HR),
    L  = ifelse(HR==1L & L==1L & U==1L,NA,L),
    U  = ifelse(HR==1L & L==1L & U==1L,NA,U)
  ) %>% 
  dplyr::mutate(
    hr    = sprintf("%0.2f", HR),
    lo    = sprintf("%0.2f", L),
    hi    = sprintf("%0.2f", U),
    zero  = ifelse((L < 1 & 1 < U), TRUE,FALSE),
    sign  = ifelse((L < 1 & 1 < U), " ",  "*"),
    dense = sprintf("%4s (%4s,%5s)%1s", hr, lo, hi,sign),
    dense = ifelse(is.na(HR),"---", dense)
    # dense = ifelse(dense %in% c("(1.00 (1.00, 1.00) ","(1.00 (1.00, 1.00)*"), NA, dense)
  ) %>% 
  dplyr::select(
    study, transition, predictor, dense,HR, U,L, zero
  )

saveRDS(ds_long,"./data/shared/derived/summary-tables/table-odds.rds")

ds_odds %>% dplyr::filter(study=="wh")
##############################
##### Life Expectancies ######
##############################

# # ---- load-data-le ------------------------------------
# ds0_le <-  readxl::read_excel("./data/shared/raw/misc/results.xlsx", sheet = "le")
# 
# # ----- groom-life-expectancies ---------------------
# regex_estimator <- "^(-?\\d+.\\d+)\\s*\\((\\d+.\\d+),\\s*(-?\\d+.\\d+)\\)$"
# ds_le <- ds0_le %>% tidyr::gather_('study',"value", c("octo","lasa","h70","lbc1921","map")) %>%
#   dplyr::mutate(
#     est      = as.numeric(gsub(regex_estimator,"\\1",value,perl=T)),
#     low      = as.numeric(gsub(regex_estimator,"\\2",value,perl=T)),
#     high     = as.numeric(gsub(regex_estimator,"\\3",value,perl=T))
#   ) %>% 
#   dplyr::mutate(
#     age = factor(age),
#     condition = factor(condition, levels = c(
#       "Female, Educ(H), SES(H)",
#       "Female, Educ(M), SES(M)",
#       "Female, Educ(L), SES(L)",
#       "Male, Educ(H), SES(H)",
#       "Male, Educ(M), SES(M)",
#       "Male, Educ(L), SES(L)"
#     )),
#     edu       = factor(edu, levels = c("high","medium","low")),
#     condition = factor(condition, levels = rev(levels(condition))),
#     sex_group = factor(paste0(sex,"-",group)),
#     group_sex = factor(paste0(group,"-",sex)),
#     group_age = factor(paste0(group,"-",age)), 
#     age_group = factor(paste0(age,"-",group)),
#     age_group = factor(age_group, levels = c(
#       "80-total" , "80-non-impaired","85-total","85-non-impaired" 
#     )),
#     age_sex   = factor(paste0(age,"-",sex)),
#     sex_age   = factor(paste0(sex,"-",age)),
#     group_tertile = factor(paste0(group,"-",edu)),
#     group_tertile = factor(group_tertile, levels = c(
#       "non-impaired-low","non-impaired-medium", "non-impaired-high",
#       "total-low", "total-medium","total-high"
#       
#     ))
#   )
# dplyr::select(-value)
# x <- ds_le
# head(ds_le)
# levels(ds_le$group_tertile)


