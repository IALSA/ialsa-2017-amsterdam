# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output. 
# knitr::stitch_rmd(
#   script="./manipulation/map/0-ellis-island-map.R",
#   output="./manipulation/map/stitched-output/0-ellis-island-map.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

############################
##  Land on Ellis Island  ##
############################

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
# Ideally, no real operations are performed in these sourced scripts. 
source("./scripts/common-functions.R") # used in multiple reports

# ---- load-packages ----------------------------------------------
# Attach packages so their functions don't need to be qualified when used
# See more : http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # Pipes
library(ggplot2) # Graphs
# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("testit") # for asserting conditions meet expected patterns.

# ---- declare-globals ----------------------------------------------
# reach out to the curator for a dataset prepared for general consumption
data_path_input  <- "../MAP/data-unshared/derived/dto.rds"
# point to the local metadata to be used for this project (specific consumption)
# metadata_path_input <- "./data/shared/meta/map/meta-data-map.csv" 
metadata_path_input <- "./data/shared/meta/map/meta-data-map-2016-09-09.csv" 

# ---- load-data ------------------------------------------------
# load data objects
dto      <- readRDS(data_path_input)
# dto already contains meta data, but you should load a local one for greater control
metaData <- read.csv(metadata_path_input, stringsAsFactors=F, header=T)

# ---- inspect-data ----------------------------------------------
# inspect loaded data objects (using basic demographic variables )
ds <- as.data.frame(dto$unitData) # assing alias
length(unique(ds$id))  # sample size, number of respondents
# t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t 
# t <- table(ds[,"msex"], ds[,"race"], useNA = "always"); t[t==0]<-".";t 
# t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t 

# ---- assemble-data-object-dto-1 --------------------------------------
# reconstruct the dto to be used in this project
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- ds
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  metaData # new, local meta-data!!
# verify and glimpse
# dto[["unitData"]] %>% dplyr::glimpse()
# dto[["metaData"]] %>% dplyr::glimpse()


# ----- view-metadata-1 ---------------------------------------------
meta_data <- dto[["metaData"]] %>%
  dplyr::filter(include == TRUE) %>%
  # dplyr::select(name, name_new, type, label, construct) %>%
  dplyr::arrange(type, construct, name) %>% 
  dplyr::select(name, name_new, label, construct, longitudinal)
knitr::kable(meta_data)

# # ----- apply-meta-data-1 -------------------------------------
# # rename variables
# d_rules <- dto[["metaData"]] %>%
#   dplyr::filter(name %in% names(ds)) %>% 
#   dplyr::select(name, name_new ) # leave only collumn, which values you wish to append
# names(ds) <- d_rules[,"name_new"]
# # transfer changes to dto
# ds <- ds %>% dplyr::filter(study == "MAP ")
# table(ds$study)
# dto[["unitData"]] <- ds 

# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
names(dto)
# 1st element - unit(person) level data
# 2nd element - meta data, info about variables
knitr::kable(names_labels(dto$unitData))












