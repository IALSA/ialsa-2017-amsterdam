#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
# the purpose of this script is to examine the results of estimation of msm models

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(msm)
requireNamespace("ggplot2", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE) 
requireNamespace("testit", quietly=TRUE)

# ---- load-sources ------------------------------------------------------------
# base::source("http://www.ucl.ac.uk/~ucakadl/ELECT/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT.r") # load  ELECT functions
base::source("./scripts/ELECT-utility-functions.R") # ELECT utility functions
# base::source("./scripts/common-functions.R") # used in multiple reports
base::source("./scripts/graph-presets.R") # fonts, colors, themes
# base::source("./scripts/general-graphs.R")
# base::source("./scripts/specific-graphs.R")
# ---- declare-globals ---------------------------------------------------------
# ELECT option:
digits = 2
# # Model objects saved here:
path_folder <- "./data/shared/derived/models/"
# ---- load-data ---------------------------------------------------------------
# first, the script `0-ellis-island.R` imports and cleans the raw data
# second, the script `1-encode-multistate.R` augments the data with multi-states
# thirds, the script `2-estimate-models.R` conducts msm estimation and ELECT simulation
# now we import the objects containing the results of the last script

# ---- load-data -------------------------------------------------

# ---- load-models-A ---------------------------------------------------------------
models <- list.files(file.path(path_folder),full.names=T, pattern="A_v")
model <- models[1]
# ---- load-data-v2 ---------------------------------------------------------------
# model_2 <- list.files(file.path(path_folder),full.names=T, pattern="A_v2")

# ---- load-data-target ---------------------------------------------------------------
# path_files %>% as.data.frame() %>% format(justify="left") %>% print()
# models <- list()
# for(i in seq_along(path_files)){
#   (min_age <- strsplit(path_files[i], split = "_")[[1]][3])
#   models[[paste0("age_",min_age)]] <- readRDS(path_files[i])
# }
model <- readRDS(models[1])

# ---- inspect-data -------------------------------------------------------------

# ---- utility-functions -------------------------------------------------------
# adds neat styling to your knitr table
neat <- function(x){
  # knitr.table.format = "html"
  x_t <- x %>%
  # x %>%
    # knitr::kable() %>%
    knitr::kable(format="html") %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed","responsive"),
      # bootstrap_options = c( "condensed"),
      full_width = F,
      position = "left"
    )
  # cat("\n",x_t,"\n")
  # print(x_t, n=n_)
  return(x_t)
}
# ds %>% distinct(id) %>% count() %>% neat(10)

# ---- msm-1 -----------------------------------
model$call
msm_summary(model) 

# ---- msm-2 -----------------------------------
msm_details(models$age_60$msm)

# ---- msm-3 -----------------------------------
print_hazards(model) %>% neat()

# ---- publisher --------------
path_report_1 <- "./reports/msm-model-review/msm-model-review-A.Rmd"


allReports <- c(path_report_1)
# allReports <- c(path_report_2)
# allReports <- c(path_report_3)
# allReports <- c(path_report_1, path_report_2, path_report_3)



pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}



