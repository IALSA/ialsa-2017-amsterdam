# knitr::stitch_rmd(script="./reports/review-variables/map/review-variables-map.R", output="./reports/review-variables/map/review-variables-map.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/function-common.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes 
source("./scripts/graph-general.R")
source("./scripts/graph-specific.R")


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(knitr)
library(dplyr)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
# data_path_input  <- "../MAP/data-unshared/derived/ds0.rds" # original
dto <- readRDS("./data/unshared/derived/2-dto.rds") # local copy
# each element this list is another list:
names(dto)
# 1st element - data set with unit data. Inspect the names of variables:
names(dto[["unitData"]])
# 2nd element - dataset with augmented names and labels of the unit data
ds_meta <- dto[["metaData"]] %>% tibble::as_tibble()
kable(head(ds_meta))

# full version of data before any subsetting or filtering
ds0 <- dto[["unitData"]] %>% tibble::as_tibble()
# import data set used for modeling
ds_model <- dto[["ms_mmse"]][["model"]]%>% tibble::as_tibble()


# ---- meta-table --------------------------------------------------------
ds_meta %>%  
  dplyr::select(-url, -notes) %>% 
  dplyr::mutate(type = ordered(type)) %>% 
  DT::datatable(
    class   = 'cell-border stripe',
    caption = "This is a dynamic table of the metadata file. Edit at `./data/meta/map/meta-data-map.csv",
    filter  = "top",
    options = list(pageLength = 6, autoWidth = TRUE)
  )

  

# ---- inspect-data -------------------------------------------------------------
ds0 %>% dplyr::glimpse()
# this is how we can interact with the `dto` to call and graph data and metadata
ds_meta %>% 
  dplyr::filter(type=="demographic") %>% 
  dplyr::select(name,name_new,label)


# ---- tweak-data --------------------------------------------------------------
# create a subset of
# observations - only those used in statistical models
# identify the individuals used for modeling
modeled_ids <- 
  # dto[["ms_mmse"]][["model"]] %>% 
  ds_model %>% 
  dplyr::distinct(id) %>% 
  as.list() %>% 
  unlist() %>% 
  as.vector()
modeled_ids %>% head()
modeled_ids %>% length()

# variablables - only those relevant to the research question
ds <- ds0 %>% 
  dplyr::filter(id %in% modeled_ids) %>% 
  dplyr::mutate(
    age_bl       = as.numeric(age_bl),
    age_at_death = as.numeric(age_death),
    male         = as.logical(ifelse(!is.na(msex), msex=="1", NA_integer_)),
    died         = as.logical(died),
    dementia     = as.logical(dementia),
    edu          = as.numeric(educ)
  ) %>%
  dplyr::select_(
    "id",              # personal identifier
    "male",            # gender
    "edu",             # years of education
    "age_bl",          # age at baseline
    "age_at_death",    # age at death
    "died",            # death indicator
    "birth_year",      # year of birth 
    "fu_year",         # follow-up year
    "age_at_visit",    # age at interview
    "date_at_visit",   # date of the interview
    "mmse",            # mini mental state exam (max =30)
    "cogn_global",     # global cognition
    "dementia",        # dementia diagnosis (?)
    "income_40",       # income at age 40, proxy for SES
    "cogact_old",      # cognitive activity in late life
    "socact_old",      # social activity in late life
    "soc_net",         # social network size
    "social_isolation" # perceived loneliness
  )
head(ds)
ds %>% distinct(id) %>% count()
# ds$id %>% unique() %>% length() # alt
ds %>% glimpse()
ds %>% filter(id==9121) %>% as.data.frame()

# subset a smaller cohort for testing
set.seed(41)
sample_size <- 100
ids <- sample(ds$id,sample_size)
d <- ds %>% dplyr::filter(id %in% ids) 

# ---- basic-table --------------------------------------------------------------

# ---- basic-graphs --------------------------------------------------------------
ds0 %>% histogram_continuous("age_death", bin_width=1)

ds0 %>% histogram_discrete("msex")

# print an elemental graph
d %>% basic_line_v2("cogn_global", "fu_year", "salmon", .9, .1, T)
# print complex graph of various applications of elemental graphs
d %>% raw_smooth_lines_v2("cogn_global")
# d %>% raw_smooth_lines_v2("cogact_old")
# d %>% raw_smooth_lines_v2("socact_old")
# d %>% raw_smooth_lines_v2("soc_net")
# ds %>% raw_smooth_lines_v2("soc_net",line_size=.3, line_alpha=.2)

# ---- testing --------------------------

psych::summary.psych(ds)

library(dplyr)
library(ggplot2)
ids <- d$id %>%  unique() %>% sample(1)
d %>% 
  dplyr::filter(id %in% ids) %>% 
  print()

d <- ds %>% 
  group_by(id) %>% 
  mutate(unique = length(unique(income_40))) %>% 
  ungroup() %>% 
  select(id,fu_year,income_40, unique) %>% 
  arrange(desc(unique)) %>% 
  as.data.frame()
head(d)
table(d$unique)

g <- ggplot2::ggplot(ds, aes(x=fu_year, y=age_at_visit)) +
       geom_point(aes(color=male)) +
  facet_grid(.~ income_40) 
g


# ---- print-time-variant ---------------------------------
print_these <- c(
  "mmse",           
  "cogn_global",    
  "dementia",       
  "income_40",      
  "cogact_old",     
  "socact_old",     
  "soc_net",        
  "social_isolation"
)
for(i in print_these){
  cat("\n")
  cat("\n## ", i)
  i <- "mmse"
  label <- ds_meta %>% 
    dplyr::filter(name_new == i) %>% 
    dplyr::select(name_new, label, url)
  cat("\nVariable name: ", label$name_new)
  cat("\nVariable label: ", label$label)
  cat("\n", paste0("[More info](",label$url,")"),"\n")
  summary(d)
  cat("\n Observed and Modeled measures over time \n")
  d %>% raw_smooth_lines_v2(i) %>% print()
  cat("\n")
}

# ---- publish ---------------------------------------
path_report_1 <- "./reports/descriptives/map/review-variables-map.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
# allReports <- c(path_report_1,path_report_2)
allReports <- c(path_report_1)

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



# ---- B-1-N-at-each-wave -------------------------------------------------------
ds %>% dplyr::filter(name=="fu_year")
ds %>% 
  dplyr::group_by_("fu_year") %>%
  dplyr::summarize(sample_size=n())

# ----- B-2-cognitive-1 -----------------------
knitr::kable(dto[["metaData"]] %>% 
  dplyr::filter(type=="cognitive") %>% 
  dplyr::select(-name,-type,-name_new, -include) %>%
  dplyr::arrange(construct))

# ----- B-2-cognitive-2 -----------------------
knitr::kable(dto[["metaData"]] %>% 
  dplyr::filter(type=="cognitive", include==TRUE) %>% 
  dplyr::select(-type,-name_new, - include) %>%
  dplyr::arrange(construct))

# ----- B-2-cognitive-3 -----------------------
dto[["unitData"]] %>% 
  dplyr::select(id,fu_year, cogn_global) %>% 
  dplyr::filter(!is.na(cogn_global)) %>%
  dplyr::group_by(fu_year) %>% 
  dplyr::summarize(average_global_cognition = round(mean(cogn_global),3),
                   sd = sprintf("%0.2f",sd(cogn_global)), 
                   observed =n()) 

# ----- B-2-cognitive-4 -----------------------
set.seed(1)
ids <- sample(ds$id,100)
d <- dto[["unitData"]] %>% dplyr::filter(id %in% ids)
g <- basic_line(d, "cogn_global", "fu_year", "salmon", .9, .1, T)
# g


# ----- B-2-cognitive-5-cogn_global -----------------------
raw_smooth_lines_v2(d, "cogn_global")

# ----- B-2-cognitive-5-cogn_se -----------------------
raw_smooth_lines_v2(d, "cogn_se")

# ----- B-2-cognitive-5-cogn_ep -----------------------
raw_smooth_lines_v2(d, "cogn_ep") 

# ----- B-2-cognitive-5-cogn_wo -----------------------
raw_smooth_lines_v2(d, "cogn_wo") 

# ----- B-2-cognitive-5-cogn_po -----------------------
raw_smooth_lines_v2(d, "cogn_po")

# ----- B-2-cognitive-5-cogn_ps -----------------------
raw_smooth_lines_v2(d, "cogn_ps")

# ----- B-2-cognitive-5-cogn_ps -----------------------
raw_smooth_lines_v2(d, "cogn_ps")

# ----- B-2-cognitive-5-mmse -----------------------
raw_smooth_lines_v2(d, "mmse")





# ----- B-3-dementia-diagnosis -------------------------------------------------
dto[["metaData"]] %>% dplyr::filter(name=="dementia")

ds <- dto[["unitData"]] %>% 
  dplyr::filter(!is.na(dementia)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(percent_diagnosed=mean(dementia),
                   observed_n = n()) 
ds

g <- ggplot2::ggplot(ds, aes_string(x="fu_year",y="percent_diagnosed")) 
g <- g + geom_line(na.rm = T)
g <- g + main_theme
g
  
ds <- dto[["unitData"]] %>% 
  dplyr::filter(!is.na(dementia)) %>% 
  dplyr::mutate(age_cat = cut(age_at_visit,breaks = 10)) %>% 
  dplyr::group_by_("age_cat") %>%
  dplyr::summarize(percent_diagnosed=mean(dementia),
                   observed_n = n())  
ds

  
  g <- ggplot2::ggplot(ds, aes_string(x="age_cat",y="percent_diagnosed")) 
  g <- g + geom_bar(stat="identity")
  g <- g + main_theme
  g
  

# ---- B-4-education -----------------------------------------------------------
dto[["metaData"]] %>% dplyr::filter(name=="educ")
# shows attrition in each education group
dto[["unitData"]] %>% 
  dplyr::filter(!is.na(educ)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(average_years_edu=mean(educ),
                   SD=sd(educ),
                   observed_n = n())


# ---- B-5-social-class -----------------------------------------------------------
# dto$unitData %>% 
#   
# income_40


# ---- B-6-bmi -----------------------------------------------------------

dto[["metaData"]] %>% dplyr::filter(name %in% c("bmi","wtkg", "htm"))
# descriptives by follow-up year
dto[["unitData"]] %>% 
  dplyr::filter(!is.na(bmi)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(average_bmi=mean(bmi),
                   SD=sd(bmi),
                   observed_n = n())

# ---- B-7-smoking -----------------------------------------------------------
dto[["metaData"]] %>% dplyr::filter(construct %in% c("smoking"))
t <- dto[["unitData"]] %>% 
  dplyr::filter(!is.na(q3smo_bl)) %>% 
  dplyr::group_by_("q3smo_bl") %>% 
  dplyr::summarize(n=n()) 
t 
t  %>% histogram_continuous("q3smo_bl")
  
dto[["unitData"]] %>% 
  dplyr::filter(!is.na(q3smo_bl)) %>% 
  dplyr::group_by_("fu_year") %>% 
  dplyr::summarize(average_smoking_quantity=mean(q3smo_bl),
                   SD=sd(q3smo_bl),
                   observed_n = n())

table(ds$iadlsum)

# ---- reproduce ---------------------------------------
rmarkdown::render(
  input = "./reports/review-variables/map/review-variables.Rmd" ,
  output_format="html_document", clean=TRUE
)