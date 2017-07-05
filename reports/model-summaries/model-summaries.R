rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
requireNamespace("readxl")

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)

# ---- load-sources ------------------------------------------------------------
source("./scripts/graph-presets.R")
source("./scripts/graph-specific.R")

# ---- declare-globals ---------------------------------------------------------
# in reverse, to save a line later
lvl_predictors <- c(
   "Cognitive Activity"= "cogact"                 
  ,"Social Network"    = "network"                     
  ,"Education"         = "edu"                     
  ,"Sex(M)"            = "male"                 
  ,"Age"               = "age"                                 
)
lvl_transitions <- c(
   "1-->2" = "State 1 - State 2"
  ,"1-->4" = "State 1 - State 4"
  ,"2-->1" = "State 2 - State 1"
  ,"2-->3" = "State 2 - State 3"
  ,"2-->4" = "State 2 - State 4"
  ,"3-->4" = "State 3 - State 4"
)
# lvl_transitions <- c(
#     "State 1 - State 2" = "1-->2"
#   , "State 1 - State 4" = "1-->4"
#   , "State 2 - State 1" = "2-->1"
#   , "State 2 - State 3" = "2-->3"
#   , "State 2 - State 4" = "2-->4"
#   , "State 3 - State 4" = "3-->4"
# )
lvl_studies <- c(
   "OCTO-Twin"   = "octo"                
  ,"MAP"         = "map"         
  ,"LASA"        = "lasa"          
  # ,"LBC1921"     = "lbc"             
  ,"H70"         = "h70"
  ,"Whitehall"   = "wh"
)

# adds neat styling to your knitr table
neat <- function(x, output_format = "html",...){ 
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # knitr::kable() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  } 
  return(x_t)
}


# ---- load-data -----------------------------
# input list objects with estimated models from all studies
ls_models <- readRDS("./data/unshared/derived/models/ls_models.rds")
# lapply(ls_models, names) # quick view into the contents
# input long data.frame of hazard ratios
ds_odds_long <- readRDS("./data/shared/derived/summary-tables/table-odds.rds")
# select only the studies in the list
ds_odds_long <-  ds_odds_long %>% dplyr::filter(study %in% lvl_studies)
# ds_odds_long %>% head()
# ds_odds_long %>% 
  # dplyr::distinct(transition, study)
# input wide data.frame with life expectancies
# ds_le_wide <-  readxl::read_excel("./data/shared/raw/misc/results.xlsx", sheet = "le")

# extract sample size
sample_size <- c()
for(i in names(ls_models)){
  sample_size[i] = length(unique(ls_models[[i]]$data$mf$`(subject)`))
}

 
# ----- tweek-data --------------------------

# groom hazard ratios
regex_estimator <- "^(-?\\d+.\\d+)\\s*\\((\\d+.\\d+),\\s*(-?\\d+.\\d+)\\)$"
regex_state <- "^\\s*State (\\d).*State (\\d)$"
ds_odds <- ds_odds_long %>%
  dplyr::mutate(
    outgoing = as.numeric(gsub(regex_state,"\\1",transition,perl=T)),
    incoming = as.numeric(gsub(regex_state,"\\2",transition,perl=T)),
    trans = paste0(outgoing,"-->",incoming)
  ) %>%
  # dplyr::filter(trans == "3-->2")
  # prepare factors
  dplyr::mutate(
    trans = factor(trans, levels = names(lvl_transitions)),
    trans = factor(trans, levels = rev(levels(trans))),
    study = factor(study, levels = lvl_studies,labels = names(lvl_studies)),
    # study_label = factor(study, levels = lvl_studies,labels = names(lvl_studies)),
    # study = as.character(study),
    predictor = factor(predictor, levels =lvl_predictors, label = names(lvl_predictors))
    # predictor_label = factor(predictor, levels =lvl_predictors, label = names(lvl_predictors)),
    # predictor = as.character(predictor)
  )
head(ds_odds)
str(ds_odds)
ds_odds %>% dplyr::distinct(study,transition, trans)   %>% 
  dplyr::arrange(trans)
levels(ds_odds$trans)

# groom life expectancies
# regex_estimator <- "^(-?\\d+.\\d+)\\s*\\((\\d+.\\d+),\\s*(-?\\d+.\\d+)\\)$"
# ds_le <- ds_le_wide %>% tidyr::gather_('study',"value", c("octo","lasa","h70","lbc1921","map")) %>%
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

# ---- prepare-odds-table -------------------------
# spread into a wide format
ds_odds_wide <- ds_odds_long %>% 
  dplyr::select(-zero) %>% 
  dplyr::select(-HR,-U,-L) %>% 
  tidyr::spread(key = study, value = dense) %>% 
  dplyr::mutate( 
    predictor = factor(predictor, 
                       levels = lvl_predictors,#   c("age","male","edu","network", "cogact"),
                       labels = lvl_predictors %>% names()) #c("Age","Sex","Education","Social Network", "Cognitive Activity"))
  )  
ds_odds_wide[is.na(ds_odds_wide)] <- "---"
ds_odds_wide <- ds_odds_wide %>% 
  # dplyr::mutate(
  #   include = ifelse(
  #     # h70 =="---" &
  #     lasa =="---" &
  #     # lbc =="---" &
  #     map =="---" &
  #     octo =="---" &
  #     wh =="---"
  #     , FALSE,TRUE)
  # ) %>% 
  # dplyr::filter(include==TRUE) %>% 
  # dplyr::select(-h70) %>% 
  dplyr::arrange(predictor, transition) #%>% 
  # dplyr::select(-include)



# ---- print-dynamic-table-2 --------------------------
ds_odds_wide %>%
  DT::datatable()
# ---- print-static-table-2 ---------------------------
col_header <- c("Transition", "Predictor",names(lvl_studies)) %>% as.character()
ds_odds_wide %>% knitr::kable(col.names=col_header, format="pandoc") %>% print()

for(i in levels(ds_odds_wide$predictor)){
  # i <- "Age"
  cat("\n",i,"\n")
  ds_odds_wide %>%
    dplyr::filter(predictor==i) %>%
    dplyr::select(-predictor) %>%
    knitr::kable(
      col.names = rep("",ncol(ds_odds_wide)-1)#,
      # format="pandoc"
      # col.names = setdiff(col_header,"Predictor")
    ) %>%
    print()
    # neat(output_format = "pandoc")
    # neat(output_format = "html")
  cat("\n")
}

# ---- print-plot-odds --------------------
head(ds_odds)
ds_odds <- ds_odds %>% 
  dplyr::rename(est=HR,low=L,high=U)
  # dplyr::mutate(study = as.character(study))
head(ds_odds)  
str(ds_odds)
ds_odds %>% dplyr::distinct(trans)
# ds_odds %>% plot_odds("trans", "OCTO-Twin", "Age")
# ds_odds %>% plot_odds("study", "1-->4","Age")
# ds_odds %>% plot_odds("study", "State 1 - State 4","Age")
# ds_odds %>% plot_odds("predictor", "OCTO-Twin","1-->4")
 
# store prints here:
folder <- "./reports/model-summaries/graphs/"

name <- "A-transition-by-predictor"
title <- paste0("Estimated Odds-Ratios and their 95% Confident Intervals.\n View (A): ",name) 
supermatrix_odds(
  ls_graphs = list(
      matrix_odds(ds_odds,"study",lvl_transitions,"Age")
    , matrix_odds(ds_odds,"study",lvl_transitions,"Sex(M)")
    , matrix_odds(ds_odds,"study",lvl_transitions,"Education")
    , matrix_odds(ds_odds,"study",lvl_transitions,"Social Network")
    , matrix_odds(ds_odds,"study",lvl_transitions,"Cognitive Activity")
  ),
  folder_name = folder,
  plot_name = name,
  main_title = title,
  width = 1200,
  height= 1200,
  res = 120
)

name <- "B-study-by-predictor"
title <- paste0("Estimated Odds-Ratios and their 95% Confident Intervals.\n View (B): ",name) 
supermatrix_odds(
  ls_graphs = list(
    matrix_odds(ds_odds,  "trans",lvl_studies,"Age")
    , matrix_odds(ds_odds,"trans",lvl_studies,"Sex(M)")
    , matrix_odds(ds_odds,"trans",lvl_studies,"Education")
    , matrix_odds(ds_odds,"trans",lvl_studies,"Social Network")
    , matrix_odds(ds_odds,"trans",lvl_studies,"Cognitive Activity")
  ),
  folder_name = folder,
  plot_name = name,
  main_title = title,
  width = 1200,
  height= 1200,
  res = 120
)

name <- "C-study-by-transition"
title <- paste0("Estimated Odds-Ratios and their 95% Confident Intervals.\n View (C): ",name) 
supermatrix_odds(
  ls_graphs = list(
    matrix_odds(ds_odds,"predictor",lvl_studies, "1-->2")
    ,matrix_odds(ds_odds,"predictor",lvl_studies,"1-->4")
    ,matrix_odds(ds_odds,"predictor",lvl_studies,"2-->1")
    ,matrix_odds(ds_odds,"predictor",lvl_studies,"2-->3")
    ,matrix_odds(ds_odds,"predictor",lvl_studies,"2-->4")
    ,matrix_odds(ds_odds,"predictor",lvl_studies,"3-->4")
  ),
  folder_name = folder,
  plot_name = name,
  main_title = title,
  width = 1200,
  height= 1200,
  res = 120
)

# ---- insert-odds-plots ---------------------

# plot_a <- "./reports/model-summaries/graphs/A-transition-by-predictor.png"
# plot_b <- "./reports/model-summaries/graphs/B-study-by-predictor.png"
# plot_c <- "./reports/model-summaries/graphs/C-study-by-transition.png"

ls_path_plots <- list(
  "View (A)" = "./reports/model-summaries/graphs/A-transition-by-predictor.png",
  "View (B)" = "./reports/model-summaries/graphs/B-study-by-predictor.png",
  "View (C)" = "./reports/model-summaries/graphs/C-study-by-transition.png"
)

for(i in names(ls_path_plots)){
  cat("\n ## ", i , "\n")
  cat('<img src="', ls_path_plots[[i]], '" alt="', basename(ls_path_plots[[i]]), '">\n', sep="")
}



# ----- publisher --------------------
path <- "./reports/model-summaries/table-2.Rmd"

rmarkdown::render(
  input = path ,
  output_format=c(
    # "html_document"
    "word_document"
    # ,"pdf_document"
  ),
  clean=TRUE
)

