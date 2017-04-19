# ---- plotting-expectancies ------------------
plot_le_2 <- function(
  x
){
  x <- ds_le
  # yaxis = "condition"
  # yaxis = "group_sex"
  # yaxis = "sex_group"
  # group_ = "total"
  
  # d <- x
  d <- x %>%
    # dplyr::filter(edu=="high") %>% 
    # dplyr::filter(ses=="high")
    # dplyr::filter(group == group_) %>%
    dplyr::filter(age == 80)
  
  # study_ = levels_studies["octo"]
  # predictor_ = levels_predictors["age"]
  
  g <-  ggplot2::ggplot(d,aes_string(y     = "group_tertile" 
                                     ,x     = "est"
                                     # ,color = "age"
                                     # ,color = "edu"
                                     # ,fill  = "edu"
                                     # , color = "sex"
                                     # , fill = "sex"
                                     # , shape = "edu"
                                     # ,shape = "ses"
                                     # , shape = "sex"
  ))  
  g <- g + geom_errorbarh(aes_string(xmin = "low", xmax = "high"),
                          color    = "gray25", 
                          height   = .3, 
                          linetype = "solid", 
                          size     = 0.5) 
  g <- g + geom_point(size = 3) 
  # g <- g + facet_grid(edu~ses)
  # g <- g + facet_grid(condition~group)
  g <- g + facet_grid(sex~study)
  g <- g + main_theme
  # g
  # g <- g + scale_shape_manual(values = c("male"=24,"female"=25))
  g <- g + scale_shape_manual(values = c("low"=25,"medium"=21,"high"=24))
  # g <- g + scale_color_manual(values = c("male"="","FALSE"=NA))
  # g <- g + scale_fill_manual(values = c("TRUE"="black","FALSE"="white"))
  # g <- g + main_theme
  # g <- g + labs(shape = "", color="p < .05", fill = "p < .05")
  # g <- g + theme(axis.text.y = element_text(size=baseSize))
  g
  # if(label_=="(R)-Error"){
  #   g + guides(fill=FALSE, color=FALSE)
  # }else{
  #   g + guides(fill=FALSE, color=FALSE, shape=FALSE)
  #   
  # }
  
} 

# ds_le %>% plot_le_1()


plot_le_1 <- function(
  x
){
  # x <- ds_le
  # yaxis = "condition"
  # yaxis = "group_sex"
  # yaxis = "sex_group"
  # group_ = "total"
  
  d <- x
  # d <- x %>%
  # dplyr::filter(edu=="high") %>% 
  # dplyr::filter(ses=="high")
  # dplyr::filter(group == group_) %>%
  # dplyr::filter(age == 80)
  
  # study_ = levels_studies["octo"]
  # predictor_ = levels_predictors["age"]
  
  g <-  ggplot2::ggplot(d,aes_string(y     = "condition" 
                                     ,x     = "est"
                                     # ,color = "age"
                                     # ,color = "edu"
                                     # ,fill  = "edu"
                                     , color = "sex"
                                     , fill = "sex"
                                     , shape = "edu"
                                     # ,shape = "ses"
                                     # , shape = "sex"
  ))  
  g <- g + geom_errorbarh(aes_string(xmin = "low", xmax = "high"),
                          color    = "gray25", 
                          height   = .3, 
                          linetype = "solid", 
                          size     = 0.5) 
  g <- g + geom_point(size = 3) 
  # g <- g + facet_grid(edu~ses)
  # g <- g + facet_grid(condition~group)
  g <- g + facet_grid(age_group~study)
  g <- g + main_theme
  # g
  # g <- g + scale_shape_manual(values = c("male"=24,"female"=25))
  g <- g + scale_shape_manual(values = c("low"=25,"medium"=21,"high"=24))
  # g <- g + scale_color_manual(values = c("male"="","FALSE"=NA))
  # g <- g + scale_fill_manual(values = c("TRUE"="black","FALSE"="white"))
  # g <- g + main_theme
  # g <- g + labs(shape = "", color="p < .05", fill = "p < .05")
  # g <- g + theme(axis.text.y = element_text(size=baseSize))
  g <- g + labs(shape = "Tertile", color = "Gender", x= "Estimate")
  g <- g + guides(fill=FALSE)
  g
  # if(label_=="(R)-Error"){
  #   g + guides(fill=FALSE, color=FALSE)
  # }else{
  #   g + guides(fill=FALSE, color=FALSE, shape=FALSE)
  #   
  # }
  
} 

# ds_le %>% plot_le_1()


# ---- plotting-odds ------------------
plot_odds <- function(
  x,
  yaxis,
  limit1, 
  limit2
){
  # x <- ds_odds
  # study_ = levels_studies["octo"]
  # predictor_ = levels_predictors["age"]
  if(yaxis =="trans"){
    d <- x %>%
      dplyr::filter(study == limit1) %>% 
      dplyr::filter(predictor==limit2)
  }
  if(yaxis =="study"){
    d <- x %>%
      dplyr::filter(trans == limit1) %>% 
      dplyr::filter(predictor==limit2)
  }
  if(yaxis =="predictor"){
    d <- x %>%
      dplyr::filter(study == limit1) %>% 
      dplyr::filter(trans == limit2) 
  }
  
  g <-  ggplot2::ggplot(d,aes_string(y     = yaxis 
                                     ,x     = "est"
                                     # ,color = "sign"
                                     # ,fill  = "sign"
                                     # ,shape = "model_number"
  ))  
  g <- g + geom_vline(xintercept = 1, 
                      color      = "gray50",  
                      linetype   = "dotted", 
                      size       = 1)   
  g <- g + geom_errorbarh(aes_string(xmin = "low", xmax = "high"),
                          color    = "gray25", 
                          height   = 0, 
                          linetype = "solid", 
                          size     = 0.5) 
  g <- g + geom_point(size = 3, color="black") 
  # g <- g + facet_grid(predictor~study)
  g <- g + main_theme
  g
  # g <- g + scale_shape_manual(values = c("TRUE"=21,"FALSE"=42))
  # g <- g + scale_shape_manual(values = c("Linear"=24,"Quadratic"=22))
  # g <- g + scale_color_manual(values = c("TRUE"="black","FALSE"=NA))
  # g <- g + scale_fill_manual(values = c("TRUE"="black","FALSE"="white"))
  # g <- g + main_theme
  # g <- g + labs(shape = "", color="p < .05", fill = "p < .05")
  # g <- g + theme(axis.text.y = element_text(size=baseSize))
  # if(label_=="(R)-Error"){
  #   g + guides(fill=FALSE, color=FALSE)
  # }else{
  #   g + guides(fill=FALSE, color=FALSE, shape=FALSE)
  #   
  # }
  
}   
# trans - study - predictor
# study - trans - predictor
# predictor - study - trans
# ds_odds %>% plot_odds("trans", "octo", "age")
# ds_odds %>% plot_odds("study", "1-->4","age")
# ds_odds %>% plot_odds("predictor", "octo","1-->4")
# ---- matrix-of-odds ----------------------------
matrix_odds <- function(
  x,
  yaxis,
  mat_cols,
  mat_row
){
  lst <- list()
  for(i in seq_along(mat_cols)){
    lst[[i]] <- x %>% plot_odds(yaxis, mat_cols[i], mat_row )
  }
  pm <- GGally::ggmatrix(
    lst,
    nrow = 1, ncol = length(mat_cols),
    # title = "MMSE",
    xAxisLabels = names(mat_cols),
    yAxisLabels = mat_row
    # legend = 1
  ) + theme(
    legend.position = "right",
    strip.text.x = element_text(size=baseSize+3)
  )
  pm
  
} 
# matrix_odds(ds_odds,"predictor",lvl_studies,"1-->2")
# matrix_odds(ds_odds,"predictor",lvl_studies,"1-->4")
# matrix_odds(ds_odds,"predictor",lvl_studies,"2-->1")
# matrix_odds(ds_odds,"predictor",lvl_studies,"2-->3")
# matrix_odds(ds_odds,"predictor",lvl_studies,"2-->4")
# matrix_odds(ds_odds,"predictor",lvl_studies,"3-->4")
# 
# matrix_odds(ds_odds,"trans",lvl_studies,"age")
# matrix_odds(ds_odds,"trans",lvl_studies,"sex")
# matrix_odds(ds_odds,"trans",lvl_studies,"edu_med_low")
# matrix_odds(ds_odds,"trans",lvl_studies,"edu_high_low")
# matrix_odds(ds_odds,"trans",lvl_studies,"ses")
# 
# matrix_odds(ds_odds,"study",lvl_transitions,"age")
# matrix_odds(ds_odds,"study",lvl_transitions,"sex")
# matrix_odds(ds_odds,"study",lvl_transitions,"edu_med_low")
# matrix_odds(ds_odds,"study",lvl_transitions,"edu_high_low")
# matrix_odds(ds_odds,"study",lvl_transitions,"ses")

# ---- supermatrix-odds -------------------------
supermatrix_odds <- function(
  ls_graphs, 
  folder_name,
  plot_name,
  main_title,
  width, height,res
){
  
  lst <- ls_graphs
  # folder_name = "./reports/model-summaries/graphs/"
  # plot_name = "transition-by-predictor"
  # main_title = "Hazard ratios and 95% confidence intervals"
  # width = 1200
  # height= 1200
  # res = 120
  
  (n_g <- length(lst))
  (height_unit <- .95/n_g)
  (heights <- c(.05,rep(height_unit,n_g) ) )
  (unit_nulls <- rep("null",n_g))
  (path_save = paste0(folder_name,plot_name,".png"))
  # open device
  png(
    filename = path_save, 
    width    = width, 
    height   = height,
    res      = res
  )
  # 
  vpLayout <- function(rowIndex, columnIndex){ 
    return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) 
  }
  grid::grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(
    nrow    = n_g+1, 
    ncol    = 1,
    widths  = grid::unit(1 ,"null"),
    heights = grid::unit(heights, unit_nulls)
  )
  grid::pushViewport(grid::viewport(layout=layout))
  # print the main title
  grid::grid.text(
    main_title, 
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
    just = "left"
  )
  # print graphs
  for(i in seq_along(lst)){
    # i <- 1
    cstring <- paste0("print(lst[[",i,"]], vp=grid::viewport(layout.pos.row=",i+1, ", layout.pos.col=1))")
    eval(parse(text=cstring)) # evaluates the content of the command string
  }
  dev.off() # close device
  return(grid::popViewport(0))
}

# ---- raw_smooth_lines --------------------

raw_smooth_lines <- function(
  ds, 
  variable_name,
  line_size=.5, 
  line_alpha=.5,
  # top_y = max(ds[,variable_name]),  
  # bottom_y = min(ds[,variable_name]),  
  # by_y = round(top_y/10,0),
  bottom_age = 60,
  top_age = 100,
  bottom_time = 0,
  top_time = 20
){
  # d <- dto[["unitData"]]
  d <- ds
  # browser()
  g11 <- basic_line(d, variable_name, "age_at_visit", "black", line_alpha, line_size, F)
  g21 <- basic_line(d, variable_name, "age_at_visit", "salmon", line_alpha, line_size, T)
  g12 <- basic_line(d, variable_name, "fu_year", "black", line_alpha, line_size, F)
  g22 <- basic_line(d, variable_name, "fu_year", "salmon", line_alpha, line_size, T)
  
  # g13 <- basic_line(d, variable_name, "date_at_visit", "black", line_alpha, line_size, F)
  # g23 <- basic_line(d, variable_name, "fu_year", "salmon", line_alpha, line_size, T)
  # d_observed,
  # variable_name = "cogn_global",
  # time_metric, 
  # color_name="black",
  # line_alpha=.5,
  # line_size =.5, 
  # smoothed = FALSE,
  # main_title     = variable_name,
  # x_title        = paste0("Time metric: ", time_metric),
  # y_title        = variable_name,
  # rounded_digits = 0L
  # 
  
  
  
  g11 <- g11 + labs(x="Age at visit")
  g21 <- g21 + labs(x="Age at visit")
  
  g12 <- g12 + labs(x="Follow-up year")
  g22 <- g22 + labs(x="Follow-up year")
  
  # bottom_age <- 60
  # top_age <- 100
  # bottom_time <- 0
  # top_time <- 20
  
  g11 <- g11 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  g21 <- g21 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  
  g12 <- g12 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  g22 <- g22 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  
  # top_y <- 10
  # bottom_y <- 0
  # by_y <- 1
  
  # g11 <- g11 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g21 <- g21 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g12 <- g12 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g22 <- g22 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # 
  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=2,
                              widths=grid::unit(c(.5, .5) ,c("null","null")),
                              heights=grid::unit(c(.5, .5), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  print(g11, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1 ))
  print(g21, vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  
  print(g12, vp=grid::viewport(layout.pos.row=1, layout.pos.col=2 ))
  print(g22, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  
  grid::popViewport(0)
  
} 

# head(ds);
# raw_smooth_lines(ds, "cogn_global")
#   
#   # ds, 
#   variable_name= "cogn_global",
#   line_size=.5,
#   line_alpha=.5,
#   top_y = max(ds[,outcome]),
#   bottom_y = min(ds[,outcome]),
#   by_y = round(top_y/10,0),
#   bottom_age = 60,
#   top_age = 100,
#   bottom_time = 0,
#   top_time = 20
# # )


raw_smooth_lines_v2 <- function(
  ds, 
  variable_name,
  line_size=.5, 
  line_alpha=.5,
  # top_y = max(ds[,variable_name]),  
  # bottom_y = min(ds[,variable_name]),  
  # by_y = round(top_y/10,0),
  bottom_age = 60,
  top_age = 100,
  bottom_time = 0,
  top_time = 20
){
  # d <- dto[["unitData"]]
  d <- ds
  # browser()
  g11 <- basic_line_v2(d, variable_name, "age_at_visit", "black", line_alpha, line_size, F)
  g21 <- basic_line_v2(d, variable_name, "age_at_visit", "salmon", line_alpha, line_size, T)
  g12 <- basic_line_v2(d, variable_name, "fu_year", "black", line_alpha, line_size, F)
  g22 <- basic_line_v2(d, variable_name, "fu_year", "salmon", line_alpha, line_size, T)
  
  g13 <- basic_line_v2(d, variable_name, "date_at_visit", "black", line_alpha, line_size, F)
  g23 <- basic_line_v2(d, variable_name, "date_at_visit", "salmon", line_alpha, line_size, T)
  # d_observed,
  # variable_name = "cogn_global",
  # time_metric, 
  # color_name="black",
  # line_alpha=.5,
  # line_size =.5, 
  # smoothed = FALSE,
  # main_title     = variable_name,
  # x_title        = paste0("Time metric: ", time_metric),
  # y_title        = variable_name,
  # rounded_digits = 0L
  # 
  
  
  
  g11 <- g11 + labs(x="Age at visit")
  g21 <- g21 + labs(x="Age at visit")
  
  g12 <- g12 + labs(x="Follow-up year")
  g22 <- g22 + labs(x="Follow-up year")
  
  g13 <- g13 + labs(x="Date at visit")
  g23 <- g23 + labs(x="Date at visit")
  
  # bottom_age <- 60
  # top_age <- 100
  # bottom_time <- 0
  # top_time <- 20
  
  g11 <- g11 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  g21 <- g21 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  
  g12 <- g12 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  g22 <- g22 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  
  # top_y <- 10
  # bottom_y <- 0
  # by_y <- 1
  
  # g11 <- g11 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g21 <- g21 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g12 <- g12 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g22 <- g22 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # 
  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=3,
                              widths=grid::unit(c(.33, .33, .34) ,c("null","null", "null")),
                              heights=grid::unit(c(.5, .5), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  print(g11, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1 ))
  print(g21, vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  
  print(g12, vp=grid::viewport(layout.pos.row=1, layout.pos.col=2 ))
  print(g22, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  
  print(g13, vp=grid::viewport(layout.pos.row=1, layout.pos.col=3 ))
  print(g23, vp=grid::viewport(layout.pos.row=2, layout.pos.col=3 ))
  
  grid::popViewport(0)
  
} 



build_le_matrix_4 <- function(
  ds, 
  variable_name,
  line_size=.5, 
  line_alpha=.5,
  # top_y = max(ds[,variable_name]),  
  # bottom_y = min(ds[,variable_name]),  
  # by_y = round(top_y/10,0),
  bottom_age = 60,
  top_age = 100,
  bottom_time = 0,
  top_time = 20
){

   plot.elect(le)
  le
  ds <- describe_replication(le)
  ds
  
  g11 <- ggplot2::ggplot(ds) + geom_text()
  
  g11 <- basic_line(d, variable_name, "age_at_visit", "black", line_alpha, line_size, F)
  g21 <- basic_line(d, variable_name, "age_at_visit", "salmon", line_alpha, line_size, T)
  g12 <- basic_line(d, variable_name, "fu_year", "black", line_alpha, line_size, F)
  g22 <- basic_line(d, variable_name, "fu_year", "salmon", line_alpha, line_size, T)
  

  
  
  
  g11 <- g11 + labs(x="Age at visit")
  g21 <- g21 + labs(x="Age at visit")
  
  g12 <- g12 + labs(x="Follow-up year")
  g22 <- g22 + labs(x="Follow-up year")
  
  # bottom_age <- 60
  # top_age <- 100
  # bottom_time <- 0
  # top_time <- 20
  
  g11 <- g11 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  g21 <- g21 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=5), limits=c(bottom_age,top_age))
  
  g12 <- g12 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  g22 <- g22 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=5), limits=c(bottom_time,top_time))
  
  # top_y <- 10
  # bottom_y <- 0
  # by_y <- 1
  
  # g11 <- g11 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g21 <- g21 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g12 <- g12 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # g22 <- g22 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y), limits=c(bottom_y,top_y))
  # 
  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=2,
                              widths=grid::unit(c(.5, .5) ,c("null","null")),
                              heights=grid::unit(c(.5, .5), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  print(g11, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1 ))
  print(g21, vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  
  print(g12, vp=grid::viewport(layout.pos.row=1, layout.pos.col=2 ))
  print(g22, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  
  grid::popViewport(0)
  
} 
