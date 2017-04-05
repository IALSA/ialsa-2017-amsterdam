rm(list=ls(all=TRUE)) # clear environment
cat("\f") # clear console 


# Arrival : All data land on Ellis Island
knitr::stitch_rmd(
  script="./manipulation/map/0-ellis-island-map.R", 
  output="./manipulation/map/stitched-output/0-ellis-island-map.md"
)
# look into knitr::spin() http://www.r-bloggers.com/knitrs-best-hidden-gem-spin/

# Development : Encoding of the multi-state (ms) measure(s)
knitr::stitch_rmd(
  script="./manipulation/map/1-encode-multistate-mmse.R",
  output="./manipulation/map/stitched-output/1-encode-multistate-mmse.md"
)
 
