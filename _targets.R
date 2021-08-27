## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # Flow chart diagrams for the "Plan" document
  tar_target(Plan_Flowcharts, 
             make_plan_flowcharts()),

  tar_render(Plan, "docs/Plan.Rmd"),
  
  tar_render(Writeup, "docs/Writeup.Rmd")

)
