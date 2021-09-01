## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  ## Section: File definitions
  ##################################################
  tar_target(YieldFile_2020, 
             here("data", "YieldData_2020.csv")), 
  
  tar_target(NIRFile_2020, 
             here("data", "NIRData_2020.csv")),
  
  # Merge the data from 2020
  tar_target(Yield2020,
             merge_2020_data(YieldFile = YieldFile_2020, NIRFile = NIRFile_2020)),

  tar_render(Plan, "docs/Plan.Rmd"),
  
  tar_render(Writeup, "docs/Writeup.Rmd")

)
