## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  ## Section: File definitions
  ##################################################
  tar_target(YieldFile_2020, 
             here("data", "YieldData_2020.csv"), 
             format = "file"), 
  
  tar_target(NIRFile_2020, 
             here("data", "NIRData_2020.csv"), 
             format = "file"),
  
  tar_target(YieldFile_2019,
             here("data", "YieldData_2019.csv"), 
             format = "file"), 
  
  # Read in the yield data from 2020 and 2019
  tar_target(YieldData,
             read_YieldData(Data_2019    = YieldFile_2019, 
                            Data_2020    = YieldFile_2020,
                            NIRData_2020 = NIRFile_2020)),
  
  # Clean the data to make a balanced set of data 
  # Basically, remove lines from 2020 that aren't in the 2019 data
  tar_target(BalancedData, 
             Balance_YieldData(YieldData = YieldData$All_WithAg_ThreeReps)),
  
  # Simulate a dataset for the 2021 data
  tar_target(Yield2021, 
             simulate_yield_2021(OldYield = BalancedData)),
  
  tar_target(Merged2021, 
             merge_2021_data(OldData = BalancedData, NewData = Yield2021)), 
  
  # Make summary tables and plots for EDA
  ExploratoryAnalysis = explore_BalancedData(Merged2021),
  
  # Fit LMMs to the balanced data for the traits included in "TraitSelection"
  MixedModels = fit_MixedModels(Merged2021, 
                                TraitSelection = c("Yield", "Oil", "Protein")),
  
  # GGE models
  GGEModels = fit_GGEModels(Merged2021, TraitSelection = c("Yield")),
  
  # Plots derived from the gge model
  GGEPlots = plot_GGEModels(GGEModels),
  
  # Genotype by yield*trait plot
  GYTModel = fit_GYTModel(Merged2021, TraitSelection = c("Protein", "Oil"), wt = c(1, 1), ideo = c('h', 'h')),
  
  # General plots of the BLUPs
  BLUP_Plots = plot_BLUPs(MixedModels),
  
  MTSI_Plots = calc_MTSI(Merged2021), 
  
  # Merge the data from 2020
  tar_target(Yield2020,
             merge_2020_data(YieldFile = YieldFile_2020, NIRFile = NIRFile_2020)),

  tar_render(Plan, "docs/Plan.Rmd"),
  
  tar_render(Writeup, "docs/Writeup.Rmd")

)
