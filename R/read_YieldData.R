##' Read in the .csv files for the 2019 and 2020 yield data for the "Jay" yield tests.
##' This function reads in both files, filters genotypes from the 2019 data so that it
##' only contains the entries which were also grown in 2020, cleans each data set 
##' so that they share a set of column names and variable types, and combines them
##' to a single file. 
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Data_2019
##' @param Data_2020
##' @return A single data set ready for analysis with the metan package
read_YieldData <- function(Data_2019    = file_in(!!Yield_2019), 
                           Data_2020    = file_in(!!Yield_2020),
                           NIRData_2020 = file_in(!!NIR_2020)) {

  # Read in the data from their files
  Data2019 <- read_csv(Data_2019)
  Data2020 <- read_csv(Data_2020)
  NIR2020  <- read_csv(NIRData_2020)
  
  # Add the NIR data from 2020 to the 2020 field data
  Data2020 <- left_join(Data2020, NIR2020)
  
  # Keep only the entries that are included in 2020, 
  # after pulling aside the N18-1632 entry data
  # and keep only the checks from one of the tests
  Data2019 <- Data2019 %>%
    dplyr::filter(Genotype %in% c(unique(Data2020$Genotype), "N18-1632"),
                  !(Genotype %in% c("Dunphy", "Osage") & Test %in% c("Yield Test 2", "Yield Test 3", "Yield Test 4"))) %>% 
    clean_names() %>%
    mutate(loc = toupper(loc),
           ENV = paste(loc, year, sep = "-")) %>%
    select(genotype, 
           year,
           rep, 
           ENV,
           plot,
           ag_score,
           ht,
           lod,
           sdwt,
           estimated_seed_count,
           yield,
           oil, 
           protein, 
           protein_plus_oil)
  
  
  Data2020 <- Data2020 %>%
    clean_names() %>%
    mutate(loc = toupper(loc),
           ENV = paste(loc, year, sep = "-"),
           estimated_seed_count = (yield/sdwt)*100) %>%
    select(genotype, 
           year,
           rep,
           ENV,
           plot, 
           ag_score,
           ht,
           lod,
           sdwt,
           estimated_seed_count,
           yield,
           oil,
           protein,
           protein_plus_oil)
  
  AllData <- bind_rows(Data2019, Data2020)
  
  # Colummns to convert to factor
  Factor_cols <- c('genotype', 'ENV', 'rep')

  # Create a set of data that does not include rep 4 as tests were frown in three reps in 2019
  # and keep only protein, oil, and yield
  AllData_ThreeReps <- AllData %>% 
    dplyr::filter(rep != 4) %>%
    mutate_at(Factor_cols, metan::as_factor) %>%
    select(genotype, 
           year,
           rep,
           ENV,
           plot, 
           yield,
           oil,
           protein,
           protein_plus_oil)
  
  # Datasets that keep agronomic notes as well
  AllData %>%
    mutate_at(Factor_cols, metan::as_factor) -> AllData_WithAg
  
  AllData %>%
    mutate_at(Factor_cols, metan::as_factor) %>%
    dplyr::filter(rep != 4) -> AllData_WithAg_ThreeReps
  
  
  AllData %>%
    mutate_at(Factor_cols, metan::as_factor) %>%
    select(genotype, 
           year,
           rep,
           ENV,
           plot, 
           yield,
           oil,
           protein,
           protein_plus_oil) -> AllData
  
  return(list("All"                  = AllData, 
              "ThreeReps"            = AllData_ThreeReps, 
              "All_WithAg"           = AllData_WithAg, 
              "All_WithAg_ThreeReps" = AllData_WithAg_ThreeReps))
}
