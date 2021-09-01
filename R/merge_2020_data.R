#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param YieldFile
#' @param NIRFile
merge_2020_data <- function(YieldFile = YieldFile_2020, NIRFile = NIRFile_2020) {

  # Read in the yield and NIR files
  yield_2020 <- read_csv(YieldFile)
  nir_2020 <- read_csv(NIRFile)

  # And join these two dataframes togetherwith the ID columns
  all_2020 <- left_join(yield_2020, 
                        nir_2020, 
                        by = c("Test", "Loc", "Genotype", "Code", "Rep", "Year", "Plot"))
  
  return(all_2020)
}
