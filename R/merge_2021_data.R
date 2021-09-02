#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param OldData
#' @param NewData
merge_2021_data <- function(OldData = BalancedData, NewData = Yield2021) {

  # Bind the new data to the old data
  MergedData <- bind_rows(OldData, NewData)
  
  return(MergedData)
}
