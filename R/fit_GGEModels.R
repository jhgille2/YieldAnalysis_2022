##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param BalancedData
##' @param TraitSelection
fit_GGEModels <- function(BalancedData, TraitSelection = c("Yield")) {

  gge_model <- gge(BalancedData, 
                   env  = ENV, 
                   gen  = GEN, 
                   resp = TraitSelection)
  
  gge_model
}
