##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param BalancedData
fit_MixedModels <- function(BalancedData, TraitSelection = c("Yield", "Oil", "Protein")) {

  # Fit LMMs for the traits in TraitSelection
  MixedModels <- gamem_met(BalancedData, 
                           env     = ENV, 
                           gen     = GEN, 
                           rep     = REP, 
                           resp    = TraitSelection, 
                           verbose = FALSE, 
                           random  = "all")
  
  # Metan weighted average of absolute scores scheme
  WAAS_Models <- waasb(BalancedData, 
                       env     = ENV, 
                       gen     = GEN, 
                       rep     = REP, 
                       resp    = TraitSelection,
                       verbose = FALSE)
  
  # Extract various components and format here for easier presentation
  
  # The BLUPs
  ModelBLUPs <- get_model_data(MixedModels, "blupg")
  
  ModelBLUPs %>%
    arrange(desc(Yield)) %>%
    kbl(caption = "BLUPs for selected traits.", digits = 2) %>%
    kable_classic(full_width = FALSE, html_font = "Cambria") %>%
    kable_styling(position = "left") -> Formatted_BLUPs
  
  # Genetic paramaters
  ModelGenPar <- get_model_data(MixedModels, "genpar")
  
  ModelGenPar %>%
    kbl(caption = "Genetic paramaters for selected traits.", digits = 2) %>%
    kable_classic(full_width = FALSE, html_font = "Cambria") %>%
    kable_styling(position = "left") -> Formatted_GENPAR
  
  # Combine all the components into a list fo easy reference
  ResultList <- list("BLUP"          = ModelBLUPs,
                     "GENPAR"        = ModelGenPar,
                     "Format_BLUP"   = Formatted_BLUPs,
                     "Format_GENPAR" = Formatted_GENPAR,
                     "ModelObjects"  = MixedModels,
                     "WAASModels"    = WAAS_Models)
  
  ResultList
}
