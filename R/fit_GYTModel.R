##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param BalancedData
##' @param TraitSelection
fit_GYTModel <- function(BalancedData, TraitSelection = c("Protein", "Oil"), wt = c(1, 1), ideo = c('h', 'h')) {

  # Fit a model that includes just protein and oil
  GYTMod <- gytb(BalancedData, 
                 gen       = GEN, 
                 yield     = Yield, 
                 traits    = all_of(TraitSelection),
                 ideotype  = all_of(ideo),
                 svp       = "genotype",
                 scaling   = 1, 
                 centering = 2,
                 weight    = all_of(wt))
  
  # Fit a model that includes lodging as well
  GYTMod_ag <- gytb(BalancedData, 
                  gen       = GEN, 
                  yield     = Yield, 
                  traits    = c("Protein", "Oil", "ag_score", "LOD"),
                  ideotype  = c("h", "h", "l", "l"),
                  svp       = "genotype",
                  scaling   = 1, 
                  centering = 2,
                  weight    = c(1, 1, 0.5, 0.5))
  
  
  # Get the superiority indices from the model
  SITable    <- get_model_data(GYTMod, what = "si")
  SITable_ag <- get_model_data(GYTMod_ag, what = "si")
  
  SITable_ag %>%
    arrange(desc(SI)) %>%
    kbl(caption = "Superiority Indices drived from the GYT biplot for yield, seed oil, seed protein, lodging, and agronomic score.", digits = 2) %>%
    kable_classic(full_width = FALSE, html_font = "Cambria") %>%
    kable_styling(position = "left") -> Formatted_SITable_ag
  
  # Return a list of everything
  return(list("Model"             = GYTMod, 
              "Model_ag"          = GYTMod_ag,
              "SI_Table"          = SITable,
              "SI_Table_ag"       = SITable_ag,
              "Formatted_SITable" = Formatted_SITable_ag))
}
