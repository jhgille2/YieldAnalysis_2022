##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param BalancedData
explore_BalancedData <- function(BalancedData) {

  # A quick summary of the data structure. Shows how many observations are present 
  # for each of the numeric variables/how many levels are present for each factor, 
  # and gives some basic summary statistics
  BasicSummary <- inspect(BalancedData, verbose = FALSE) %>%
    kbl(caption = "Basic summary data") %>%
    kable_styling(full_width = F, position = "left")
    
  
  # Which genotype had the highest score for each phenotype in each environment
  GenoWinners <- BalancedData %>%
    ge_winners(ENV, GEN, resp = everything()) %>%
    gt() %>%
    tab_header(title = "Winning genotypes for each trait in each environment.")
  
  # Plots showing the genotype x environment scores for yield, protein, and oil
  #TODO: Modify the ge_plot to produce a more compact, but still readable plot (Make a function for it)
  YieldPlot   <- ge_plot(BalancedData, GEN, ENV, Yield) + theme(axis.text.x = element_blank(), 
                                                                 axis.ticks.x = element_blank(), 
                                                                 axis.title.y = element_blank(),
                                                                 axis.title.x = element_blank())
  
  ProteinPlot <- ge_plot(BalancedData, GEN, ENV, Protein) + theme(axis.text.x = element_blank(),
                                                                   axis.ticks.x = element_blank(), 
                                                                   axis.title.x = element_blank()) + labs(y = "Environment")
  
  OilPlot     <- ge_plot(BalancedData, GEN, ENV, Oil) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
                                                               axis.title.y = element_blank()) + labs(x = "Genotype")
  
  # Combine all three into a single plot with patchwork
 GenoByEnvironmentPlot <- YieldPlot / ProteinPlot / OilPlot
 
 # A barplot version of the above plot
 Yield_BarPlot <- BalancedData %>%
   mutate(GEN = fct_reorder(GEN, Yield, .desc = TRUE)) %>%
   plot_factbars(GEN, ENV, resp = Yield)
 
 Yield_BarPlot <- Yield_BarPlot + theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
 
 # A function to make histograms for oil, protein, and yield
 MainTraitsHist <- function(Balanced_Data = BalancedData){
   
   Balanced_Data %>%
     dplyr::select(GEN, ENV, Yield, Oil, Protein) %>%
     pivot_longer(cols = 3:5, names_to = "Variable") -> PlotData
   
   
   Plots_BW <- ggplot(PlotData, aes(x = value)) + 
     geom_histogram(bins = 15, colour = 'black', fill = 'light gray') + 
     theme_few() + 
     facet_wrap(~Variable, scales = "free", ncol = 1) + 
     labs(x = "Value")
   
   
   Plots_colour <- ggplot(PlotData, aes(x = value, fill = ENV)) + 
     geom_histogram(bins = 15, colour = 'black') + 
     theme_few() + 
     theme(axis.text.y  = element_blank(),
           axis.ticks.y = element_blank(),
           axis.title.y = element_blank()) + 
     facet_wrap(~Variable, scales = "free", ncol = 1) + 
     labs(x = "Value", y = "Count")
   
   Plots_BW + Plots_colour
 }
 
 MainTraitsPlot <- MainTraitsHist(BalancedData)
 
 CorrelationPlot <- corr_coef(BalancedData) %>% plot()
 
 
 return(list("BasicSummary"          = BasicSummary,
             "GenoByEnvironmentPlot" = GenoByEnvironmentPlot,
             "YieldBarPlot"          = Yield_BarPlot,
             "CorrelationPlot"       = CorrelationPlot,
             "GenoWinners"           = GenoWinners,
             "MainTraitHistograms"   = MainTraitsPlot))

}
