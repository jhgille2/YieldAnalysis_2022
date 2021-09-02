##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param GGEModels
plot_GGEModels <- function(GGEModels) {

  # The names associated with the numeric biplot types
  Biplot_type_names <- c("Basic" = 1,
                         "Mean performance vs stability" = 2,
                         "Which-won-where" = 3,
                         "Discriminativeness vs, representativeness" = 4,
                         "Examine an environment" = 5,
                         "Rank Environments" = 6,
                         "Examine a genotype" = 7,
                         "Rank genotypes" = 8,
                         "Compare two Genotypes" = 9,
                         "Relationship among environments" = 10)
  
  # For this study, I'm most interested in types 2, and 8. 
  
  # A plotting function to return a single plot for all traits contained
  # in a gge model object
  GGE_Plot <- function(type = 2, GGEModel){
    
    # Get the names of the traits contained within the ggemodel object
    TraitNames <- names(GGEModel)
    
    # Make a list to hold all the plots
    PlotList        <- vector("list", length = length(GGEModel))
    names(PlotList) <- TraitNames
    
    for(i in 1:length(PlotList)){
        PlotList[[i]] <- plot(GGEModel, type = type, var = i, size.text.gen = 3)
    }
    
    # Combine the biplots for each trait into a single image
    arrange_ggplot(PlotList, tag_levels = list(TraitNames))
  }
  
  Type2Plot <- GGE_Plot(GGEModel = GGEModels)
  Type8Plot <- GGE_Plot(type = 8, GGEModel = GGEModels)
  
  # Arrange both plots into a single plot with patchwork
  AllTypes <- Type2Plot / Type8Plot
  
  AllTypes[[1]] <- AllTypes[[1]] + plot_layout(tag_level = 'new')
  AllTypes[[2]] <- AllTypes[[2]] + plot_layout(tag_level = 'new')
  
  Type2_8_GGEPlot <- AllTypes + plot_annotation(tag_levels = c('A'))
  
  return(list("Type_2_and_8" = Type2_8_GGEPlot))
}
