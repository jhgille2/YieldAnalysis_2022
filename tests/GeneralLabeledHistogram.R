library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(ggthemes)

# A (fairly) general plot for plotting a histogram(s) of univariate (multivariate) data with the positions of specific 
# observations on each distribution labeled with an arrow

LabeledHistogram <- function(data, measure.vars = c("Protein", "Oil", "Yield"), labelColumn = "Genotype", labelObservations = c("LMN09-119", "N09-09")){
  
  # Pivot data to a long format
  longData <- data %>% 
    pivot_longer(measure.vars)
  
  # The data for the "special" observations that need to be labeled
  specialObservations <- longData %>%
    dplyr::filter(!!sym(labelColumn) %in% labelObservations)
  
  # Split the dataframe by variable
  longData_split <- split(longData, longData$name)
  
  # Do the same for the special data
  specialObservations_split <- split(specialObservations, specialObservations$name)
  
  
  # The base plotting function. Takes as an argument a dataframe 
  # containing measurements for a single phenotype (like AllPheno_long$oil)
  Plot_initial <- function(TraitData = longData_split$Oil, ParentData = specialObservations_split$Oil, TraitName = "Value"){
    
    # Get the initial plot
    Plot.init <-   ggplot(TraitData, aes(x = value)) + 
      geom_histogram(color = "black", alpha = 0.6, bins = 10) + 
      theme_bw() + 
      ylab("Count") + 
      theme_ipsum() + 
      theme(axis.text.x  = element_text(face = "bold", size = 20),
            axis.text.y  = element_text(face = "bold", size = 20),
            axis.title.y = element_text(size = 25, hjust = 0.5),
            axis.title.x = element_text(size = 25, hjust = 0.5),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.title = element_text(face = "bold", size = 20),
            legend.text  = element_text(face = "bold", size = 20)) +
      labs(x = TraitName)
    
    # Data from the initial plot (I want the bin heights)
    PlotData <- ggplot_build(Plot.init)$data[[1]]
    
    # Using the bin counts from the plot, find the y-value where the labels for each
    # check/parent genotype shoud start
    ParentData$yval <- NA
    for(i in 1:nrow(ParentData)){
      ParentData$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < ParentData$value[[i]]))]]
    }
    
    # Add labels w/arrows for the parents/checks using this new data
    FinalPlot <- Plot.init + 
      ggrepel::geom_label_repel(data = ParentData,
                                aes(x = value, y = yval, label = !!sym(labelColumn)),
                                nudge_y            = max(PlotData$count)/6,
                                arrow              = arrow(length = unit(0.015, "npc")),
                                min.segment.length = 1,
                                size               = 7,
                                show.legend        = FALSE,
                                inherit.aes        = FALSE)
    
    FinalPlot
  }
  
  AllPlots <- vector("list", length = length(longData_split))
  names(AllPlots) <- names(longData_split)
  for(Trait in names(longData_split)){
    AllPlots[[Trait]] <- Plot_initial(TraitData = longData_split[[Trait]], ParentData = specialObservations_split[[Trait]], TraitName = Trait)
  }
  
  #PatchPlot <- wrap_plots(AllPlots)
  PatchPlot <- AllPlots$Yield | AllPlots$Oil | AllPlots$Protein
}