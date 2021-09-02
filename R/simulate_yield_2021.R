#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param OldYield
simulate_yield_2021 <- function(OldYield = Yield2020) {

  # Just add some random noise to the measurement columns
  MeasureVars <- c("Yield", "SDWT", "LOD", "HT", "ag_score", "Oil", "Protein")

  # A function that gets the sd, mean, and length of a normal random variable and
  # simulates from a normal distribution with the same mean and sd
  sim_normal <- function(randVar){
    
    SD   <- sd(randVar, na.rm = TRUE)
    MEAN <- mean(randVar, na.rm = TRUE)
    N    <- length(randVar)
    
    newvar <- rnorm(n = N, mean = MEAN, sd = SD)
    return(newvar)
  }
  
  Data_2020 <- OldYield %>% 
    dplyr::filter(ENV %in% c("CLA-2020", "CAS-2020"))
  
  Yield_new <- Data_2020 %>% 
    group_by(ENV) %>%
    mutate(across(all_of(MeasureVars), sim_normal), 
           ag_score = (ag_score/0.5)*0.5, 
           Estimated_Seed_Count = (Yield/SDWT)*1000, 
           ENV = str_replace(ENV, "2020", "2021"))

  return(Yield_new)
}
