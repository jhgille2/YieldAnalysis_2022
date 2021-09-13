# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packaged
pacman::p_load(conflicted, 
               dotenv, 
               targets, 
               tarchetypes, 
               tidyverse, 
               DiagrammeR, 
               rmarkdown, 
               lme4, 
               broom.mixed, 
               faux, 
               here, 
               janitor, 
               ggcorrplot, 
               patchwork,
               gt, 
               ggthemes, 
               tufte, 
               kableExtra, 
               ggrepel, 
               hrbrthemes, 
               emmeans, 
               metan, 
               bookdown, 
               officer, 
               officedown)




