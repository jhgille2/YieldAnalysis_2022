##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nameme1
calc_MTSI <- function(BalancedData, 
                      WAASTraits = c("Yield", "Oil", "Protein", "LOD")) {
  
  # SHow the effect of changing wresp
  Multiple_WAAS <- function(data = BalancedData, RespTraits = WAASTraits, start = 50, end = 100){
    # Sequence of wresps
    wresp_seq <- seq(start, end, 10)
    
    AllModels        <- vector('list', length = length(wresp_seq))
    names(AllModels) <- as.character(wresp_seq)
    for(i in 1:length(AllModels)){
      AllModels[[i]] <- waasb(BalancedData, 
                              env     = ENV, 
                              gen     = GEN, 
                              rep     = REP, 
                              resp    = all_of(RespTraits),
                              mresp = c('h', 'h', 'h', 'l'),
                              wresp = wresp_seq[[i]], 
                              verbose = FALSE)
    }
    AllModels
  }
  
  return(Multiple_WAAS())
}
