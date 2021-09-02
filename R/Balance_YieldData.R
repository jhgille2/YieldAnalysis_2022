##' .. content for \description{} (no empty lines) ..
##' Some lines were added to the 2020 data, mainly checks
##' and one line was entered twice. For analysis across years and environments
##'  I need the set of lines that were grown at all locations in both years. This
##'  function takes the full yield data and returns the balanced data.
##' .. content for \details{} ..
##'
##' @title
##' @param YieldData
Balance_YieldData <- function(YieldData) {

  # Three conditions to resolve
  # 1. An entry that was added twice in 2020 data (N18-1632). Labeled as N18-1632-1 and N18-1632-2
  #    - Average reps within years/location
  # 2. Entries that were added in 2020 but weren't included in 2019 
  #    - Remove these
  # 3. Entries that were included in both tests (the parents).
  #    - AVerage across reps within years/location
  
  DuplicateReps <- c("N18-1632-1", "N18-1632-2")
  MeasureVars   <- c("yield", "oil", "protein", "protein_plus_oil", "ht", "ag_score", "lod", "sdwt", "estimated_seed_count")
   
  YieldData %>% dplyr::filter(genotype %in% DuplicateReps) %>% 
    mutate(genotype = str_sub(genotype, 1, 8)) %>% 
    group_by(genotype, year, rep, ENV) %>% 
    mutate_at(MeasureVars, mean) %>% 
    sample_n(1) %>%
    ungroup() -> RepeatedEntries_2020 
  
  # Remove these entries from the full data
  CleanData <- YieldData %>% dplyr::filter(genotype != DuplicateReps)
  
  
  YieldData %>% 
    separate(ENV, into = c("LOC", "YEAR"), sep = "-") %>%
    group_by(genotype, YEAR) %>% 
    count(name = "TotalEntries") %>%  # How many entries are present in each year
    group_by(genotype) %>% 
    mutate(YearCount = n()) %>%       # In how many years is the genotype present
    ungroup() -> CountSummary
  
  # The entries added in 2020 that aren't present in 2019
  CountSummary %>%
    dplyr::filter(YearCount == 1, 
                  genotype != "N18-1632") -> AddedEntries
  
  # remove these entries
  CleanData <- CleanData %>% 
    dplyr::filter(!(genotype %in% AddedEntries$genotype))
  
  # Average the parent data
  Parents <- c("LMN09-119", "N09-09")
  YieldData %>% 
    dplyr::filter(genotype %in% Parents) %>%
    group_by(genotype, rep, ENV) %>%
    mutate_at(MeasureVars, mean) %>%
    sample_n(1) %>%
    ungroup() -> ParentAverages
  
  # Colummns to convert to factor
  Factor_cols <- c('genotype', 'ENV', 'rep')

  # Bind the cleaned parent and duplicated entry data back to the full data set
  CleanData %>%
    dplyr::filter(!(genotype %in% Parents)) %>%
    bind_rows(ParentAverages) %>%
    bind_rows(RepeatedEntries_2020) %>%
    mutate_at(Factor_cols, metan::as_factor) %>%
    mutate_if(is.factor, droplevels) %>%
    select(genotype, 
           rep, 
           ENV, 
           ag_score,
           ht,
           lod,
           sdwt,
           estimated_seed_count,
           yield, 
           oil,
           protein) %>%
    rename(GEN                  = genotype,
           REP                  = rep,
           HT                   = ht,
           LOD                  = lod,
           SDWT                 = sdwt,
           Estimated_Seed_Count = estimated_seed_count,
           Yield                = yield, 
           Oil                  = oil, 
           Protein              = protein) -> CleanData
  
  CleanData$ENV <- factor(CleanData$ENV, levels = c("PLY-2019", "CAS-2019", "CAS-2020", "CLA-2020"))
  
  CleanData
}
