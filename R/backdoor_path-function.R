#' A single causal diagram table
#'
#' Make a single causal diagram table using edge tables of baseline and 
#' measured variables given causal factor(s) of interest and an outcome.
#'
#' @param baseline_edge Baseline variables, a dataframe with 'from' and 'to' 
#' columns.
#' @param measure_edge Measured variables, a dataframe with 'from' and 'to' 
#' columns, which are from the baseline to the measured variables.
#' @param causes Causal factor, a character vector of the causal factor(s) of 
#' interest.
#' @param outcome Outcome, a character of an outcome.
#'
#' @details
#' None.
#' 
#' @return A single causal diagram table, a dataframe with 'from' and 'to' 
#' columns.
#' 

backdoor_path=function(baseline_edge,measure_edge,causes,outcome){
  
  # A function to get any backdoor pathway
  backdoor_pathway=function(edge_table,causes,outcome){
    lapply(causes,function(cause){
      edge_table %>%
        filter(
          (from==cause & to==outcome)
          |to==cause
          |(from%in%filter(.,to==cause)$from & to==outcome)
        )
    }) %>%
      do.call(rbind,.) %>%
      .[!duplicated(.),]
  }
  
  # Get the backdoor pathways.
  baseline_edge %>%
    backdoor_pathway(causes,outcome) %>%
    
    # Combine with the measurement pathways.
    rbind(
      backdoor_pathway(
        measure_edge
        ,c(.$from,.$to) %>%
          .[!duplicated(.)] %>%
          paste0('*')
        ,paste0(outcome,'*')
      )
    )
  
}