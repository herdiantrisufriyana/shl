#' Get set for training or testing
#'
#' Get set for training or testing specifically for this PROM study.
#'
#' @param data Training or testing set, an ExpressionSet that contains outcome 
#' in the phenotype dataframe.
#'
#' @details
#' This function is highly contextual for this PROM study.
#' 
#' @return A training or testing set, a dataframe with uncensored outcome and 
#' features.
#' 

get_set=function(data){
  data %>%
    
    # Get features.
    exprs() %>%
    t() %>%
    as.data.frame() %>%
    
    # Get the outcome.
    cbind(
      data %>%
        pData() %>%
        mutate(
          outcome2=ifelse(censoring,NA,as.character(outcome))
          ,outcome=factor(outcome2,levels(outcome))
        ) %>%
        select(outcome)
    ) %>%
    select(outcome,everything()) %>%
    
    # Filter only uncensored outcome.
    filter(!is.na(outcome))
}