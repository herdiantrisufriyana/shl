#' Compute the number of visits and subjects
#'
#' Summarize the number of visits and subjects per outcome.
#'
#' @param input Input, a dataframe of visits with columns of outcome and 
#' subject_id. The outcome should be event and nonevent.
#' @param dataset Name of dataset, a character of the name.
#'
#' @details
#' None.
#' 
#' @return Summary table, a dataframe of the number of visits and subjects.
#' 

outcome_visit_subject=function(input,dataset){
  input %>%
    lapply(X=1:2,Y=.,function(X,Y){
      
      if(X==1){
        # Number of visits
        group_by(Y,outcome) %>%
          summarize(n=n()) %>%
          spread(outcome,n) %>%
          mutate(total=event+nonevent) %>%
          mutate(type='visit')
      }else{
        # Number of subjects
        select(Y,subject_id,outcome) %>%
          .[!duplicated(.),] %>%
          group_by(outcome) %>%
          summarize(n=n()) %>%
          spread(outcome,n) %>%
          mutate(total=event+nonevent) %>%
          mutate(type='subject')
      }
    }) %>%
    do.call(rbind,.) %>%
    mutate(dataset=dataset)
}