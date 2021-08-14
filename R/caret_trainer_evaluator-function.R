#' Caret training and evaluation for classification task
#'
#' Train, calibrate, and evaluate a caret model in a single pipeline.
#'
#' @param data Whole set, an ExpressionSet that includes training, calibration, 
#' and testing sets.
#' @param method Caret algorithm for training, a character of the algorithm 
#' names which are glmnet, Rborist, or gbm.
#' @param calib_method Caret algorithm for calibrating, a character of the 
#' algorithm names which are glm or gamLoess.
#' @param tuning Tuning parameters, a dataframe consisting all combinations of 
#' tuning parameters.
#' @param training Training strategy, a dataframe consisting details on the 
#' training strategy, e.g., internal validation method, etc.
#' @param title Title for log, a character of the title.
#' @param dir Path for saving the results, a character of the folder path.
#' @param file File name prefix, a character of the file name prefix.
#' @param cl Number of cluster, an integer for the number of CPU cluster used 
#' for all of the processes.
#'
#' @details
#' This function is highly contextual for this PROM study, especially for the 
#' \code{data} and the \code{training}.
#' 
#' @return Various results, a list of objects for the model (caret object),
#' calibrating model (caret object), and evaluation results (evalm object).
#' 

caret_trainer_evaluator=function(data
                                 ,method=c('glmnet','Rborist','gbm')
                                 ,calib_method=c('glm','gamLoess')
                                 ,tuning
                                 ,training
                                 ,title
                                 ,dir='.'
                                 ,file=NULL
                                 ,cl=1){
  
  # Print message for reproducible logging.
  cat(title,'\n')
  cat('Started:',as.character(now()),'\n')
  pb=startpb(0,5)
  on.exit(closepb(pb))
  setpb(pb,0)
  
  # Create multiple CPU clusters for parallel computing.
  cl=makePSOCKcluster(cl)
  registerDoParallel(cl)
  
  # Bring these packages to the CPU clusters.
  if(method=='glmnet'){
    clusterEvalQ(cl,{
      library('parallel')
      library('doParallel')
      library('tidyverse')
      library('pbapply')
      library('caret')
      library('glmnet')
      library('gam')
    })
  }else if(method=='Rborist'){
    clusterEvalQ(cl,{
      library('parallel')
      library('doParallel')
      library('tidyverse')
      library('pbapply')
      library('caret')
      library('Rborist')
      library('gam')
    })
  }else if(method=='gbm'){
    clusterEvalQ(cl,{
      library('parallel')
      library('doParallel')
      library('tidyverse')
      library('pbapply')
      library('caret')
      library('gbm')
      library('gam')
    })
  }
  
  # Hyperparameter tuning
  setpb(pb,1)
  suppressWarnings(set.seed(33,sample.kind=sample.kind))
  model=
    suppressWarnings(caret::train(
      outcome~.
      ,data=
        data %>%
        .[,pData(protocolData(.))$int] %>%
        get_set() %>%
        .[rownames(.)%in%training$pre_calib_set,,drop=F]
      ,method=method
      ,weights=
        training$outcome_weights %>%
        .[rownames(.)%in%training$pre_calib_set,,drop=F] %>%
        pull(weight)
      ,metric='ROC'
      ,trControl=training$tuning_trControl
      ,tuneGrid=tuning
    ))
  
  # Model training
  setpb(pb,2)
  model=
    suppressWarnings(caret::train(
      outcome~.
      ,data=
        data %>%
        .[,pData(protocolData(.))$int] %>%
        get_set() %>%
        .[rownames(.)%in%training$pre_calib_set,,drop=F]
      ,method=method
      ,weights=
        training$outcome_weights %>%
        .[rownames(.)%in%training$pre_calib_set,,drop=F] %>%
        pull(weight)
      ,metric='ROC'
      ,trControl=training$final_trControl
      ,tuneGrid=model$bestTune
    ))
  if(!is.null(file)) saveRDS(model,paste0(dir,'/',file,'.rds'))
  
  # Model calibration
  setpb(pb,3)
  calib_model=
    suppressWarnings(caret::train(
      outcome~.
      ,data=
        data %>%
        .[,pData(protocolData(.))$int] %>%
        get_set() %>%
        .[!rownames(.)%in%training$pre_calib_set,,drop=F] %>%
        cbind(predict(model,newdata=.,type='prob')) %>%
        select(event,outcome)
      ,method=calib_method
      ,weights=
        training$outcome_weights %>%
        .[!rownames(.)%in%training$pre_calib_set,,drop=F] %>%
        pull(weight)
      ,metric='ROC'
      ,trControl=training$final_trControl
      ,family=binomial(link='logit')
    ))
  if(!is.null(file)) saveRDS(calib_model,paste0(dir,'/calib_',file,'.rds'))
  
  # Model evaluation on validation sets
  setpb(pb,4)
  eval_model=
    list('ran','geo','tem','bgt') %>%
    lapply(function(x){
      data %>%
        .[,pData(protocolData(.))[[x]]] %>%
        get_set() %>%
        cbind(predict(model,newdata=.,type='prob')) %>%
        select(nonevent,event,outcome) %>%
        mutate(obs=outcome) %>%
        select(-outcome) %>%
        evalm(silent=T,showplots=F)
    }) %>%
    setNames(paste0('nocalib_',c('ran','geo','tem','bgt'))) %>%
    c(list('ran','geo','tem','bgt') %>%
        lapply(function(x){
          data %>%
            .[,pData(protocolData(.))[[x]]] %>%
            get_set() %>%
            cbind(predict(model,newdata=.,type='prob')) %>%
            select(event,outcome) %>%
            cbind(
              predict(calib_model,newdata=.,type='prob') %>%
                rename_all(function(x)paste0(x,'2'))
            ) %>%
            select(-event) %>%
            rename_all(function(x)str_remove_all(x,'2')) %>%
            mutate(obs=outcome) %>%
            select(-outcome) %>%
            evalm(silent=T,showplots=F)
        }) %>%
        setNames(paste0('calib_',c('ran','geo','tem','bgt')))
    )
  if(!is.null(file)) saveRDS(eval_model,paste0(dir,'/eval_',file,'.rds'))
  
  # Close the CPU clusters and clean up memory.
  stopCluster(cl)
  registerDoSEQ()
  rm(cl)
  gc()
  
  # Log off.
  setpb(pb,5)
  cat('\nEnd:',as.character(now()))
  
  list(model=model,calib_model=calib_model,eval_model=eval_model)
  
}