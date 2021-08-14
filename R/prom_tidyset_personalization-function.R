#' Personalize PROM tidy set
#'
#' Make the tidy set specific for this PROM study.
#'
#' @param tidy_set Tidy set, an ExpressionSet from medhist package.
#' @param outcome Outcome data, a dataframe with columns related to pregnancy 
#' episodes, categorical variables, and healthcare facility information.
#' @param experimenter Experimenter's information, a MIAME class to store 
#' the experimenter's information.
#' @param annotation annotation, a dataframe of annotation data specificly for 
#' this PROM study.
#'
#' @details
#' This function is highly contextual for this PROM study.
#' 
#' @return Tidy set, an ExpressionSet with additional data and information.
#' 

prom_tidyset_personalization=function(tidy_set,outcome,experimenter,annotation){
  
  tidy_set %>%
    
    # Personalize phenotype data.
    `phenoData<-`(
      lapply(X=1,Y=.,Z=outcome,function(X,Y,Z){
        pData(phenoData(Y)) %>%
          
          # Combine the latest and admission dates to phenotype data.
          cbind(
            pData(protocolData(Y)) %>%
              select(subject_id,latest_date,admission_date)
          ) %>%
          rownames_to_column(var='id') %>%
          
          # Join outcome data (with pregnancy episodes, 
          # and categorical variables, except age) to phenotype data.
          left_join(
            Z %>%
              select(
                outcome,subject_id,latest_date
                ,censoring,preg_e,termin_e,termin_l,preg_l
                ,gestation,termination
                ,birth_date,marital_status,insurance_class,occupation_segment
              )
            ,by=c('outcome','subject_id','latest_date')
          ) %>%
          
          # Compute age in years from birth date to the admission date.
          mutate(age=as.duration(admission_date-birth_date)/dyears(1)) %>%
          select(-subject_id,-latest_date,-admission_date,-birth_date) %>%
          select(
            outcome,censoring,preg_e,termin_e,termin_l,preg_l
            ,gestation,termination
            ,age,everything()
          ) %>%
          column_to_rownames(var='id') %>%
          
          # Integrate metadata to describe each variable in phenotype data.
          AnnotatedDataFrame(
            varMetadata=
              c('outcome','Event is O42.'
                ,'censoring','Whether the outcome is censored.'
                ,'preg_e','The earliest date of pregnancy-code encounters.'
                ,'termin_e'
                ,'The earliest date of termination-code encounters.'
                ,'termin_l','The latest date of termination-code encounters.'
                ,'preg_l','The latest date of pregnancy-code encounters.'
                ,'gestation','Number of total gestation'
                ,'termination'
                ,'Number of total termination (delivered/aborted).'
                ,'age','Age in years at admission.'
                ,'marital_status'
                ,'Single, married, divorced/widowed, and unspecified.'
                ,'insurance_class','First (high economy), second, third, NA.'
                ,'occupation_segment'
                ,paste(
                  'Occupation of the householder:'
                  ,'central-government-paid, local-government-paid,'
                  ,'employee, employer, and unemployed'
                )
              ) %>%
              matrix(
                ncol=2
                ,byrow=T
                ,dimnames=list(NULL,c('var','labelDescription'))
              ) %>%
              as.data.frame() %>%
              column_to_rownames(var='var')
          )
      }) %>%
        .[[1]]
    ) %>%
    
    # Personalize feature data.
    `featureData<-`(
      fData(.) %>%
        rownames_to_column(var='code') %>%
        
        # Join annotation data to feature data.
        left_join(
          rbind(annotation)
          ,by='code'
        ) %>%
        column_to_rownames(var='code') %>%
        
        # Integrate metadata to describe each variable in feature data.
        AnnotatedDataFrame(
          varMetadata=
            c('desc'
              ,paste('Short description of the ICD-10 code,'
                     ,'or regular expression of causal factor.')) %>%
            matrix(
              ncol=2
              ,byrow=T
              ,dimnames=list(NULL,c('var','labelDescription'))
            ) %>%
            as.data.frame() %>%
            column_to_rownames(var='var')
        )
    ) %>%
    
    # Personalize the experimenter's information using MIAME format.
    `experimentData<-`(experimenter) %>%
    
    # Beware this study is specific to ICD-10 (2016 version).
    `annotation<-`(value='ICD-10 (2016)') %>%
    
    # Personalize protocol data.
    `protocolData<-`(
      lapply(X=1,Y=.,Z=outcome,function(X,Y,Z){
        pData(protocolData(Y)) %>%
          rownames_to_column(var='id') %>%
          
          # Join outcome data (with healthcare facility information)
          # to protocol data.
          left_join(
            Z %>%
              select(
                subject_id,latest_date
                ,householder_id,reghc_id,subject_country,subject_city
              ) %>%
              rename(subject_province=subject_country)
            ,by=c('subject_id','latest_date')
          ) %>%
          column_to_rownames(var='id') %>%
          
          # Integrate metadata to describe each variable in protocol data.
          AnnotatedDataFrame(
            varMetadata=
              c('visit_id','Unique identification number for a visit.'
                ,'subject_id','Unique identification number for a subject.'
                ,'latest_date','The latest date up to the outcome.'
                ,'healthcare_id'
                ,paste(
                  'Unique identification number for'
                  ,'a healthcare provider which a subject visits'
                )
                ,'admission date','Admission date of a visit episode.'
                ,'db_start_date','The earliest date of any medical histories.'
                ,'householder_id'
                ,'Unique identification number for a householder.'
                ,'reghc_id'
                ,paste(
                  'Unique identification number for a healthcare'
                  ,'provider which a subject is registered at'
                )
                ,'subject_province'
                ,paste(
                  'Province code of a healthcare'
                  ,'provider which a subject is registered at'
                )
                ,'subject_city'
                ,paste(
                  'City code of a healthcare'
                  ,'provider which a subject is registered at'
                )
              ) %>%
              matrix(
                ncol=2
                ,byrow=T
                ,dimnames=list(NULL,c('var','labelDescription'))
              ) %>%
              as.data.frame() %>%
              column_to_rownames(var='var')
          )
      }) %>%
        .[[1]]
    )
  
}