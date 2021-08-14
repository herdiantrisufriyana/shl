#' Split for external validation
#'
#' Split the tidy set specific for this PROM study for external validation.
#'
#' @param tidy_set Tidy set, an ExpressionSet from medhist package after 
#' being processed by \code{prom_tidyset_personalization}.
#' @param geo_p Geographical proportion, a numeric of the city proportion per 
#' province for geographical splitting.
#' @param tem_p Temporal proportion, a numeric of the day proportion per 
#' subtropical season for temporal splitting.
#' @param bgt_p Geotemporal proportion, a numeric of either the city proportion 
#' per state or the day proportion per subtropical season that are allowed to 
#' overlap on each other for geotemporal splitting.
#' @param ran_p Random proportion, a numeric of the instance proportion for 
#' random splitting.
#'
#' @details
#' This function is highly contextual for this PROM study, after processing by 
#' \code{prom_tidyset_personalization}.
#' 
#' @return Tidy set, an ExpressionSet with splitting information for external 
#' validation.
#' 

extv=function(tidy_set,geo_p=0.12,tem_p=0.35,bgt_p=1,ran_p=0.2){
  suppressWarnings(set.seed(33,sample.kind=sample.kind))
  tidy_set %>%
    
    # Insert splitting information into protocol data
    `protocolData<-`(
      lapply(X=1,Y=.,function(X,Y){
        
        # Get protocol data.
        Z=protocolData(Y) %>%
          pData()
        
        K=Z %>%
          
          # Take only geographical and temporal data.
          select(subject_province,subject_city,latest_date) %>%
          rownames_to_column(var='id') %>%
          
          # Determine a season for the latest date.
          mutate(
            season=case_when(
              between(latest_date
                      ,as_date('2015-01-01')
                      ,as_date('2015-03-20'))
              ~'winter_2015'
              ,between(latest_date
                       ,as_date('2015-03-21')
                       ,as_date('2015-06-21'))
              ~'spring_2015'
              ,between(latest_date
                       ,as_date('2015-06-22')
                       ,as_date('2015-09-23'))
              ~'summer_2015'
              ,between(latest_date
                       ,as_date('2015-09-24')
                       ,as_date('2015-12-22'))
              ~'autumn_2015'
              ,between(latest_date
                       ,as_date('2015-12-23')
                       ,as_date('2016-03-20'))
              ~'winter_2016'
              ,between(latest_date
                       ,as_date('2016-03-21')
                       ,as_date('2016-06-20'))
              ~'spring_2016'
              ,between(latest_date
                       ,as_date('2016-06-21')
                       ,as_date('2016-09-22'))
              ~'summer_2016'
              ,between(latest_date
                       ,as_date('2016-09-23')
                       ,as_date('2016-12-31'))
              ~'autumn_2016'
              ,TRUE
              ~'none'
            )
          ) %>%
          
          # For each province, randomly take geo_p proportion of cities 
          # that is assigned true to exclude. Minimum 1 city.
          group_by(subject_province) %>%
          mutate(
            geo=
              subject_city %>%
              .[!duplicated(.)] %>%
              sample(max(1,round(geo_p*length(.))),F) %>%
              paste(collapse='|')
          ) %>%
          ungroup() %>%
          mutate(geo=str_detect(subject_city,geo)) %>%
          mutate(geo=ifelse(is.na(geo),FALSE,geo)) %>%
          
          # For each season, randomly take tem_p proportion of days 
          # that is assigned true to exclude. The day interval is determined 
          # by tem_p. This is randomly shifted within the season. Assign true 
          # if the latest date is within that interval.
          group_by(season) %>%
          mutate(
            tem_d=
              as_date(min(latest_date):max(latest_date)) %>%
              length(.)
            ,tem_d=
              round(tem_p*tem_d)
            ,tem=
              as_date(min(latest_date):max(latest_date)) %>%
              .[seq((1+round(tem_p*length(.)))
                    ,(length(.)-round(tem_p*length(.))))] %>%
              sample(1,F)
          ) %>%
          ungroup() %>%
          mutate(tem=between(latest_date,tem-days(tem_d),tem+days(tem_d))) %>%
          select(-tem_d) %>%
          mutate(tem=ifelse(is.na(tem),FALSE,tem)) %>%
          
          # Join bgt_p proportion of either geographical or temporal splits 
          # above into the previous results.
          left_join(
            filter(.,geo) %>%
              group_by(subject_province) %>%
              mutate(
                bg=
                  subject_city %>%
                  .[!duplicated(.)] %>%
                  sample(max(1,round(bgt_p*length(.))),F) %>%
                  paste(collapse='|')
              ) %>%
              ungroup() %>%
              mutate(bg=str_detect(subject_city,bg)) %>%
              filter(bg) %>%
              select(subject_province,subject_city) %>%
              filter(!duplicated(.)) %>%
              mutate(bg=1)
            ,by=c('subject_province','subject_city')
          ) %>%
          left_join(
            filter(.,tem) %>%
              group_by(season) %>%
              mutate(
                bt_d=
                  as_date(min(latest_date):max(latest_date)) %>%
                  length(.)
                ,bt_d=
                  round(bgt_p*bt_d)
                ,bt=
                  as_date(min(latest_date):max(latest_date)) %>%
                  .[seq((1+round(bgt_p*length(.)))
                        ,(length(.)-round(bgt_p*length(.))))] %>%
                  sample(1,F)
              ) %>%
              ungroup() %>%
              mutate(bt=between(latest_date,bt-days(bt_d),bt+days(bt_d))) %>%
              select(-bt_d) %>%
              filter(bt) %>%
              select(season,latest_date) %>%
              filter(!duplicated(.)) %>%
              mutate(bt=1)
            ,by=c('season','latest_date')
          ) %>%
          mutate(bgt=!is.na(bg) & !is.na(bt)) %>%
          select(-bg,-bt) %>%
          mutate(bgt=ifelse(is.na(bgt),FALSE,bgt)) %>%
          
          # Exclude the geotemporal split 
          # out of either geographical or temporal split
          mutate(
            geo=ifelse(bgt,FALSE,geo)
            ,tem=ifelse(bgt,FALSE,tem)
          ) %>%
          
          # For deselected instances for any of the external validation splits, 
          # randomly take ran_p proportion of instances.
          left_join(
            filter(.,geo==F & tem==F & bgt==F) %>%
              slice(sample(
                seq(nrow(.))
                ,max(1,round(ran_p*nrow(.)))
                ,F
              )) %>%
              mutate(ran=1) %>%
              select(id,ran)
            ,by='id'
          ) %>%
          mutate(ran=!is.na(ran)) %>%
          
          # All deselected instances for any splits 
          # become an internal validation set.
          mutate(int= geo==F & tem==F & bgt==F & ran==F) %>%
          
          # Select only columns of instance ID and splitting information.
          select(id,geo,tem,bgt,ran,int) %>%
          column_to_rownames(var='id')
        
        # Combine protocol data with the splitting data.
        cbind(Z,K) %>%
          
          # Add description for the splitting methods.
          AnnotatedDataFrame(
            varMetadata=
              Y %>%
              protocolData() %>%
              varMetadata() %>%
              rownames_to_column(var='var') %>%
              add_row(
                c('geo','Geographical split.'
                  ,'tem','Temporal split.'
                  ,'bgt','Geotemporal split.'
                  ,'ran','Random split.'
                  ,'int','Internal validation') %>%
                  matrix(
                    ncol=2
                    ,byrow=T
                    ,dimnames=list(NULL,c('var','labelDescription'))
                  ) %>%
                  as.data.frame()
              ) %>%
              column_to_rownames(var='var')
          )
      }) %>%
        .[[1]]
    )
}