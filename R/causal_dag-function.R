#' A single causal diagram plot
#'
#' Make a single causal diagram plot specifically for this PROM study.
#'
#' @param exposure Causal factor, a character vector of a causal factor of 
#' interest.
#' @param legend Legend, a logical whether showing the legend for the node 
#' colors.
#'
#' @details
#' This function is highly contextual for this PROM study. Specific variables
#' of dag$baseline_edges, dag$measure_edges, and dag$formula should be 
#' available.
#' 
#' @return A single causal diagram plot, a ggplot object.
#' 

causal_dag=function(exposure,legend=T){
  dag$baseline_edges[,-3] %>%
    
    # Exclude the second-level factors prefixed by L.
    filter(str_sub(from,1,1)!='L') %>%
    
    # Get a single causal diagram table.
    backdoor_path(dag$measure_edges[,-3],exposure,'Y01') %>%
    
    # Join nodes of unmeasured variables 
    # that may affect the measurement variables.
    left_join(
      dag$baseline_nodes %>%
        rename(from=label) %>%
        rbind(mutate(.,from=paste0(from,'*'))) %>%
        rbind(mutate(.,from=str_replace_all(from,'[:alpha:]','U'))) %>%
        mutate(label=paste(from,name)) %>%
        select(from,label)
      ,by='from'
    ) %>%
    select(-from) %>%
    rename(from=label) %>%
    left_join(
      dag$baseline_nodes %>%
        rename(to=label) %>%
        rbind(mutate(.,to=paste0(to,'*'))) %>%
        rbind(mutate(.,to=str_replace_all(to,'[:alpha:]','U'))) %>%
        mutate(label=paste(to,name)) %>%
        select(to,label)
      ,by='to'
    ) %>%
    select(-to) %>%
    rename(to=label) %>%
    
    # Convert this dataframe to an igraph class.
    graph_from_data_frame() %>%
    
    # Transform into a ggnetwork dataframe with tree layout.
    ggnetwork(
      layout=
        layout_as_tree(
          .
          ,root='Y01 PROM'
          ,mode='all'
          ,circular=F
        )[,2:1]
    ) %>%
    
    # Make a variable to assign a different color to the exposure of interest.
    mutate(
      code=str_sub(name,1,3)
      ,Type=str_sub(code,1,1)
      ,Type=ifelse(code==exposure,'I',Type)
    ) %>%
    
    # Plot the dataframe.
    ggplot(aes(x=x,y=y,xend=xend,yend=yend,color=Type)) +
    geom_nodes(size=8,show.legend=legend) +
    geom_edges(
      arrow=arrow(length=unit(8,'pt'),type='closed')
      ,show.legend=F
      ,curvature=-0.15
    ) +
    geom_nodelabel_repel(
      aes(label=name),family='sans',size=unit(3,'pt')
      ,alpha=0.75
      ,show.legend=F
    ) +
    scale_color_npg() +
    theme_blank() +
    theme(
      plot.caption=element_text(size=unit(9,'pt'),family='sans')
      ,legend.title=element_text(family='sans')
      ,legend.text=element_text(family='sans')
      ,legend.position='bottom'
    ) +
    labs(
      caption=
        dag$formula %>%
        lapply(function(x)paste0('Y01',' ~ ',x[3])) %>%
        .[[exposure]] %>%
        str_remove_all('\\s+')
    )
}