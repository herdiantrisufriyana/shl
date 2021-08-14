#' Unified causal diagram (caugram) plot
#'
#' Plot a causal diagram using an edge (from-to) table with a node table 
#' that is already defined in dag$baseline_nodes.
#'
#' @param edge_table Edge table, a dataframe with 'from' and 'to' columns.
#' @param curved Edge curvature, a numeric of 0 to 1 to make the curvature 
#' respectively being curved to straight.
#' @param root Root name, a character vector of the names of the root (if 
#' needed).
#' @param mode Edge direction, a character of direction for the edge included 
#' in the diagram. This should be 'in', 'out', or 'all' respectively for 
#' incoming, outcoming, or all edges.
#' @param circular Circular network, a logical whether the diagram is made 
#' circular.
#' @param plot.title Plot title, a character of the title.
#' @param label_size Label size, an integer of the size
#'
#' @details
#' This function is highly contextual for this PROM study. A specific 
#' variable of dag$baseline_nodes should be available.
#' 
#' @return Unified causal diagram, an ggplot object.
#' 

plot_dag=function(edge_table
                  ,curved=1
                  ,root=c()
                  ,mode='in'
                  ,circular=F
                  ,plot.title=NULL
                  ,label_size=4){
  
  # Set seed for reproducible positioning of nodes (randomly positioned).
  suppressWarnings(set.seed(66,sample.kind=sample.kind))
  
  # Create the plot.
  edge_table %>%
    
    # Join the node table as 'from'.
    left_join(
      dag$baseline_nodes %>%
        rename(from=label) %>%
        mutate(label=paste(from,name)) %>%
        select(from,label)
      ,by='from'
    ) %>%
    select(-from) %>%
    rename(from=label) %>%
    
    # Join the node table as 'to'.
    left_join(
      dag$baseline_nodes %>%
        rename(to=label) %>%
        mutate(label=paste(to,name)) %>%
        select(to,label)
      ,by='to'
    ) %>%
    select(-to) %>%
    rename(to=label) %>%
    
    # Convert this dataframe to an igraph class.
    lapply(X=1,Y=.,function(X,Y){
      Z=graph_from_data_frame(directed=T,d=Y)
      Z
    }) %>%
    .[[1]] %>%
    
    # Transform into a ggnetwork dataframe with tree layout.
    ggnetwork(
      layout=
        layout_as_tree(
          .
          ,root=root
          ,mode=mode
          ,circular=circular
        )[,2:1]
    ) %>%
    mutate(code=str_sub(name,1,3))  %>%
    
    # Plot the dataframe.
    ggplot(aes(x=x,y=y,xend=xend,yend=yend,color=code)) +
    geom_nodes(
      show.legend=F
      ,size=8
    ) +
    geom_edges(
      arrow=arrow(length=unit(8,'pt'),type='closed')
      ,show.legend=F
      ,curvature=curved
    ) +
    geom_nodelabel_repel(
      aes(label=name),family='sans',size=unit(label_size,'pt')
      ,alpha=0.5
      ,color='black'
      ,show.legend=F
    ) +
    scale_color_manual(
      values=
        unlist(mapply(
          RColorBrewer::brewer.pal
          ,RColorBrewer::brewer.pal.info %>%
            .[.$category=='qual',] %>%
            .$maxcolors
          ,RColorBrewer::brewer.pal.info %>%
            .[.$category=='qual',] %>%
            rownames()
        ))
    ) +
    theme_blank() +
    theme(
      plot.background=element_rect(fill='gray80')
      ,panel.background=element_rect(fill='gray80')
      ,plot.title=element_text(hjust=0.5)
    ) +
    ggtitle(plot.title)
}