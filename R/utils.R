
# Generate list of labels with a prefix (used for selectInput)
create_labels <- function(prefix, num_elems){
    labels <- list()
    for(i in 1:num_elems){
        lab <- paste(prefix, i)
        labels[[lab]] <- i
    }
    return(labels)
}

create_step_graph <- function(gamma_list, cells_list, per_tree_k, chosen_tree, chosen_gamma){
    mydata <- data.frame(gamma = gamma_list, cells = cells_list)
    
    if (!is.null(chosen_tree)){
        mydata <- filter(per_tree_k, tree_id == chosen_tree)
    }
    gamma_min <-filter(mydata, gamma >= chosen_gamma) %>% top_n(1, -gamma)
    label_text <- c()
    for (i in 1:length(mydata$cells)){
        label_text <- c(paste(mydata$cells[[i]], mydata$gamma[[i]]), label_text)
    }
    p <-ggplot(mydata, aes(x=cells, y=gamma)) + 
        geom_step(size=1, color="darkblue") + 
        geom_hline(aes(yintercept = chosen_gamma), linetype = "dotdash") +
        geom_vline(aes(xintercept=gamma_min$cells[1]), linetype = "dotdash") +
        scale_y_continuous(breaks = seq(0,1, by=0.1)) +
        ylab("confidence level") +
        xlab("number of cells to sequence")+
        geom_text(aes(x=gamma_min$cells[1], label=paste("k =",gamma_min$cells[1]), y=0.2), colour="black", angle=90) +
        geom_text(aes(x=gamma_min$cells[1]-5, 
                      label=paste("𝛾 =",chosen_gamma,"\n"),
                      y=chosen_gamma), 
                  colour="black", angle=90)
    return(p)
}

create_box_jitter <- function(per_tree_k, chosen_tree, chosen_gamma){
    max_cells <- max(per_tree_k$cells)
    filtered_k <- filter(per_tree_k, gamma >= chosen_gamma-0.001 & gamma <= chosen_gamma+0.001)
    p <- ggplot(filtered_k, aes(x=sample, y=cells,key=tree_id))+
        geom_boxplot() +
        geom_jitter(position=position_jitter(width=0.1,height=0), fill='gray',alpha=0.5, shape=21) +
        scale_x_discrete(limits=c("1"), label=c("1"))+
        scale_y_continuous(limits=c(0,max_cells+1))+
        ylab("number of cells to sequence")+
        xlab("biopsy")
    if (!is.null(chosen_tree)){
        highlighted_k <- head(filter(filtered_k, tree_id == as.numeric(chosen_tree)),1)
        p <- p + geom_point(highlighted_k, mapping=aes(), color="red", size=3)
    }
    else{
        max_cells_filtered <- max(filtered_k$cells) # get the max number of cells
        max_tree <- head(filter(filtered_k, cells == max_cells_filtered),1)
        p <- p + geom_point(max_tree, mapping=aes(), color="red", size=3 )
    }
    return(p)
}

# returns the quantile of the chosen tree for the chosen gamma
get_quantile_and_tree <- function(per_tree_k, chosen_tree, chosen_gamma){
    
    filtered_k <- filter(per_tree_k, gamma >= chosen_gamma-0.001 & gamma <= chosen_gamma+0.001)
    if (is.null(chosen_tree)){
        max_cells_filtered <- max(filtered_k$cells) # get the max number of cells
        max_tree <- head(filter(filtered_k, cells == max_cells_filtered),1)
        chosen_tree <- head(filter(filtered_k, cells == max_cells_filtered),1)$tree_id
    }
    print(paste("chosen tree", chosen_tree))
    filtered_k <- filter(per_tree_k, gamma==chosen_gamma)
    num_cells <- head(filter(filtered_k, tree_id == chosen_tree),1)$cells
    print(paste("num cells", num_cells))
    # method to get quantile from https://stat.ethz.ch/pipermail/r-help/2012-March/305368.html
    return(list(tree=chosen_tree,quantile=mean(filtered_k$cells <= num_cells)))
}

get_diagrammer_tree <- function(parse_data, all_dff, tree_num, df_num, featurette_num, showu, showf){
    if (tree_num == "" || df_num == "" || featurette_num == ""){
        return (NULL)
    }
    tree <- parse_data$trees[[as.numeric(tree_num)]]
    featurette <- all_dff[[as.numeric(tree_num)]][[as.numeric(df_num)]][[as.numeric(featurette_num)]]
    u <- NULL
    f <- NULL
    if (showu){
        f <- parse_data$fmatrices[[as.numeric(tree_num)]]
    }
    if (showf){
        u <- parse_data$u_list[[as.numeric(tree_num)]]
    }
    d_graph <- plotTree(tree, featurette=featurette)
    return(d_graph)
}