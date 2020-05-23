library(shiny)
library(shinyFiles)
library(stringr)
library(sjmisc)
library(dplyr,warn.conflicts=FALSE)
library(tidyr)
library(shinyjs)
library(DiagrammeR)
library(shinyAce)
library(ggplot2)
library(rlist)
library(phydose)
library(plotly)
library(shinydashboard)

server <- function(input, output, session) {
    
    # Reactive values ----------------------------------------------------------
    values <- reactiveValues(
        chosenTree = NULL,
        chosenSample = NULL,
        parse_data = NULL,
        all_dff = NULL,
        ret = NULL,
        confSliderArgs = list("min"=0,"max"=0.99,"res"=0.01)
    )
    
    phydoseData <- reactiveValues(
        gamma_list = NULL,
        cells = NULL,
        all_dff = NULL,
        per_tree_k = NULL,
        parse_data = NULL
    )
    
    # Observe if file has been uploaded ----------------------------------------
    observeEvent(input$file, {
        enable("run")
        withProgress(message = 'Parsing file', value = 0, {
            # 1. file is parsed to get trees, frequency matrix, and u matrix
            incProgress(0.3, detail = "Checking file format")
            values$parse_data <- ReadJoint(input$file[['datapath']])
            
            #parse_data <- ALLPatient2
            # 2. using output from file parse, call the getdff to get dff
            incProgress(0.3, detail = "Finding distinguishing features...")
            values$all_dff <- generateDistFeat(values$parse_data$trees)
        })
    })
    
    # Observe if specific tree is chosen to be displayed -----------------------
    observeEvent(input$plotTree1,{
        values$chosenTree <- as.numeric(input$treeNum)
    })
    
    observeEvent(input$plotTree2,{
        values$chosenTree <- as.numeric(input$treeNum2)
    })
    
    observeEvent(input$clearChosenTree,{
        values$chosenTree <- NULL
    })
    
    observeEvent(event_data(event="plotly_click", source="mysource"),{
        req(values$ret)
        d <- event_data(event="plotly_click", source="mysource")
        values$chosenTree <- as.numeric(d$key)
        updateSelectInput(session, "treeNum", selected=as.numeric(d$key)
        )
    })
    
    observe({
        req(values$ret)
        labels <- list()
        optionsid <- 1:length(values$ret$parse_data$trees)
        for(i in 1:length(optionsid)){
            lab <- paste("Tree", i)
            labels[[lab]] <- i
        }
        updateSelectInput(session, "treeNum",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        updateSelectInput(session, "treeNum2",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
    })
    
    observeEvent(input$treeNum, {
        if (input$treeNum == ""){
            return (NULL)
        }
        all_dff <- values$ret$all_dff
        mydff <- all_dff[[as.numeric(input$treeNum)]]
        dff_labels <- list()
        for(i in 1:length(mydff)){
            lab <- paste("DF", i)
            dff_labels[[lab]] <- i
        }
        disp <- ""
        
        updateSelectInput(session, "dfNum",
                          label = "Select distinguishing feature",
                          choices = dff_labels
        )
        
        featurettes <- mydff[[as.numeric(input$dfNum)]]
        feat_labels <- list()
        for(i in 1:length(featurettes)){
            lab <- paste("Featurette", i)
            feat_labels[[lab]] <- i
        }
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = feat_labels
        )
    })
    
    observeEvent(input$treeNum2, {
        if (input$treeNum2 == ""){
            return (NULL)
        }
        all_dff <- values$ret$all_dff
        mydff <- all_dff[[as.numeric(input$treeNum2)]]
        dff_labels <- list()
        for(i in 1:length(mydff)){
            lab <- paste("DF", i)
            dff_labels[[lab]] <- i
        }
        disp <- ""
        
        updateSelectInput(session, "dfNum2",
                          label = "Select distinguishing feature",
                          choices = dff_labels
        )
        
        featurettes <- mydff[[as.numeric(input$dfNum2)]]
        feat_labels <- list()
        for(i in 1:length(featurettes)){
            lab <- paste("Featurette", i)
            feat_labels[[lab]] <- i
        }
        updateSelectInput(session, "featuretteNum2",
                          label = "Select featurette",
                          choices = feat_labels
        )
    })
    
    observeEvent(input$dfNum,{
        if (input$dfNum == ""){
            return (NULL)
        }
        mydff <- all_dff[[as.numeric(input$treeNum)]]
        featurettes <- mydff[[as.numeric(input$dfNum)]]
        feat_labels <- list()
        for(i in 1:length(featurettes)){
            lab <- paste("Featurette", i)
            feat_labels[[lab]] <- i
        }
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = feat_labels
        )
    })
    
    observeEvent(input$dfNum2,{
        if (input$dfNum2 == ""){
            return (NULL)
        }
        mydff <- all_dff[[as.numeric(input$treeNum2)]]
        featurettes <- mydff[[as.numeric(input$dfNum2)]]
        feat_labels <- list()
        for(i in 1:length(featurettes)){
            lab <- paste("Featurette", i)
            feat_labels[[lab]] <- i
        }
        updateSelectInput(session, "featuretteNum2",
                          label = "Select featurette",
                          choices = feat_labels
        )
    })
    
    # ALL patient 1 is chosen
    observeEvent(input$runALL, {
        load(file="data/ALLphydose.rda")
        values$ret <- list("my_glist"=ALLphydose$gamma_list,
                    "my_clist"=ALLphydose$cells,
                    "per_tree_k"=ALLphydose$per_tree_k,
                    "parse_data"=ALLphydose$parse_data,
                    "all_dff"=ALLphydose$all_dff
        )
        
    })
    
    # AML patient 1 is chosen
    observeEvent(input$runAML, {
        load(file="data/AML38phydose.rda")
        values$ret <- list("my_glist"=AML38phydose$my_glist,
                           "my_clist"=AML38phydose$my_clist,
                           "per_tree_k"=AML38phydose$per_tree_k,
                           "parse_data"=AML38phydose$parse_data,
                           "all_dff"=AML38phydose$all_dff
        )
    })
    
    # Run button is pressed
    observeEvent(input$run, {
        ret <- NULL
        withProgress(message = 'Running PhyDOSE', value = 0, {
            # 1. file is parsed to get trees, frequency matrix, and u matrix
            incProgress(0.1, detail = "Parsing file...")
            parse_data <- ReadJoint(input$file[['datapath']])
            
            #parse_data <- ALLPatient2
            # 2. using output from file parse, call the getdff to get dff
            incProgress(0.1, detail = "Finding distinguishing features...")
            all_dff <- generateDistFeat(parse_data$trees)
            #all_dff <- ALLPatient2DFF
            # 3. after getting the dff, call phydose multiple times to get data for plot
            incProgress(0.2, detail = "Calculating values of k...")
            gamma_list <- seq(input$gmin, input$gmax, by=input$gresolution)
            if ( tail(gamma_list, 1) != input$gmax ) {
                gamma_list <- c(gamma_list, input$gmax)
            } 
            cells <- numeric(length(gamma_list))
            index <- 1
            u_list <- input$u
            per_tree_k <- data.frame(tree = character(), 
                                     tree_id = numeric(), 
                                     sample = numeric(), 
                                     cells = numeric(),
                                     gamma = numeric())
            for(g in gamma_list){
                incProgress(0.002, detail = paste("Calculating values of k for", g))
                phydose_ret <- phydose(
                    parse_data$tree, 
                    parse_data$fmatrices, 
                    distFeat=all_dff,
                    gamma = g,
                    fnr = input$fn,
                    kstar_quant = input$percentile
                )
                mydata <- phydose_ret$kTdata
                mydata[["gamma"]] <- g
                per_tree_k <- rbind(per_tree_k, mydata)
                
                cells[index] = phydose_ret$kstar
                index = index + 1
            }
            
            # 4. update UI
            incProgress(0.1, detail = "Updating UI...")
            # update the selection of trees
            
            incProgress(0.2, detail = "Done!")
            ret <- list("my_glist"=gamma_list,
                        "my_clist"=cells,
                        "per_tree_k"=per_tree_k,
                        "parse_data"=parse_data,
                        "all_dff"=all_dff
            )
            incProgress(0.2, detail = "Done!!")
        })
                    
        
        values$ret <- ret
    })
    
    # render tree 1
    output$graphV <- renderGrViz({ 
        req(values$ret)
        ret <- values$ret
        if (input$treeNum == ""){
            return (NULL)
        }
        tree <- ret$parse_data$trees[[as.numeric(input$treeNum)]]
        featurette <- ret$all_dff[[as.numeric(input$treeNum)]][[as.numeric(input$dfNum)]][as.numeric(input$featuretteNum)]
        
        #print(ret$all_dff[[as.numeric(input$treeNum)]][[as.numeric(input$dfNum)]][[as.numeric(input$featuretteNum)]])
        d_graph <- plotTree(tree, featurette=featurette)
        render_graph(d_graph, layout="tree")
    })
    
    # render tree 2
    output$graphV2 <- renderGrViz({ 
        req(values$ret)
        ret <- values$ret
        if (input$treeNum2 == ""){
            return (NULL)
        }
        tree <- ret$parse_data$trees[[as.numeric(input$treeNum2)]]
        featurette <- ret$all_dff[[as.numeric(input$treeNum2)]][[as.numeric(input$dfNum2)]][[as.numeric(input$featuretteNum2)]]
        d_graph <- plotTree(tree, featurette=featurette)
        render_graph(d_graph, layout="tree")
    })
    
    output$kvscl <- renderPlotly({
        # generate data for plot
        # create the plot
        req(values$ret)
        ret <- values$ret
        gamma_list <- ret$my_glist
        cells_list <- ret$my_clist
        mydata <- data.frame(gamma = gamma_list, cells = cells_list)

        if (!is.null(values$chosenTree)){
            mydata <- filter(ret$per_tree_k, tree_id == as.numeric(values$chosenTree))
        }
        gamma_min <-filter(mydata, gamma >= input$gamma) %>% top_n(1, -gamma)
        p <-ggplot(mydata, aes(x=cells, y=gamma)) + geom_step(size=1.5, color="darkblue") + ylab("confidence level") +
                geom_hline(aes(yintercept = input$gamma), linetype = "dotdash") +
                geom_vline(aes(xintercept=gamma_min$cells[1]), linetype = "dotdash") +
                scale_y_continuous(breaks = seq(0,1, by=0.1)) +
                xlab("number of cells to sequence")
        ggplotly(p)
    })
    
    output$box_jitter <- renderPlotly({
        req(values$ret)
        ret <- values$ret
        
        max_cells <- max(ret$per_tree_k$cells)
        #max_cells <- ceiling(max_cells/10)*10
        filtered_k <- filter(ret$per_tree_k, gamma >= input$gamma-0.001 & gamma <= input$gamma+0.001)
        p <- ggplot(filtered_k, aes(x=sample, y=cells,key=tree_id))+
            geom_boxplot() +
            geom_jitter(position=position_jitter(width=0,height=0)) +
            scale_x_discrete(limits=c("1"), label=c("1"))+
            scale_y_continuous(limits=c(0,max_cells+1))+
            ylab("number of cells to sequence")+
            xlab("biopsy")
        if (!is.null(values$chosenTree)){
            highlighted_k <- filter(filtered_k, tree_id == as.numeric(values$chosenTree))
 
            p <- p + geom_point(highlighted_k, mapping=aes(), color="red", size=3)
        }
        ggplotly(p, source="mysource")
    })
    
    output$num_cells <- renderText({
        req(values$ret)
        ret <- values$ret
        gamma_list <- ret$my_glist
        cells_list <- ret$my_clist
        mydata <- data.frame(gamma = gamma_list, cells = cells_list)
        
        if (!is.null(values$chosenTree)){
            mydata <- filter(ret$per_tree_k, tree_id == as.numeric(values$chosenTree))
        }
        
        gamma_min <-filter(mydata, gamma >= input$gamma) %>% top_n(1, -gamma)
        return(paste0(gamma_min$cells[1], " cells required to sequence with confidence level = ", input$gamma))
    })
    
    output$treeNumDetail <- renderText({
        treeShown <- "-"
        if (!is.null(values$chosenTree)){
            treeShown <- values$chosenTree
        }
        return(paste0(
            "Tree shown: ", treeShown
        ))
    })
    
    output$sampleNumDetail <- renderText({
        sampleShown <- "-"
        if (!is.null(values$chosenSample)){
            sampleShown <- values$chosenSample
        }
        return(paste0(
            "Biopsy shown: ", sampleShown
        ))
    })
    
    observe({
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "gamma",
                          min = input$gmin, max = input$gmax, step = min(input$gresolution,input$gmax-input$gmin))
    })
    
    observe({
        updateSliderInput(session, "gmin", max = input$gmax-0.01)
    })
    observe({
        updateSliderInput(session, "gmax", min = input$gmin+0.01)
    })
}