

server <- function(input, output, session) {
    
    # Reactive values ----------------------------------------------------------
    values <- reactiveValues(
        chosenTree = NULL,
        chosenSample = NULL,
        parse_data = NULL,
        all_dff = NULL,
        ret = NULL,
        confSliderArgs = list("min"=0,"max"=0.99,"res"=0.01),
        fnr=NULL,
        quant=NULL,
        datasetName=NULL,
        d_graph1=NULL,
        d_graph2=NULL
    )
    
    phydoseData <- reactiveValues(
        gamma_list = NULL,
        cells = NULL,
        all_dff = NULL,
        per_tree_k = NULL,
        parse_data = NULL
    )
    
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
        updateSelectizeInput(session, "treeNum", selected=as.numeric(d$key)
        )
    })
    
    observeEvent(values$ret,{
        req(values$ret)
        labels <- create_labels("Tree", length(values$ret$parse_data$trees))
        updateSelectizeInput(session, "treeNum",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        updateSelectizeInput(session, "treeNum2",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
    })
    
    observeEvent({
        input$treeNum
        values$ret
        }, {
        if (input$treeNum == ""){
            return (NULL)
        }
            treeNum <- input$treeNum
            if (as.numeric(input$treeNum) > length(values$ret$all_dff)){
                treeNum <- 1
            }
        labels <- create_labels("DF", length(values$ret$all_dff[[as.numeric(treeNum)]]))
        updateSelectInput(session, "dfNum",
                          label = "Select distinguishing feature",
                          choices = labels
        )
    })
    
    observeEvent({
        input$treeNum2
        values$ret
        }, {
        if (input$treeNum2 == ""){
            return (NULL)
        }
            treeNum2 <- input$treeNum2
            if (as.numeric(input$treeNum2) > length(values$ret$all_dff)){
                treeNum2 <- 1
            } 
        labels <- create_labels("DF", length(values$ret$all_dff[[as.numeric(treeNum2)]]))
        updateSelectInput(session, "dfNum2",
                          label = "Select distinguishing feature",
                          choices = labels
        )
    })
    
    observeEvent({
        input$treeNum
        input$dfNum
        values$ret
        },{
        if (input$treeNum == "" || input$dfNum == ""){
            return (NULL)
        }
            treeNum <- input$treeNum
            if (as.numeric(input$treeNum) > length(values$ret$all_dff)){
                treeNum <- 1
            }
            dfNum <- input$dfNum
            if (as.numeric(input$dfNum) > length(values$ret$all_dff[[as.numeric(treeNum)]])){
                dfNum <- 1
            }
        labels <- create_labels(
            "Featurette", 
            length(values$ret$all_dff[[as.numeric(treeNum)]][[as.numeric(dfNum)]])
            )
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = labels
        )
    })
    
    observeEvent(
        {input$dfNum2
            input$treeNum2
            values$ret
            },{
        if (input$treeNum2 == "" || input$dfNum2 == ""){
            return (NULL)
        }
                treeNum2 <- input$treeNum2
                if (as.numeric(input$treeNum2) > length(values$ret$all_dff)){
                    treeNum2 <- 1
                }
                dfNum2 <- input$dfNum2
                if (as.numeric(input$dfNum2) > length(values$ret$all_dff[[as.numeric(treeNum2)]])){
                    dfNum2 <- 1
                }
        labels <- create_labels(
            "Featurette", 
            length(values$ret$all_dff[[as.numeric(treeNum2)]][[as.numeric(dfNum2)]])
        )
        updateSelectInput(session, "featuretteNum2",
                          label = "Select featurette",
                          choices = labels
        )
    })
    
    # ALL patient 1 is chosen
    observeEvent(input$runALL, {
        withProgress(message = 'Showing ALL patient', value = 50, {
        load(file="data/ALLphydose.rda")
        ret <- list("my_glist"=ALLphydose$gamma_list,
                           "my_clist"=ALLphydose$cells,
                           "per_tree_k"=ALLphydose$per_tree_k,
                           "parse_data"=ALLphydose$parse_data,
                           "all_dff"=ALLphydose$all_dff
        )
        labels <- create_labels("Tree", length(ret$parse_data$trees))
        updateSelectizeInput(session, "treeNum",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        updateSelectizeInput(session, "treeNum2",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        values$ret <- ret
        values$fnr <- 0.2
        values$quant <- 1
        values$datasetName <- "ALL patient"
        values$confSliderArgs <- list("min"=0,"max"=0.99,"res"=0.01)
        values$chosenTree <- NULL
        })
    })
    
    # AML patient 1 is chosen
    observeEvent(input$runAML, {
        # withProgress(message = 'Showing AML patient', value = 50, {
        # load(file="data/AML38phydose.rda")
        # ret <- list("my_glist"=AML38phydose$my_glist,
        #                    "my_clist"=AML38phydose$my_clist,
        #                    "per_tree_k"=AML38phydose$per_tree_k,
        #                    "parse_data"=AML38phydose$parse_data,
        #                    "all_dff"=AML38phydose$all_dff
        # )
        # labels <- create_labels("Tree", length(ret$parse_data$trees))
        # updateSelectizeInput(session, "treeNum",
        #                   label = paste0("Select tree [", length(labels)," trees]"),
        #                   choices = labels
        # )
        # updateSelectizeInput(session, "treeNum2",
        #                   label = paste0("Select tree [", length(labels)," trees]"),
        #                   choices = labels
        # )        
        # values$ret <- ret
        # values$fnr <- 0.2
        # values$quant <-1
        # values$datasetName <- "AML patient"
        # values$confSliderArgs <- list("min"=0,"max"=0.99,"res"=0.01)
        # values$chosenTree <- NULL
        # })
        ret <- NULL
        withProgress(message = 'Running PhyDOSE', value = 0, {
            # 1. file is parsed to get trees, frequency matrix, and u matrix
            incProgress(0.1, detail = "Parsing file...")
            parse_data <- AML38
            
            #parse_data <- ALLPatient2
            # 2. using output from file parse, call the getdff to get dff
            incProgress(0.1, detail = "Finding distinguishing features...")
            all_dff <- AML38DFF
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
                incProgress(0.002, detail = paste("Calculating values for k =", g))
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
        
        labels <- create_labels("Tree", length(ret$parse_data$trees))
        updateSelectizeInput(session, "treeNum",
                             label = paste0("Select tree [", length(labels)," trees]"),
                             choices = labels
        )
        updateSelectizeInput(session, "treeNum2",
                             label = paste0("Select tree [", length(labels)," trees]"),
                             choices = labels
        )          
        
        values$ret <- ret
        values$fnr <- input$fn
        values$quant <- input$quantile
        values$datasetName <- input$file[['name']]
        values$chosenTree <- NULL
        values$confSliderArgs <- list("min"=input$gmin,
                                      "max"=input$gmax,
                                      "res"=min(input$gresolution,input$gmax-input$gmin))
    })
    
    # Run button is pressed
    observeEvent(input$run, {
        ret <- NULL
        
        if (is.null(input$file)){
            showModal(modalDialog(
                "Please upload file or run an example dataset",
                easyClose = TRUE
            ))
            return(NULL)
        }
        
        
        
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
        print("changing the tree nums 1")
        labels <- create_labels("Tree", length(ret$parse_data$trees))
        updateSelectizeInput(session, "treeNum",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        updateSelectizeInput(session, "treeNum2",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )          
        
        values$ret <- ret
        values$fnr <- input$fn
        values$quant <- input$quantile
        values$datasetName <- input$file[['name']]
        values$chosenTree <- NULL
        values$confSliderArgs <- list("min"=input$gmin,
                                      "max"=input$gmax,
                                      "res"=min(input$gresolution,input$gmax-input$gmin))
    })
    
    # render tree 1
    output$graphV <- renderGrViz({ 
        req(values$ret)
        values$d_graph1 <- get_diagrammer_tree(values$ret$parse_data, 
                                       values$ret$all_dff, 
                                       input$treeNum, 
                                       input$dfNum, 
                                       input$featuretteNum,
                                       input$showU1,
                                       input$showF1
        )
        render_graph(values$d_graph1, layout="tree")
    })
    
    # render tree 2
    output$graphV2 <- renderGrViz({ 
        req(values$ret)
        values$d_graph2 <- get_diagrammer_tree(values$ret$parse_data, 
                                       values$ret$all_dff, 
                                       input$treeNum2, 
                                       input$dfNum2, 
                                       input$featuretteNum2,
                                       input$showU2,
                                       input$showF2
                                       )
        render_graph(values$d_graph2, layout="tree")
    })
    
    output$kvscl <- renderPlotly({
        req(values$ret)
        ret <- values$ret
        p <- create_step_graph(
            gamma_list=ret$my_glist, 
            cells_list=ret$my_clist, 
            per_tree_k=ret$per_tree_k, 
            chosen_tree=values$chosenTree, 
            chosen_gamma=input$gamma)
        ggplotly(p, tooltip=c("text","x","y"))
    })
    
    output$box_jitter <- renderPlotly({
        req(values$ret)
        ret <- values$ret
        p <- create_box_jitter(
            per_tree_k=ret$per_tree_k,
            chosen_tree=values$chosenTree,
            chosen_gamma=input$gamma
            )
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
        return(paste0(gamma_min$cells[1], " cells required to sequence with confidence level 𝛾 = ", input$gamma))
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
                          min = values$confSliderArgs$min, 
                          max = values$confSliderArgs$max, 
                          step = values$confSliderArgs$res,
                          value = values$confSliderArgs$max
                          )
    })
    
    observe({
        updateSliderInput(session, "gmin", max = input$gmax-0.01)
    })
    observe({
        updateSliderInput(session, "gmax", min = input$gmin+0.01)
    })
    
    output$detTable <- renderTable({
        if (is.null(values$fnr)){
            return(NULL)
        }
        sampleShown <- "-"
        
        if (!is.null(values$chosenSample)){
            sampleShown <- as.integer(values$chosenSample)
        }
        treeShown <- "-"
        if (!is.null(values$chosenTree)){
            treeShown <- as.integer(values$chosenTree)
        }
        tabl <- data.frame(
            "tree"=treeShown,
            "biopsy"=sampleShown,
            "fnr"=values$fnr,
            "quant"=values$quant,
            "dataset"=values$datasetName
            )
    }, width='auto')
    
    
    output$fTable <- renderTable({
        if (is.null(values$ret)){
            return(NULL)
        }
        return(values$ret$parse_data$fmatrices[[1]])
    })
    
    output$box_title <- renderText({
        req(values$ret)
        q_t <- get_quantile_and_tree(values$ret$per_tree_k, values$chosenTree, input$gamma)
        trunc_q <- sprintf("%.3f", q_t$quantile)
        return(paste0("Tree #", q_t$tree, " with quantile = ", trunc_q))
    })
    
    observeEvent(input$downloadTree1,{
        dot_version <- values$d_graph1 %>% add_global_graph_attrs(attr="layout", value="dot", attr_type = "graph")
        export_graph(dot_version,file_name = "tree.pdf", file_type = "PDF")
    })
    
    observeEvent(input$downloadTree2,{
        dot_version <- values$d_graph2 %>% add_global_graph_attrs(attr="layout", value="dot", attr_type = "graph")
        export_graph(dot_version,file_name = "tree.pdf", file_type = "PDF")
    })
}