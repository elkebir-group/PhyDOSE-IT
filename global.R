# INSTALL DEPENDENCIES ---------------------------------------------------------

source('dependencies.R')

# LOAD UTILS.R -----------------------------------------------------------------

source("R/utils.R", local=TRUE)

# UI MODULES -------------------------------------------------------------------

runPhydoseInput <- function(id){
    ns <- NS(id)
    list(
        actionButton("runPhydose","Run PhyDOSE", 
                     style="margin: 6px 5px 6px 0px; width:90%")
    )
}

treeVisTogglesInput <- function(id){
    ns <- NS(id)
    list(
           selectizeInput(ns("treeNum"), "Select Tree:",
                          c(), options= list(maxOptions = 10000)),
           selectInput(ns("dfNum"), "Select Distinguishing Feature:",
                       c()),
           selectInput(ns("featuretteNum"), "Select Featurette:",
                       c()),
           checkboxInput(ns("showF"), "Show F matrix"),
           checkboxInput(ns("showU"), "Show U matrix"),
           downloadLink(ns("downloadTree"), "Download tree"),
           tags$div(style='height:10px')
    )
}

treeVisOutput <- function(id){
    ns <- NS(id)
    column(align="center",
        grVizOutput(ns('graphV')),
        width=7
    )
}


# SERVER MODULES ---------------------------------------------------------------

runPhydose <- function(input, output, session,
                       parse_data, all_dff,
                       gmin, gmax, gresolution,
                       fn, percentile, gamma, mytrig
                       ){
    values <- reactiveValues(
        chosenTree = NULL, # this is changed by tree vis, reset button, and initial run, and is read by the graphs
        tree1Num = NULL, # this is changed by the graphs, reset button, and initial run, and is read by tree 1
        chosenSample = NULL,
        ret = NULL,
        confSliderArgs = list("min"=0,"max"=0.99,"res"=0.01)
    )
    
    observeEvent(mytrig() , {
        if(is.null(parse_data())){
            return()
        }
        gmin <- gmin()
        gmax <- gmax()
        gresolution <-gresolution()
        fn <- fn()
        percentile <- percentile()
        gamma <- gamma()
        ret <- NULL
        withProgress(message = 'Running PhyDOSE', value = 0, {
            # 1. after getting the dff, call phydose multiple times to get data for plot
            parse_data <- parse_data()
            all_dff <- all_dff()
            
            incProgress(0.2, detail = "Calculating values of k...")
            gamma_list <- seq(gmin, gmax, by=gresolution)
            if ( tail(gamma_list, 1) != gmax ) {
                gamma_list <- c(gamma_list, gmax)
            } 
            cells <- numeric(length(gamma_list))
            index <- 1
            per_tree_k <- data.frame(tree = character(), 
                                     tree_id = numeric(), 
                                     sample = numeric(), 
                                     cells = numeric(),
                                     gamma = numeric())
            for(g in gamma_list){
                incProgress(0.002, detail = paste("Calculating values for 𝛾 =", g))
                phydose_ret <- phydose(
                    parse_data$tree, 
                    parse_data$fmatrices, 
                    distFeat=all_dff,
                    gamma = g,
                    fnr = fn,
                    kstar_quant = percentile
                )
                mydata <- phydose_ret$kTdata
                mydata[["gamma"]] <- g
                per_tree_k <- rbind(per_tree_k, mydata)
                
                cells[index] = phydose_ret$kstar
                index = index + 1
            }
            
            # 2. update UI
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
        values$confSliderArgs <- list("min"=gmin,
                                      "max"=gmax,
                                      "res"=min(gresolution,gmax-gmin))
        values$tree1Num <- get_max_tree(ret$per_tree_k, gmax)
        values$chosenTree <- values$tree1Num
        values$chosenSample <- get_best_sample(ret$per_tree_k, gmax)
        shinyjs::show('mainpanel')
    })
    return(
        list(
            ret = reactive({ values$ret }),
            confSliderArgs = reactive({ values$confSliderArgs }),
            tree1Num = reactive({ values$tree1Num }),
            chosenTree = reactive({ values$chosenTree }),
            chosenSample = reactive({ values$chosenSample })
        )
    )
}

treeVisToggles <- function(input, output, session, 
                           ret, gamma=0.95, d_graph, dtn,
                           chosenTree
                           ){
    values <- reactiveValues(
        updatedTreeNum=NULL,
        plotTree=NULL
        )
    
    observeEvent({ret()}, {
        if(is.null(ret())){
            return(NULL)
        }
        labels <- create_labels("Tree", length(ret()$parse_data$trees))
        updateSelectizeInput(session, "treeNum",
                             label = paste0("Select tree [", length(labels)," trees]"),
                             choices = labels
        )
        
        labels <- create_labels("DF", length(ret()$all_dff[[1]]))
        updateSelectInput(session, "dfNum",
                          label = "Select distinguishing feature",
                          choices = labels
        )
        
        labels <- create_labels(
            "Featurette", 
            length(ret()$all_dff[[1]][[1]])
        )
        labels[["Show all"]] <- 0
        labels[["Show none"]] <- -1
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = labels
        )
        if (is.null(chosenTree())){
            values$updatedTreeNum <- 1
        }
        else{
            values$updatedTreeNum <- chosenTree()
        }
        
    })
    
    observeEvent({chosenTree()}, {
        values$updatedTreeNum <- chosenTree()
        updateSelectizeInput(session, "treeNum", selected=as.numeric(chosenTree()))
    })
    
    observeEvent({input$treeNum}, {
        if(is.null(ret())){
            return(NULL)
        }
        labels <- create_labels("DF", length(ret()$all_dff[[as.numeric(input$treeNum)]]))
        updateSelectInput(session, "dfNum",
                          label = "Select distinguishing feature",
                          choices = labels
        )
        
        labels <- create_labels(
            "Featurette", 
            length(ret()$all_dff[[as.numeric(input$treeNum)]][[1]])
        )
        labels[["Show all"]] <- 0
        labels[["Show none"]] <- -1
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = labels
        )
        values$updatedTreeNum <- input$treeNum
    })
    
    observeEvent({input$dfNum}, {
        if(is.null(ret())){
            return(NULL)
        }
        labels <- create_labels(
            "Featurette", 
            length(ret()$all_dff[[as.numeric(input$treeNum)]][[as.numeric(input$dfNum)]])
        )
        labels[["Show all"]] <- 0
        labels[["Show none"]] <- -1
        updateSelectInput(session, "featuretteNum",
                          label = "Select featurette",
                          choices = labels
        )
        values$updatedTreeNum <- input$treeNum
    })
    
    output$downloadTree <- downloadHandler(
        filename = function(){
            paste0(dtn,"_", "tree", input$treeNum, ".pdf")
        },
        content = function(file){
            d_graph <- d_graph()
            if (is.null(d_graph)){
                return()
            }
            dot_version <- d_graph %>% 
                add_global_graph_attrs(attr="layout", value="dot", attr_type = "graph")
            export_graph(dot_version, file_name = file, file_type = "pdf")
        },
        contentType="application/pdf"
    )
    
    observeEvent(input$plotTree,{
        values$plotTree <- input$treeNum
    })
    
    return(
        list(
            treeNum = reactive({ values$updatedTreeNum }),
            dfNum = reactive({ input$dfNum }),
            featuretteNum = reactive({ input$featuretteNum }),
            showF = reactive({ input$showF }),
            showU = reactive({ input$showU }),
            plotTree = reactive({ values$plotTree })
        )
    )
}

treeVis <- function(input, output, session, 
                    parse_data, all_dff, 
                    treeToggles,
                    chosenSample) {
    
    values <- reactiveValues(d_graph=NULL)
    
    output$graphV <- renderGrViz({
        chosenSample <- chosenSample()
        values$d_graph <- get_diagrammer_tree(parse_data(), 
                                       all_dff(), 
                                       treeToggles$treeNum(), 
                                       treeToggles$dfNum(), 
                                       treeToggles$featuretteNum(),
                                       treeToggles$showU(),
                                       treeToggles$showF(),
                                       chosenSample
        )
        if (is.null(values$d_graph)){
            return(NULL)
        }
        render_graph(values$d_graph, layout="tree")
    })
    
    return(reactive(values$d_graph))
}