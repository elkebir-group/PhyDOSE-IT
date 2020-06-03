# INSTALL DEPENDENCIES ---------------------------------------------------------

source('dependencies.R')

# LOAD UTILS.R -----------------------------------------------------------------

source("R/utils.R", local=TRUE)

# UI MODULES -------------------------------------------------------------------

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