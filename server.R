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

server <- function(input, output, session) {
    
    # observe if file has been uploaded
    observeEvent(input$file, {
        enable("run")
        hide("displaymessage")
    })
    
    # display help message
    output$displaymessage <- renderText({ 
        "Upload your file"
    })
    
    # run button is pressed
    ret <- eventReactive(input$run, {
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
            gamma_list <- seq(0.0, 0.99, by=0.01)
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
            graphs <- ret$graphs
            labels <- list()
            optionsid <- 1:length(parse_data$trees)
            print(paste("There are this many trees:", length(parse_data$trees)))
            for(i in 1:length(optionsid)){
                lab <- paste("Tree", i)
                labels[[lab]] <- i
            }
            updateSelectInput(session, "treeNum",
                              label = paste0("Select tree [", length(labels)," trees]"),
                              choices = labels
            )
            
            # update the selection of distinguishing features
            mydff <- all_dff[[1]]
            print("This is my dff")
            print(mydff)
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
            
            featurettes <- mydff[[1]]
            print("These are the featurettes for my dff")
            print(paste("I have this many featurettes", length(featurettes)))
            print(featurettes)
            feat_labels <- list()
            for(i in 1:length(featurettes)){
                lab <- paste("Featurette", i)
                feat_labels[[lab]] <- i
            }
            updateSelectInput(session, "featuretteNum",
                              label = "Select featurette",
                              choices = feat_labels
            )
            
            incProgress(0.2, detail = "Done!")
            ret <- list("my_glist"=gamma_list,
                        "my_clist"=cells,
                        "per_tree_k"=per_tree_k,
                        "parse_data"=parse_data,
                        "all_dff"=all_dff
            )
            incProgress(0.2, detail = "Done!!")
        })
        
        #res <- data.frame(k=cells, gamma = gamma_list)
        # 4. get the dot representation of the selected tree (default to first tree)
        # tree_diag <- plotTree(parse_data$trees[[1]])
        # # 5. put everything in the return value
        # ret <- list("my_glist"=gamma_list,
        #             "my_clist"=cells,
        #             "per_tree_k"=per_tree_k,
        #             "parse_data"=parse_data
                    
        
        return(ret)
    })
    
    # use this to hide the main panel (but there is a second delay so you can stillsee the panel for a sec)
    output$timeToDisplay <- reactive({
        return(!is.null(ret()))
    })
    outputOptions(output, 'timeToDisplay', suspendWhenHidden=FALSE)
    
    
    # render the tree
    output$graphV <- renderGrViz({ 
        ret <- ret()
        if (input$treeNum == ""){
            return (NULL)
        }
        tree <- ret$parse_data$trees[[as.numeric(input$treeNum)]]
        featurette <- ret$all_dff[[as.numeric(input$treeNum)]][[as.numeric(input$dfNum)]][[as.numeric(input$featuretteNum)]]
        
        #print(ret$all_dff[[as.numeric(input$treeNum)]][[as.numeric(input$dfNum)]][[as.numeric(input$featuretteNum)]])
        print("this is the featurette displayed")
        print(featurette)
        d_graph <- plotTree(tree, featurette=featurette)
        render_graph(d_graph, layout="tree")
    })
    
    output$kvscl <- renderPlotly({
        # generate data for plot
        # create the plot
        ret <- ret()
        gamma_list <- ret$my_glist
        cells_list <- ret$my_clist
        mydata <- data.frame(gamma = gamma_list, cells = cells_list)

        if (input$plotThisTree){
            mydata <- filter(ret$per_tree_k, tree_id == as.numeric(input$treeNum))
        }
        
        gamma_min <-filter(mydata, gamma >= input$gamma) %>% top_n(1, -gamma)
        max_cells <- tail(mydata$cells, n=1)
        max_cells <- ceiling(max_cells/10)*10
        
        p <-ggplot(mydata, aes(x=cells, y=gamma)) + geom_step(size=1.5, color="darkblue") + ylab("confidence level") +
                geom_hline(aes(yintercept = input$gamma), linetype = "dotdash") +
                geom_vline(aes(xintercept=gamma_min$cells[1]), linetype = "dotdash") +
                scale_y_continuous(breaks = seq(0,1, by=0.1)) +
                xlab("number of cells to sequence")
        ggplotly(p)
    })
    
    output$box_jitter <- renderPlotly({
        ret <- ret()
        
        max_cells <- tail(ret$per_tree_k$cells, n=1)
        max_cells <- ceiling(max_cells/10)*10
        filtered_k <- filter(ret$per_tree_k, gamma >= input$gamma-0.001 & gamma <= input$gamma+0.001)
        p <- ggplot(filtered_k, aes(x=sample, y=cells))+
            geom_boxplot() +
            geom_jitter(position=position_jitter(width=0.5,height=0.01)) +
            scale_x_discrete(limits=c("1"), label=c("1"))+
            scale_y_continuous(limits=c(0,max_cells+1))+
            ylab("number of cells to sequence")+
            xlab("biopsy")
            
        ggplotly(p)
        #%>% config(displayModeBar = FALSE)
    })
    
    output$num_cells <- renderText({
        ret <- ret()
        gamma_list <- ret$my_glist
        cells_list <- ret$my_clist
        mydata <- data.frame(g_list = gamma_list, c_list = cells_list)
        
        gamma_min <-filter(mydata, g_list >= input$gamma) %>% top_n(1, -g_list)
        return(paste0(gamma_min$c_list[1], " cells required to sequence with confidence level = ", input$gamma))
    })
}