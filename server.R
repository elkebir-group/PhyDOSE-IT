library(shiny)
library(shinyFiles)
library(stringr)
library(sjmisc)
library(dplyr,warn.conflicts=FALSE)
library(tidyr)
library(PhyDOSE)
library(shinyjs)
library(DiagrammeR)
library(shinyAce)
library(ggplot2)
library(rlist)

server <- function(input, output, session) {
    observeEvent(input$file, {
        enable("run")
        hide("displaymessage")
        #addTooltip(session, "run", "haluuu", placement = "bottom", trigger = "hover",
        #          options = NULL)
    })
    
    output$displaymessage <- renderText({ 
        "Upload your file"
    })
    
    ret <- eventReactive(input$run, {
        ret <- PhyDOSE(input$file[['datapath']], 
                       conf_level=input$gamma, 
                       fn_rate=input$fn)
        # update the selection of trees based on what phydose parsed
        graphs <- ret$graphs
        labels <- list()
        optionsid <- 1:length(graphs)
        for(i in 1:length(optionsid)){
            lab <- paste("Tree", i)
            labels[[lab]] <- i
        }
        
        updateSelectInput(session, "treeNum",
                          label = paste0("Select tree [", length(labels)," trees]"),
                          choices = labels
        )
        
        return(ret)
    })
    
    output$graphV <- renderGrViz({ 
        ret <- ret()
        graphs <- ret$graphs
        if (input$treeNum == ""){
            return (NULL)
        }
        shinyjs::show("treeNum")
        shinyjs::show("which_dff")
        grViz( graphs[[as.numeric(input$treeNum)]] )
    })
    
    output$kvscl <- renderPlot({
        ret <- ret()
        res <- ret$plot_vals
        gamma_min <-filter(res, gamma >= input$gamma) %>% top_n(1, -gamma)
        max_cells <- tail(res$k, n=1)
        max_cells <- ceiling(max_cells/10)*10
        p <-ggplot(res, aes(x=k, y=gamma)) + geom_step(size=1.5) + ylab("Confidence level") +
            geom_hline(aes(yintercept = input$gamma), linetype = "dotdash") + 
            geom_vline(aes(xintercept=gamma_min$k[1]), linetype = "dotdash") +
            theme(axis.text.x = element_text(size = rel(1.7)),
                  axis.text.y = element_text(size = rel(1.7))) +
            scale_y_continuous(breaks = seq(0,1, by=0.1)) +
            scale_x_continuous() + xlab("Number of cells to sequence")
        return(p)
    })
    
    output$k_star <- renderText({
        ret <- ret()
        res <- ret$plot_vals
        gamma_min <-filter(res, gamma >= input$gamma) %>% top_n(1, -gamma)
        return(gamma_min$k[1])
    })
    
    output$num_cells <- renderText({
        ret <- ret()
        res <- ret$plot_vals
        gamma_min <-filter(res, gamma >= input$gamma) %>% top_n(1, -gamma)
        return(paste0(gamma_min$k[1], " cells required to sequence with confidence level = ", input$gamma))
    })
    
    output$which_dff <- renderText({
        return(paste0("Distinguishing feature family for Tree ", input$treeNum))
    })
    
    output$display_dff <- renderText({
        ret <- ret()
        dff <- ret$dff
        if (input$treeNum == ""){
            return (NULL)
        }
        mydff <- dff[[as.numeric(input$treeNum)]]
        disp <- ""
        for(i in 1:length(mydff)){
            disp <- paste0(disp, "Distinguishing feature ", i, " : ", mydff[[i]], "\n")
        }
        return(disp)
    })
}