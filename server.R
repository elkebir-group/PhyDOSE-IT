

server <- function(input, output, session) {
    
    # Reactive values ----------------------------------------------------------
    values <- reactiveValues(
        chosenTree = NULL, # this is changed by tree vis, reset button, and initial run, and is read by the graphs
        tree1Num = NULL, # this is changed by the graphs, reset button, and initial run, and is read by tree 1
        chosenSample = NULL,
        parse_data = NULL,
        all_dff = NULL,
        ret = NULL,
        confSliderArgs = list("min"=0,"max"=0.99,"res"=0.01),
        fnr=NULL,
        quant=NULL,
        datasetName=NULL,
        d_graph1=NULL,
        d_graph2=NULL,
        trig = 0 #triggers the run button to execute every time it is clicked
    )
    
    runVals <- reactiveValues()
    
    phydoseData <- reactiveValues(
        gamma_list = NULL,
        cells = NULL,
        all_dff = NULL,
        per_tree_k = NULL,
        parse_data = NULL
    )
    
    # Observe if specific tree is chosen to be displayed -----------------------
    observeEvent(input$clearChosenTree,{
        req(runVals$ret())
        ret <- runVals$ret()
        updateSliderInput(session, "gamma",
                          value = values$confSliderArgs$max
        )
        values$tree1Num <- get_max_tree(ret$per_tree_k, values$confSliderArgs$max)
        values$chosenTree <- values$tree1Num
        values$chosenSample <- get_best_sample(ret$per_tree_k, values$confSliderArgs$max)
    })
    
    observeEvent(event_data(event="plotly_click", source="mysource"),{
        req(runVals$ret())
        ret <- runVals$ret()
        d <- event_data(event="plotly_click", source="mysource")
        values$tree1Num <- as.numeric(d$key)
        values$chosenTree <- values$tree1Num
        values$chosenSample <- as.numeric(d$customdata)
    })
    
    observeEvent(input$plotTree2, {
        values$chosenTree <- treeToggles2$treeNum()
    })
    observeEvent(input$plotTree1, {
        values$chosenTree <- treeToggles$treeNum()
        values$tree1Num <- values$chosenTree
    })
    
    # MAIN PLOT ----------------------------------------------------------------
    
    output$kvscl <- renderPlotly({
        req(runVals$ret())
        ret <- runVals$ret()
        p <- create_step_graph(
            gamma_list=ret$my_glist, 
            cells_list=ret$my_clist, 
            per_tree_k=ret$per_tree_k, 
            chosen_tree=values$chosenTree, 
            chosen_gamma=input$gamma,
            chosen_sample=values$chosenSample)
        ggplotly(p, tooltip=c("text","x","y"))
    })
    
    output$num_cells <- renderText({
      req(runVals$ret())
      ret <- runVals$ret()
      gamma_list <- ret$my_glist
      cells_list <- ret$my_clist
      mydata <- data.frame(gamma = gamma_list, cells = cells_list)
      
      if (!is.null(values$chosenTree)){
        mydata <- filter(ret$per_tree_k, tree_id == as.numeric(values$chosenTree))
      }
      
      gamma_min <-filter(mydata, gamma >= input$gamma) %>% top_n(1, -gamma)
      return(paste0(gamma_min$cells[1], " cells required to sequence with confidence level 𝛾 = ", input$gamma))
    })
    
    # BOX PLOT -----------------------------------------------------------------
    
    output$box_jitter <- renderPlotly({
        req(runVals$ret())
        ret <- runVals$ret()
        p <- create_box_jitter(
            per_tree_k=ret$per_tree_k,
            chosen_tree=values$chosenTree,
            chosen_gamma=input$gamma,
            chosen_sample=values$chosenSample
            )
        ggplotly(p, source="mysource")
    })
    
    output$box_title <- renderText({
      req(runVals$ret())
      ret <- runVals$ret()
      q_t <- get_quantile_and_tree(ret$per_tree_k, values$chosenTree, input$gamma, values$chosenSample)
      trunc_q <- sprintf("%.3f", q_t$quantile)
      return(paste0("Tree #", q_t$tree, " with quantile = ", trunc_q))
    })
    
    # GAMMA TOGGLES ------------------------------------------------------------
    
    observeEvent(runVals$confSliderArgs(),{
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "gamma",
                          min = runVals$confSliderArgs()$min,
                          max = runVals$confSliderArgs()$max,
                          step = as.numeric(runVals$confSliderArgs()$res),
                          value = runVals$confSliderArgs()$max
                          )
    })
    
    observe({
        updateSliderInput(session, "gmin", max = input$gmax-0.01)
    })
    observe({
        updateSliderInput(session, "gmax", min = input$gmin+0.01)
    })
    
    # DOWNLOAD DATASETS --------------------------------------------------------
    
    output$downloadAML <- downloadHandler(
        filename = function(){
            "AML38.txt"
        },
        content = function(file){
            file.copy(system.file("extdata", "AML38.txt", package = "phydoser"), file)
        },
    )
    
    output$downloadSim1 <- downloadHandler(
        filename = function(){
            "multsample.txt"
        },
        content = function(file){
            file.copy(system.file("extdata", "multsample.txt", package = "phydoser"), file)
        },
    )
    
    # Compare and Visualize Trees ----------------------------------------------
    values$d_graph1 <- callModule(treeVis,
                                  "tree1",
                                  reactive(runVals$ret()$parse_data),
                                  reactive(runVals$ret()$all_dff),
                                  treeToggles,
                                  runVals$chosenSample
    )
    
    treeToggles <- callModule(treeVisToggles, "tree1input", runVals$ret, 
                              d_graph=values$d_graph1, dtn=values$datasetName,
                              chosenTree=reactive(values$tree1Num))
    
    values$d_graph2 <- callModule(treeVis,
                                  "tree2",
                                  reactive(runVals$ret()$parse_data),
                                  reactive(runVals$ret()$all_dff),
                                  treeToggles2,
                                  runVals$chosenSample
    )
    
    treeToggles2 <- callModule(treeVisToggles, "tree2input", runVals$ret, 
                              d_graph=values$d_graph2, dtn=values$datasetName,
                              chosenTree=reactive(NULL))
    
    # RUN PHYDOSE --------------------------------------------------------------
    runVals <- callModule(runPhydose, "runphydose",
                   reactive(values$parse_data),
                   reactive(values$all_dff),
                   reactive(input$gmin), reactive(input$gmax), reactive(input$gresolution),
                   reactive(input$fn), reactive(input$percentile), reactive(input$gamma),
                   reactive(values$trig)
               )
    
    observeEvent(runVals$ret(),{
        values$chosenSample <- runVals$chosenSample()
        values$chosenTree <- runVals$chosenTree()
        values$tree1Num <- runVals$tree1Num()
        values$confSliderArgs <- runVals$confSliderArgs()
        values$ret <- runVals$ret()
        shinyjs::show('mainpanel')
    })
    
    # Simulation
    observeEvent(input$runSim1, {
      values$parse_data <- multsample
      values$all_dff <- generateDistFeat(multsample$trees)
      values$trig <- values$trig+1
      values$datasetName <- "Sim1"
    })
    
    # AML
    observeEvent(input$runAML, {
      values$parse_data <- AML38
      values$all_dff <- AML38DFF
      values$trig <- values$trig+1
      values$datasetName <- "AML38"
    })
    
    # main
    observeEvent(input$run, {
      if (is.null(input$file)){
        showModal(modalDialog(
          "Please upload a file or run an example dataset",
          easyClose = TRUE
        ))
        return(NULL)
      }
      withProgress(message = 'Parsing file', value = 0, {
        # 1. file is parsed to get trees, frequency matrix, and u matrix
        incProgress(0.2, detail = "Getting trees and frequency matrices")
        values$parse_data <- ReadJoint(input$file[['datapath']])
        
        #parse_data <- ALLPatient2
        # 2. using output from file parse, call the getdff to get dff
        incProgress(0.3, detail = "Finding distinguishing features...")
        values$all_dff <- generateDistFeat(values$parse_data$trees)
      })
      values$datasetName <- input$file[['name']]
      values$trig <- values$trig+1
    })
}