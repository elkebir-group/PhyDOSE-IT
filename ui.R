

ui <- dashboardPage( skin='black',
    dashboardHeader(title = "PhyDOSE-IT"),
    
    dashboardSidebar(
        fileInput("file", "Input file:", multiple = TRUE, 
                  accept = c(".csv", ".tsv", ".txt")),
        sliderInput("quantile", "Quantile for k*:",0,1,1,step=0.01,ticks=FALSE),
        sliderInput("fn", "False negative rate:",0,0.5,0,step=0.01,ticks=FALSE),
        
        sidebarMenu(
            menuItem("Advanced toggles", 
                     tabName="advanced",
                     icon = icon("plus"),
                     sliderInput("gmin", "𝛾 minimum:",0.00,0.9,0.00,step=0.1,ticks=FALSE),
                     sliderInput("gmax", "𝛾 maximum:",0.1,0.99,0.99,step=0.1,ticks=FALSE),
                     sliderInput("gresolution", "𝛾 resolution:",0.01,0.1,0.1,step=0.01,ticks=FALSE)
            )
        ),
        actionButton("run", "Run PhyDOSE", width='85%'),
        sidebarMenu(
            menuItem("Example datasets", 
                     tabName="egdatasets",
                     icon = icon("table"),
                     #actionButton("runSim","Simulation dataset 1",width='auto'),
                     #actionButton("runALL","ALL patient"),
                     menuItem("AML Patient 38",
                              actionButton("runAML","Run PhyDOSE", style="margin: 6px 5px 6px 0px; width:90%"),
                              downloadButton("downloadAML", "Download dataset", style="width:90%")
                              ),
                     menuItem("Simulation Dataset 1",
                              actionButton("runSim1","Run PhyDOSE", 
                                           style="margin: 6px 5px 6px 0px; width:90%"),
                              downloadButton("downloadSim1", "Download dataset", style="width:90%")
                     )
                     )
            
        )
    ),
    
    dashboardBody(
      useShinyjs(),
      tabsetPanel(
        tabPanel("Main",
                 shinyjs::hidden(
                   div(id="mainpanel",
                                     fluidRow(
                                       column(12, align="center",
                                              h3("Design of Follow-up Single-cell Sequencing Experiments of Tumors"), 
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         width=9,
                                         h4(textOutput("num_cells")),
                                         plotlyOutput(outputId="kvscl"),
                                         br(),
                                         sliderInput("gamma", 
                                                     "Confidence level",
                                                     0.52,0.99,0.52,
                                                     step=0.2,
                                                     ticks=FALSE,
                                                     width='90%'),
                                         align='center',
                                         height='560px'
                                       ),
                                       box(
                                         width=3,
                                         h4(textOutput("box_title")),
                                         plotlyOutput(outputId ="box_jitter"),
                                         br(),
                                         
                                         align='center',
                                         justify='center',
                                         height='560px',
                                         actionButton("clearChosenTree", "Reset", width='100%'),
                                       ),
                                     ),
                                     fluidRow(
                                       h3("Compare and Visualize Trees", align="center"),
                                       box(
                                         width = 6,
                                         column(width=5,
                                                align="center",
                                           treeVisTogglesInput("tree1input"),
                                           actionButton("plotTree1", "Plot tree", width='100%'),
                                         ),
                                         treeVisOutput("tree1")
                                       ),
                                       box(
                                         width = 6,
                                         treeVisOutput("tree2"),
                                         column(width=5,
                                                align="center",
                                                treeVisTogglesInput("tree2input"),
                                                actionButton("plotTree2", "Plot tree", width='100%'),
                                         ),
                                       ),
                                     ),
                 )
                 )),
        tabPanel("About",
                 includeMarkdown("www/about.md")
        ),
        tabPanel("FAQ",
                 includeMarkdown("www/faq.md")
        )
      ),
    ),
    
   
)