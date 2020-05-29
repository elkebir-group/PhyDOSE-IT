

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
                     sliderInput("gmin", "𝛾 minimum:",0.00,0.99,0.00,step=0.01,ticks=FALSE),
                     sliderInput("gmax", "𝛾 maximum:",0.01,0.99,0.99,step=0.01,ticks=FALSE),
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
                     actionButton("runAML","AML patient 38", width='85%')
                     )
        )
    ),
    
    dashboardBody(
      useShinyjs(),
      tabsetPanel(
        tabPanel("User Interface",
                 shinyjs::hidden(div(id="mainpanel",
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
                                                     0.01,0.99,0.95,
                                                     step=0.01,
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
                                         #tableOutput('detTable'),
                                         #DT::dataTableOutput('dtt')
                                       ),
                                     ),
                                     fluidRow(
                                       h3("Compare and Visualize Trees", align="center"),
                                       box(
                                         width = 6,
                                         column(align="center",
                                                selectizeInput("treeNum", "Select Tree:",
                                                               c(), options= list(maxOptions = 10000)),
                                                selectInput("dfNum", "Select Distinguishing Feature:",
                                                            c()),
                                                selectInput("featuretteNum", "Select Featurette:",
                                                            c()),
                                                checkboxInput("showF1", "Show F matrix"),
                                                checkboxInput("showU1", "Show U matrix"),
                                                actionButton("plotTree1", "Plot tree", width='100%'),
                                                tags$div(style='height:10px'),
                                                # actionButton("downloadTree1", "Download tree", width='100%'),
                                                downloadLink("downloadTree1", "Download tree"),
                                                width=5
                                         ),
                                         column(align="center",grVizOutput('graphV'),width=7),
                                       ),
                                       
                                       
                                       
                                       box(
                                         width = 6,
                                         column(align="center",grVizOutput('graphV2'),width=7),
                                         column(align="center",
                                                selectizeInput("treeNum2", "Select Tree:",
                                                               c(), options= list(maxOptions = 10000)),
                                                selectInput("dfNum2", "Select Distinguishing Feature:",
                                                            c()),
                                                selectInput("featuretteNum2", "Select Featurette:",
                                                            c()),
                                                checkboxInput("showF2", "Show F matrix"),
                                                checkboxInput("showU2", "Show U matrix"),
                                                actionButton("plotTree2", "Plot tree", width="100%"),
                                                tags$div(style='height:10px'),
                                                #actionButton("downloadTree2", "Download tree", width='100%',margin='10px'),
                                                downloadLink("downloadTree2", "Download tree"),
                                                width=5
                                         ),
                                         
                                       ),
                                     ),
                 ))),
        tabPanel("About",
                 includeMarkdown("www/about.md")
        ),
        tabPanel("FAQ",
                 includeMarkdown("www/faq.md")
        )
      ),
    ),
    
   
)