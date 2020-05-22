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
library(plotly)

ui <- fluidPage(
    useShinyjs(),
    titlePanel("PhyDOSE-IT, v0.0.1"),
    
    tabsetPanel(
        type = "tabs",
        tabPanel(
            "PhyDOSE-IT Interface", 
            sidebarLayout(
                sidebarPanel(
                    fileInput("file", "Input file:", multiple = TRUE, 
                              accept = c(".csv", ".tsv", ".txt")),
                    sliderInput("quantile", "Quantile for k*:",0,1,1,step=0.01,ticks=FALSE),
                    sliderInput("fn", "False negative rate:",0,0.5,0,step=0.01,ticks=FALSE),
                    disabled(actionButton("run", "Run PhyDOSE")),
                    textOutput("displaymessage"),
                ),
                #conditionalPanel(
                #    condition="output.timeToDisplay == true",
                    #h3("lwtxd",align="center")
                mainPanel(
                    fluidRow(
                        column(12, align="center",
                               h3(textOutput("num_cells")), 
                            )
                    ),
                    fluidRow(
                        column(
                            8,
                            plotlyOutput(outputId="kvscl"),
                            align="center"
                            ),
                        column(
                            4,
                            plotlyOutput(outputId ="box_jitter"),
                        )
                    ),
                    br(),
                    fluidRow(
                        column(12, align="center",
                        sliderInput("gamma", 
                                    "Confidence level",
                                    0.01,0.99,0.95,
                                    step=0.1,
                                    ticks=FALSE),
                        )
                    ),
                    hr(),
                    h3("Visualize tree", align="center"),
                    fluidRow(
                        column(align="center",
                            selectInput("treeNum", "Select Tree:",
                                               c()),
                            selectInput("dfNum", "Select Distinguishing Feature:",
                                        c()),
                            selectInput("featuretteNum", "Select Featurette:",
                                        c()),
                            checkboxInput("plotThisTree", "Plot this tree", FALSE),
                            #h4(textOutput("which_dff")),
                            #textOutput("display_dff"),
                            width=5,
                        ),
                        column(align="center",grVizOutput('graphV'),width=7),
                    ),
                    
                #),
                )
            )
                ),
        tabPanel("About", ),
        tabPanel("FAQ", 
                 )
    ),
    
   
)