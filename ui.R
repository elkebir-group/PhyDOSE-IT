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

ui <- dashboardPage(
    dashboardHeader(title = "PhyDOSE-IT"),
    
    dashboardSidebar(
        fileInput("file", "Input file:", multiple = TRUE, 
                  accept = c(".csv", ".tsv", ".txt")),
        sliderInput("quantile", "Quantile for k*:",0,1,1,step=0.01,ticks=FALSE),
        sliderInput("fn", "False negative rate:",0,0.5,0,step=0.01,ticks=FALSE),
        actionButton("run", "Run PhyDOSE"),
        sidebarMenu(
            menuItem("Advanced toggles", 
                     tabName="advanced",
                     icon = icon("spinner"),
                     sliderInput("gmin", "Gamma minimum:",0.01,0.99,0.01,step=0.01,ticks=FALSE),
                     sliderInput("gmax", "Gamma maximum:",0.01,0.99,0.99,step=0.01,ticks=FALSE),
                     sliderInput("gresolution", "Gamma resolution:",0.01,0.1,0.01,step=0.01,ticks=FALSE)
            )
        ),
        sidebarMenu(
            menuItem("Example datasets", 
                     tabName="egdatasets",
                     icon = icon("spinner"),
                     actionButton("runSim","Simulation dataset 1"),
                     actionButton("runALL","ALL patient"),
                     actionButton("runAML","AML patient")
                     )
        )
    ),
    
    dashboardBody(
        fluidRow(
            column(12, align="center",
                   h3(textOutput("num_cells")), 
            )
        ),
        fluidRow(
            box(
                width=9,
                plotlyOutput(outputId="kvscl"),
            ),
            box(
                width=3,
                plotlyOutput(outputId ="box_jitter"),
            ),
        ),
        br(),

        fluidRow(
            box(
                width=4,
                h4("Details"),
                textOutput("treeNumDetail"),
                textOutput("sampleNumDetail"),
                textOutput("quantileNumDetail")
            ),
                box(width=7, align="center",
                       sliderInput("gamma", 
                                   "Confidence level",
                                   0.01,0.99,0.95,
                                   step=0.01,
                                   ticks=FALSE,
                                   width='80%'),
                ),
                actionButton("clearChosenTree", "Reset graphs"),
        ),
        fluidRow(
            h3("Compare and visualize trees", align="center"),
            box(
                width = 6,
                column(align="center",
                       selectInput("treeNum", "Select Tree:",
                                   c()),
                       selectInput("dfNum", "Select Distinguishing Feature:",
                                   c()),
                       selectInput("featuretteNum", "Select Featurette:",
                                   c()),
                       actionButton("plotTree1", "Plot this tree"),
                       width=5
                ),
                column(align="center",grVizOutput('graphV'),width=7),
            ),
            
            
            
            box(
                width = 6,
                column(align="center",
                       selectInput("treeNum2", "Select Tree:",
                                   c()),
                       selectInput("dfNum2", "Select Distinguishing Feature:",
                                   c()),
                       selectInput("featuretteNum2", "Select Featurette:",
                                   c()),
                       actionButton("plotTree2", "Plot this tree"),
                       width=5
                ),
                column(align="center",grVizOutput('graphV2'),width=7),
                ),
        ),
  
    ),
    
   
)