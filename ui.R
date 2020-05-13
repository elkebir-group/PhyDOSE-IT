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

ui <- fluidPage(
    useShinyjs(),
    titlePanel("PhyDOSE-IT, v0.0.1"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", NULL, multiple = TRUE, accept = c(".csv", ".tsv", ".txt")),
            sliderInput("gamma", "Confidence level",0.01,0.99,0.95,step=0.01,ticks=FALSE),
            sliderInput("fn", "False Negative Rate",0,0.5,0,step=0.01,ticks=FALSE),
            disabled(actionButton("run", "Run PhyDOSE")),
            textOutput("displaymessage"),
        ),
        mainPanel(
            h3(textOutput("num_cells"),align="center"),
            plotOutput("kvscl",width = "1200px", height = "600px"),
            fluidRow(
                column(
                    hidden(selectInput("treeNum", "Select Tree:",
                                   c())),
                    h4(hidden(textOutput("which_dff"))),
                    textOutput("display_dff"),
                    width=5,
                    ),
                column(grVizOutput('graphV'),width=7),
            ),
            
        )
    )
)