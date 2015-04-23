library(shiny)
library(shinythemes)
library(ggvis)
library(data.table)
library(dplyr)
library(bit64)

fdf <<- fread('data-vis-4.csv',sep=',',header=T)
#fdf <- fread('../data/data-vis-4.csv',sep=',',header=T)
#tdf <- fdf
#fdf <- filter(fdf, 
#              !is.na(rlevel), 
#              maxdesc > 3)

shinyUI(
  fluidPage(
    
    theme = shinytheme("spacelab"),
    
    titlePanel("Convo Browser"),
    br(),
   
    sidebarLayout(
      sidebarPanel(
        width = 4,
        sliderInput(inputId="maxdescs","Limit to Convos of This Depth",
                    min = 4, max = 58, value = c(4,58)),
        selectizeInput(inputId = "root","Select Convo Roots", 
                       as.character(unique(fdf$root)), 
                       #selected = "558770074945744896",
                       width = 300, 
                       multiple = TRUE),
        sliderInput(inputId="numtrees","Or: Select This Many Random Trees",
                     value=1,min=1,max=min(20,length(unique(fdf$root)))),
        actionButton(inputId="redraw","Select new trees"),
        radioButtons(inputId="plottype","Plot Type",
                       choices=list("Separate Trees"="multi","Single Overlapping"="single"),
                       selected="multi"),
        checkboxGroupInput(inputId="lines","Regression lines?",
                       choices=list("Grouped"="single","Per Convo"="multi")),
        radioButtons(inputId="tweetvar","Tweet Variable",
                       choices=list("Per-word entropy"="pw","Per-tweet entropy"="pt","# words"="w"),
                       selected="pt"),
        checkboxInput(inputId="resid","Residualize? (Transitions only)",
                       value=F)
      ),
      
      mainPanel(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        tabsetPanel(
          tabPanel("Conversation structures",                  
                   plotOutput("tree")),
          tabPanel("Transitions",
                   plotOutput("trans"))
        )
      )
    )
  )
)


#     sidebarLayout(
#       sidebarPanel(
#         width = 3,
#         checkboxGroupInput('pred_vars', 'Predictors:',
#                            pred_names, selected = pred_names[1:3]),
#         radioButtons("labels", "Labels on individual words?",
#                      selected = FALSE,                   
#                      c("off" = FALSE,
#                        "on" = TRUE)),      
#         sliderInput("x_domain", "Model AoFP range (months)", 
#                     min = 9, max = 24, value = c(15, 23)), 
#         sliderInput("y_domain", "Child AoFP range (months)",  
#                     min = 9, max = 24, value = c(9, 24))
#       ), 
#       


#           tabPanel("Model Summary",
# #                    h4("Regression model output"),
#                    br(), 
#                    tableOutput("summary"),
#                    textOutput(outputId = "r2")),
#           tabPanel("Browse Raw Data",                   
#                    dataTableOutput(outputId = "datatable"))
#           
#           #           tabPanel("Download Raw Data",
#           #                    downloadButton('downloadData', 'download'))
