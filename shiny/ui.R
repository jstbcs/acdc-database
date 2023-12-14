# 
# User interface for the inhibition data base shiny app 
# 

library(shiny)
library(DT)
source("./shiny/helper_file_shiny.R")


ui <- fluidPage(
  
  # TITLE 
  titlePanel("Information about data in the Attentional Control Data Collection data base"),
  
  # INTRO PART
  
  # short intro text
  fluidRow(
    column(12,
           htmlOutput("short_intro"))
  ),
  
  hr(),
  
  # action button explaining the project 
  fluidRow(
    column(12, 
           actionButton("action_explain_db", "What is the ACDC data base?")),
           htmlOutput("explanation_db")
          # imageOutput("img_structure_db")
  ), 
  
  br(),
  
  # action button explaining how to contribute
  fluidRow(
    column(12, 
           actionButton("action_contribute", "How can I contribute my data to the data base?"), 
           htmlOutput("explanation_contribute")
    )
  ),
  
  hr(),
  

  # MAIN PART OF SHINY APP: USER INPUT & OUTPUT PANELS  
  br(),
  
  # SIDE BAR FOR USER INPUT
  sidebarPanel(
    
    # create tabs
    shiny::tabsetPanel(
      type = "pills",
      
      # TAB 1
      tabPanel("Filter datasets",
               
               # add argument ---
               fluidRow(    # split sidebar into 3 columns 
                 column(4, 
                        # drop down menu criterion
                        selectInput(inputId = "criterion1",
                                    label = "Choose criterion:",
                                    choices = c(" ", criteria),
                                    selected = "")), 
                 
                 column(4, 
                        # conditional operator
                        uiOutput("operator1")),
                 
                 column(4, 
                        # conditional value field 
                        uiOutput("value1"),
                        # binary choice for yes/no questions
                        uiOutput("yes_no_choice"),
                        # choice of task type(s)
                        uiOutput("choice_task_type"),
                        # choice of pub_code
                        uiOutput("choice_pubcode")
                        )
               ), # end fluid row
               
               
               fluidRow(
                 column(4),  # empty 
                 
                 column(4),  # empty 
                 
                 column(4, 
                        # conditional second value for "between" operator
                        uiOutput("value1b"))
               ), # end fluid row
               
               # option to add argument ---
               fluidRow(
                 column(4, 
                        actionButton("action_add_arg", "Add argument to list")),
                 
                 column(4),  # empty 
                 
                 column(4)  # empty 
               ), # end fluid row
               
               # summary of chosen arguments ---
               tableOutput("summary"),
               
               fluidRow(
                 column(6,  # button remove recent argument
                        uiOutput("conditional_action_remove")),   
                 
                 column(6, # button reset list
                        uiOutput("conditional_action_reset"))
               ) # end fluid row
               
               ),
      
    ) # end tabsetpanel 
  
    ), # end sidebar
  
  # MAIN PANEL FOR OUTPUT
  mainPanel(
    # create tabs for main bar
    shiny::tabsetPanel(
      type = "pills",
      
      # TAB 1
      tabPanel("Overview of suited datasets",
               span(textOutput("number_hits"), style="color:darkcyan"),
               br(),
               DTOutput("suited_datasets")),
      
      # TAB 2
      tabPanel("Descriptives",
              DTOutput("descriptives")#,
              #plotOutput("histogram")
              ),
      
      # TAB 3
      tabPanel("Descriptive plots",
               # let user choose order of x axis for histogram
               selectInput(inputId = "sort_x_axis",
                           label = "Order by:",
                           choices = c("Dataset ID", 
                                       "Mean reaction time (dataset)",
                                       "Mean accuracy (dataset)"),
                           selected = "Dataset ID"),
               
               plotOutput("histogram"), 
               
               # let user choose dataset_id for rt plot
               selectInput("choose_dataset_id", "Choose Dataset ID for rt plot",
                           choices = NULL),
               
               plotOutput("rt_dist")
               

               ),
      
      # TAB 4
      tabPanel("Get the data",
               
               # print R code
               h2("Use this code to access the data in R:"),
               verbatimTextOutput("Rcode"),
               
               # option to download data as csv file
               h2("Or download it directly as a csv file:"), 
               downloadLink("downloadData", "Download csv file")
      ),
      
      # TAB 5
      tabPanel("Overview of all publications in ACDC", 
               tableOutput("overview_datasets")
      )
    
    ) # end tabset 
  ) # end main bar
)

