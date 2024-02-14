library(shiny)
library(shinydashboard)
library(DT)
source("./shiny/helper_file_shiny.R")
source("./shiny/server.R")

# UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "ACDC overview"),
  
  # SIDEBAR 
  dashboardSidebar(
    width = 500,
    tabsetPanel(
      type = "tabs",
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
    )
  )
  ),  # end sidebar
  
  dashboardBody(
    # Body content (originally in mainPanel)
    fluidRow(
      column(12, htmlOutput("short_intro"))
    ),
    #hr(),
    fluidRow(
      column(12, 
             actionButton("action_explain_db", "What is the ACDC data base?"),
             htmlOutput("explanation_db")
      )
    ),
    br(),
    fluidRow(
      column(12, 
             actionButton("action_contribute", "How can I contribute my data to the data base?"), 
             htmlOutput("explanation_contribute")
      )
    ),
    hr(),
    br(),
    tabsetPanel(
      type = "tabs",
      
      # TAB 1
      tabPanel("Overview of suited datasets", 
               span(textOutput("number_hits"), style="color:darkcyan"),
               br(),
               DTOutput("suited_datasets")),
      
      # TAB 2
      tabPanel("Descriptives",
               DTOutput("descriptives")
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
               textOutput("test"),
               
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
      tabPanel("Overview of all datasets in ACDC", 
               dataTableOutput("overview_datasets")
      )
      
      
    )
  )
)



# Run the application
shinyApp(ui, server)
