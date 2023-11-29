# # R Shiny Sever
#
# Defining server for the "ACDC data base" shiny app
#

library(shiny)
library(knitr)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(acdcquery)
source("./shiny/helper_file_shiny.R")
source("./shiny/ui.R")

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)



server <- function(input, output, session){
  
  # FOR INTRO ---
  
  # print short intro text
  output$short_intro <- renderUI({
    HTML("This shiny app provides an overview over datasets in the Attentional Control Data Collection (ACDC) data base . <br> <br>
         You can filter datasets by criteria such as task type and sample size using the first tab of the sidebar labeled 'filter datasets'. <br>
         The main panel provides an overview of those datasets fulfilling your filter criteria (tab 1), descriptive attributes of these 
         datasets (such as mean accuracy and respose time distributions; tab 2), and instructions on how to access these datasets using our
         R package acdcquery and an option to download trial level data as a csv (tab 3). <br> <br>
         In the second/third tab of the sidebar you also find an overview over all datasets currently included in ACDC. <br>")
  })
  
  # print explanation of project 
  output$explanation_db <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_explain_db %% 2 == 1){
      updateActionButton(inputId = "action_explain_db", label = "Got it!")
      renderUI({HTML("The ACDC data base contains attentional control task data (i.e., Stroop, flanker or Simon task) from over 40 datasets as well as information about the respective studies and publications. <br>
                     It is meant to enhance access to open attentional control task data. <br>
                     Data can be accessed either via SQL or our C-RAN R package ACDC query (<a href=http://github.com/SLesche/acdc-query target='_blank' rel='noopener noreferrer'>Github</a>). <br> <br>
                     <img src='/shiny/www/db_structure.png' alt='Structure of inhibition task db' width='400' height='400'>"
                     )
        })
    } else {
      updateActionButton(inputId = "action_explain_db", label = "What is the ACDC data base?")
      tagList()
    }
  })
  
  # print description of how to contribute 
  output$explanation_contribute <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_contribute %% 2 == 1){
      updateActionButton(inputId = "action_contribute", label = "Got it!")
      renderUI({HTML("If you have data on attention control tasks that you would like to make available to other researchers, we would be happy to include it in our data base. <br>
                     Note that the study/ studies which data were collected for must have been published (this includes preprints). <br>
                     Furthermore, suited data must include the following information: <ul>
                     <li>An ID variable</li> 
                     <li>A congruency variable, indicating stimuli were congruent or conflicting </li> 
                     <li>Reaction time of each trial, in seconds </li>
                     <li>Accuracyof each trials (correct/ incorrect)</li>
                     <li>A between variable indicating between subject manipulation (if applicable)</li>
                     <li>A within variable indicating within subject manipulation (if applicable)</li>
                     <br> 
                     You can submit your data via <a href=http://www.ampl-psych.com/inhibition-database/ target='_blank' rel='noopener noreferrer'>this</a> online form. <br>
                     In case you have any questions, feel free to contact <a href = 'mailto: j.m.haaf@uva.nl'>j.m.haaf@uva.nl</a>.</div>"
                     )})
    } else {
      updateActionButton(inputId = "action_contribute", label = "How can I contribute my data to the data base?")
      tagList()
    }
  })
  
  # SIDEBAR PANELS -----#
  
  # tab 1 --#
  
  # add arguments
  # conditional panel to choose 1st operator based on criterion1
  output$operator1 <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 != ' ' & input.criterion1 != 'task_name' & input.criterion1 != 'publication_code' & input.criterion1 != 'Neutral stimuli included?' &  input.criterion1 != 'Existence of between-subject manipulation?' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)?'",
      selectInput(inputId = "operator1",
                  label = "Choose operator",
                  choices =  c("", "less", "greater", "between", "equal"))
      )
  })
  
  # conditional panel to choose value based on operator
  output$value1 <- renderUI({
    conditionalPanel(
      condition = "input.operator1 != ''",
      numericInput(
        inputId = "value1",
        label = "Choose         value",
        value = get_default_value(input$criterion1, input$operator1)[1])
    )
  })
  
  # conditional panel to choose value for between operator
  output$value1b <- renderUI({
    conditionalPanel(
      condition = "input.operator1 == 'between'",
      numericInput(
        inputId = "value1b",
        label = "and",
        value = get_default_value(input$criterion1, input$operator1)[2])
    ) 
  }) 
  
  # conditional panel to answer yes/no questions in criterion field 
  output$yes_no_choice <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'Neutral stimuli included?' |  input.criterion1 == 'Existence of between-subject manipulation?' | input.criterion1 == 'Existence of within-subject manipulation (besides congruency)?'",
      selectInput(inputId = "yes_no",
                  label = " ",
                  choices =  c("", "Yes"=1, "No"=0)) 
    ) 
  }) 
  
  # conditional panel to choose task types
  output$choice_task_type <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'task_name'",
      checkboxGroupInput(inputId = "task_type",
                         label = "Choose task type:",
                         choices = 
                           c("Select",
                             "Stroop task" = "stroop",
                             "Simon task" = "simon",
                             "Flanker task" = "flanker",
                             "Other" = "other"))
    ) 
  }) 
  
  # conditional panel to choose publication code
  output$choice_pubcode <- renderUI({
    conditionalPanel(
      condition = "input.criterion1 == 'publication_code'",
      selectInput(inputId = "pub_code",
                  label = "Choose publiction code",
                  choices = c("", sort(publication_codes)))
    ) 
  }) 
  
  # create reactive list to store added filter arguments
  argument_list <- list(
    variable = c(),
    operator = c(),
    values = c()
  )
  rv <- reactiveValues(argument_list = argument_list)
  
  # specify action whenever "Add argument to list" is clicked
  observeEvent(input$action_add_arg, {
    # add current choices to argument data frame 
    if(input$operator1 != "" & input$operator1 != "between"){
      new_entry <- list(
        variable = input$criterion1, 
        operator = input$operator1,
        values = input$value1
      )
      
    } else if (input$operator1 == "between") {
      new_entry <- list(
        variable = input$criterion1, 
        operator = input$operator1,
        values = paste(input$value1, input$value1b, sep="; ") 
      )
      
      } else if(input$yes_no != ""){
        new_entry <- list(
          variable = input$criterion1, 
          operator = "equal",
          values = input$yes_no
        )
      
      } else if(!is.null(input$task_type)){
        new_entry <- list(
          variable = input$criterion1, 
          operator = "equal",
          values = paste(input$task_type, collapse = "; ")
        )
        
      } else if (!is.null(input$pub_code)){
        new_entry <- list(
          variable = input$criterion1, 
          operator = "equal",
          values = input$pub_code
        )
      
    }
    
    # add new argument to list
    rv$argument_list <- mapply(merge_lists, rv$argument_list, new_entry, SIMPLIFY = FALSE)
    
    # reset drop down menu for criterion choice
    updateSelectInput(session, 
                      inputId = "criterion1", 
                      selected = "")
    
    updateSelectInput(session, 
                      inputId = "operator1", 
                      selected = "")
    
    updateSelectInput(session, 
                      inputId = "task_type", 
                      selected = "")
    
    updateSelectInput(session, 
                      inputId = "pub_code", 
                      selected = "")
    
  })
  
 
  # conditional action button to delete last entry to argument list 
  output$conditional_action_remove <- renderUI({
    if(!is.null(rv$argument_list[[1]])){ # show action button only after first argument was added
      actionButton("action_remove_recent", "Remove recent argument")
    }
  }) 
  
  # conditional action button to reset list 
  output$conditional_action_reset <- renderUI({
    if(!is.null(rv$argument_list[[1]])){ # show action button only after first argument was added
      actionButton("action_reset_list", "Reset list",
                   style="color: #000000; border-color: #FF000")
    }
  }) 
  
  # remove last element argument_list when 'action_remove_recent' is clicked
  observeEvent(input$action_remove_recent, {
    rv$argument_list <- lapply(rv$argument_list, function(x) x[-length(x)])
  })
  
  # reset argument_list when 'action_reset_list' is clicked
  observeEvent(input$action_reset_list, {
    rv$argument_list <- lapply(rv$argument_list, function(x) c())
  })

  # add reactive dataframe to print argument_list as a table
  argument_df <- reactive({
    # Transform the list into a data frame
    if(length(rv$argument_list) > 0) {
      as.data.frame(do.call(cbind, rv$argument_list))
    } else {
      # Return an empty data frame if rv$argument_list is empty
      data.frame()
    }
  })
  output$summary <- renderTable({
    argument_df()
  })
  
  
  
  # MAIN PANEL ---------------------------------------
  
  # TAB 1
  # print data frame of suited data sets ----
  
  # connect to data base
  conn <- DBI::dbConnect(RSQLite::SQLite(), "acdc.db")
  
  suited_overview_df <- reactive({
    get_filtered_df(rv$argument_list, conn, type = "overview")
  })
  
  # print table
  output$suited_datasets <- renderDT(suited_overview_df(),
                                     rownames= FALSE)
  
  # TAB 2 
  # print dataframe of descriptives ----
  descriptives_df <- reactive({
    get_filtered_df(rv$argument_list, conn, type = "descriptives")
  })
  
  # print table
  output$descriptives <- renderDT(descriptives_df(),
                                  caption = htmltools::tags$caption(
                                    style = 'caption-side: bottom; text-align: center;',
                                    htmltools::em('Note:'), 
                                    'percentage congruent, mean reaction time and mean accuracy are calculated across all participants and conditions within this task.'),
                                  rownames= FALSE)
  
  # print histogram of filtered datasets 
  output$histogram <- renderPlot({
    req(descriptives_df())
    plot_dataset_histograms(descriptives_df())
  })
  
  
  # TAB 3 
  # TODO: Integrate?
  # get reactive list with detailed information about dfs 
  detailed_info<- reactive({
    get_filtered_df(rv$argument_list, conn, type = "detailed")
  })
  # plot detailed information about datasets 
  #output$rt_dist <- renderPlot({
  #  req(dataset_id_choice)
  #  plot_trial_rtdist(detailed_info(), dataset_id_choice, 2)
  #})
  
  
  # print R Code to access data ----
  R_code <- reactive({
    req(length(rv$argument_list[[1]]) > 0)
    cat(get_R_code(argument_list = rv$argument_list), sep=" ") # use cat for line breaks
    })
  
  output$Rcode <- renderPrint({
    R_code()
  })
  
  # logic behind download button
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('inhibition_task_data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(suited_df, filename) #TODO: decide which data users can download exactly & make reactive 
       }
     )
  
  # TAB 5 
  overview_df <- get_overview_df(conn)
  output$overview_datasets <- renderTable(overview_df,
                                       rownames= FALSE)
}



#
shinyApp(ui = ui, server = server)

