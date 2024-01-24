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
library(htmltools)
source("./shiny/helper_file_shiny.R")
source("./shiny/ui.R")

files.sources = list.files("./functions", pattern = "\\.R$", full.names = TRUE, include.dirs = FALSE)
sapply(files.sources, source)



server <- function(input, output, session){
  
  # FOR INTRO ---
  
  # print short intro text
  output$short_intro <- renderUI({
    HTML("This shiny app provides an overview over datasets in the Attentional Control Data Collection (ACDC) data base . <br> <br>
         You can filter datasets by criteria such as task type and sample size using the sidebar labeled 'filter datasets'. <br>
         The main panel provides: <ul>
                     <li>An overview of those datasets fulfilling your filter criteria [TAB 1]</li> 
                     <li>A table showing descriptive attributes of these datasets (such as mean accuracy, number of trails etc.) [TAB 2]</li> 
                     <li>A vizualization of accuracy and reaction time (rt) per dataset and rt distributions [TAB 3]</li>
                     <li>Instructions on how to access these datasets using our  R package acdcquery and an option to download trial level data as a csv [TAB 4]</li>
                     <li>and an overview of all data sets in ACDC [TAB 5]</li>
                     ")
  })
  
  # print explanation of project 
  output$explanation_db <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_explain_db %% 2 == 1){
      updateActionButton(inputId = "action_explain_db", label = "Got it!")
      renderUI({HTML("The ACDC data base contains attentional control task data (i.e., Stroop, flanker or Simon task) from over 40 datasets as well as information about the respective studies and publications. <br>
                     It is meant to enhance access to open attentional control task data. <br>
                     Data can be accessed either via SQL or our CRAN R package <a href=https://cran.r-project.org/web/packages/acdcquery/index.html target='_blank' rel='noopener noreferrer'>acdcquery</a>. <br> <br>
                     <img src='https://raw.githubusercontent.com/jstbcs/acdc-database/7adc1609a7743803a4d760ef5cd11ff426db2794/shiny/www/db_structure.png' alt='Structure of inhibition task db' width='908' height='630'>"
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
                     <li>A congruency variable, indicating if stimuli were congruent (coded as 1), conflincting (2) or neutral (3)</li> 
                     <li>Reaction time of each trial, in seconds </li>
                     <li>Accuracy of each trial (correct coded as 1/ incorrect coded as 0)</li>
                     <li>If applicable: A between variable indicating between subject manipulation</li>
                     <li>If applicable: A within variable indicating within subject manipulation</li>
                     <br> 
                     You can submit your data via <a href=https://www.ampl-psych.com/attentional_control_data_collection/ target='_blank' rel='noopener noreferrer'>this online form</a>. <br>
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
      condition = "input.criterion1 != ' ' & input.criterion1 != 'task_name' & input.criterion1 != 'publication_code' & input.criterion1 != 'neutral_trials' &  input.criterion1 != 'Existence of between-subject manipulation?' & input.criterion1 != 'Existence of within-subject manipulation (besides congruency)?'",
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
      condition = "input.criterion1 == 'neutral_trials'",
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
                           c(#"",
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
                  choices = c("", sort(get_pub_code())))
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
    # only execute when user made a choice
    if(input$criterion1 != ' '){
      
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
      
    } else { # if nothing chosen yet
      
    }
    
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
    # after one value has been chosen 
    if(input$criterion1 != ' '){
      as.data.frame(do.call(cbind, rv$argument_list))
    # Transform the list into a data frame
    #if(length(rv$argument_list) > 0) {
     # as.data.frame(do.call(cbind, rv$argument_list))
    } else {
      # Return an empty data frame if rv$argument_list is empty
      data.frame()
    }
  })
  output$summary <- renderTable({
    argument_df()
  })
  
  
  
  # MAIN PANEL ---------------------------------------
  
  # TAB 1 ---------#
  # connect to data base
  conn <- DBI::dbConnect(RSQLite::SQLite(), "acdc.db")
  
  suited_overview_df <- reactive({
    get_filtered_df(rv$argument_list, conn, type = "overview")
  })
  
  # query data base (needed to print details on number of hits)
  # note: later used for download button
  data_for_download <- reactive({
    download_data(suited_overview_df()[['Dataset ID']], conn)
  })
  
  
  # return number of hits 
  n_hits <- reactive({
    req(length(rv$argument_list[[1]]) > 0)
    # print number of hits, subjects, and trials 
    if(nrow(suited_overview_df() != 0)) {
      subjects <- sum(suited_overview_df()$`Sample Size`)    #length(unique(data_for_download()$subject))
      all_trials <- sum(suited_overview_df()$`Sample Size` * suited_overview_df()$`Trials per block` * suited_overview_df()$`Blocks per participant`)  #nrow(data_for_download())
      paste(nrow(suited_overview_df()),"datasets in ACDC match your filter criteria, containing",
            all_trials, "trials overall from", subjects, "subjects.")
    } else {
      # if no hits 
      paste("There are no datasets that match these criteria. Please reset list.")
    }
    })
  output$number_hits <- renderText(n_hits())
  
  # print table
  output$suited_datasets <- renderDT(suited_overview_df(),
                                     rownames= FALSE)
  
  # TAB 2 --------#
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
 
  # TAB 3  --------#
  # get reactive list with detailed information about dfs 
  detailed_info <- reactive({
    get_filtered_df(rv$argument_list, conn, type = "detailed")
  })
  
  # print histogram of filtered datasets 
    output$histogram <- renderPlot({
    req(length(rv$argument_list[[1]]) > 0)
      acc_plot <- descriptives_df() |> 
    dplyr::mutate(dataset_id = factor(`Dataset ID`)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, .data[[input$sort_x_axis]], .desc = TRUE),
        y = `Mean accuracy (dataset)`
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::labs(
      title = "Mean Accuracy",
      x = "dataset_id",
      caption = "Values are based on the 'mean_dataset_acc' column."
    )+
    ggplot2::theme_classic()
  
  rt_plot <- descriptives_df() |> 
    dplyr::mutate(dataset_id = factor(`Dataset ID`)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = forcats::fct_reorder(dataset_id, .data[[input$sort_x_axis]], .desc = TRUE),
        y = `Mean reaction time (dataset)`
      )
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::labs(
      title = "Mean Reaction Time (in s)",
      x = "dataset_id",
      caption = "Values are based on the 'mean_dataset_rt' column."
    )+
    ggplot2::theme_classic()
  
  ggpubr::ggarrange(acc_plot, rt_plot, ncol = 1, nrow = 2)
      
  })
  
  #  for choice of datasetID for rt plot: only show IDs that match criteria
  observe({
    # Assuming the dataset IDs are in a column named 'id'
    dataset_ids <- suited_overview_df()[['Dataset ID']]
    
    # Update the selectInput choices
    updateSelectInput(session, "choose_dataset_id", choices = dataset_ids)
  })
  
  # plot detailed information about datasets 
  output$rt_dist <- renderPlot({
    req(input$choose_dataset_id)
    plot_trial_rtdist(detailed_info(), input$choose_dataset_id, 2)
  })
  
  # TAB 4 -------------#
  # print R Code to access data 
  R_code <- reactive({
    req(length(rv$argument_list[[1]]) > 0)
    cat(get_R_code(argument_list = rv$argument_list), sep=" ") # use cat for line breaks
    })
  
  output$Rcode <- renderPrint({
    R_code()
  })
  
  
  # logic behind download button 
  # get filtered data for download
  #data_for_download <- reactive({
   # download_data(suited_overview_df()[['Dataset ID']], conn)
  #})
  
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('attentional_control_task_data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         # Use 'con' as the file connection to write the CSV
         write.csv(data_for_download(), con, row.names = FALSE)
       }
     )
  
  # TAB 5 ------#
  overview_df <- get_overview_df(conn)
  #output$overview_datasets <- renderTable(overview_df,
   #                                  rownames= FALSE)
  
 
  output$overview_datasets <- renderDataTable({
    datatable(overview_df,
              rownames = FALSE,
              options = list(
                autoWidth = FALSE,
                columnDefs = list(
                  # "Read more button)
                  list(
                    targets = 7, 
                    createdCell = JS(js)  # use JS code (specificed in helper file)
                  ),
                  # column width
                  list(targets = 1, width = '5px'), # narrower 2nd column
                  list(targets = 7, width = '700px') # wider 8th column
                )
              ))
  })
}




shinyApp(ui = ui, server = server)

