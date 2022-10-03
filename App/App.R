library(shiny)
library(shinyjs)
library(shinyalert)
library(DT) #https://rstudio.github.io/DT/shiny.html
library(tuneR)
library(rlist)

# include all functions
source("DatasetInput.R")
source("GenerateNoise.R")
source("GenerateNoiseForBalancing.R")
source("GenerateVariableNoise.R")
source("GenerateVariableNoiseForBalancing.R")
source("DatasetBalancing.R")
source("NoiseTable.R")
source("DataAugmentationWithColoredNoise.R")
source("AmountOfNewData.R")

###
# This function is the implementation of the shiny app, from where the data augmentation
# process is performed.
###


require("V8") ## req for shinyjs::extendShinyjs()

## JavaScript snippet that disables or enables the ability to click the tab
app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }"
## css snipit that makes it look like we aren't able click the tab
app_css <-
  ".nav li a.disabled {
    background-color: #aaa !important;
    color: #333 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  }"

## JavaScript snipet to color rows of the data table by index
fnc <- JS('function(row, data, index, rowId) {',
          'console.log(rowId)','if (rowId >= TWO) {row.style.color = "#1d9136";}','}')


if (interactive()) {
  
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = app_jscode, functions = c("disableTab", "enableTab")),
    shinyjs::inlineCSS(app_css),
    navbarPage("Data Augmentation with Colored Noise", id = "tabs",
                   
           ############ Tab 1 ###############
           
           tabPanel("Dataset",
                    
                    
                column(4,
                  fluidRow(
                    column(12,
                    wellPanel(
                    #checkboxInput("unselect_columns", "Unselect all columns", FALSE),
                    checkboxInput("set_seed", "Set seed", FALSE),
                    conditionalPanel(condition = "input.set_seed == 1", 
                                     numericInput("seed", "Seed", value = 100)),
                    fileInput("file1", h3("Choose File"),
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv",
                                ".txt",
                                ".data"))
                    )
                  )),
                  fluidRow(  
                    column(12,
                    conditionalPanel(condition = "output.fileUploaded", 
                                     wellPanel(
                                       h4("Select the columns in the datatable, 
                                          where noise should be added to."),
                                       checkboxInput("select_all_variables", "Select all variables", FALSE),
                                       actionButton("confirm", "Confirm", class = "btn-primary")
                                     )))),
                  fluidRow(  
                    column(12,
                           conditionalPanel(condition = "input.confirm", 
                                            wellPanel(
                                              selectInput("rounded_variables", "Which variables should be rounded?",
                                                          choices = list(), multiple = TRUE),
                                              selectInput("negative_values", "Which variables must not contain negative values?",
                                                          choices = list(), multiple = TRUE),
                                     

                                       actionButton("toTab2", "Next", class = "btn-primary")
                                      
                                     )
                  )))),

                  column(8,
                    #tableOutput("datatable"),
                    DTOutput("tbl")

                  )
           ),
           
           ############ Tab 2 ###############
           
           tabPanel("Noise",
                    column(4,
                      fluidRow(
                        column(12,
                          wellPanel(
                      
                            radioButtons("noise_color", h3("Select the color of the noise:"),
                                         choices = list("Violet" = -1.5, "Blue" = -1,
                                                        "White" = 0, "Pink" = 1, "Red" = 1.5), selected = 0)
                          ))),
                      fluidRow(
                        column(12,
                          wellPanel(
                            radioButtons("same_noise",
                                         p("Shall all variables be added with the same noise?"),
                                         choices = list("Yes" = "same",
                                                        "No" = "different"),
                                         selected = "same")
                            
                            ,
                            conditionalPanel(condition = "input.same_noise == 'different'",
                                             tags$b("Select the variables for each noise color."),
                                             tags$br(), tags$br(), 
                                             selectInput('violet_noise', 'Violet Noise',
                                                         choices = list(), multiple = TRUE),
                                             selectInput('blue_noise', 'Blue Noise',
                                                         choices = list(), multiple = TRUE),
                                             selectInput('white_noise', 'White Noise',
                                                         choices = list(), multiple = TRUE),
                                             selectInput('pink_noise', 'Pink Noise',
                                                         choices = list(), multiple = TRUE),
                                             selectInput('red_noise', 'Red Noise',
                                                         choices = list(), multiple = TRUE)
                                             
                            ),
                            actionButton("toTab3", "Next", class = "btn-primary")
                          )))
                      ),
                        
                      column(8,
                        plotOutput("noise_plot"),
                        verbatimTextOutput("validate"),
                        tags$head(tags$style("#validate{color: red;
                         font-size: 15px;
                         }"
                        )
                        )
                      )
                      
           ),
           
           ############ Tab 3 ###############
           
           tabPanel("Augmentation", id = "test",
                    
              column(4,
                fluidRow(
                  column(12,
                    wellPanel(
                        radioButtons("reg_clas", h3("Type of Problem"),
                                     choices = list("Regression Problem" = "regression", 
                                                    "Classification Problem" = "classification"))
                        ))),
                fluidRow(
                  column(12,
                      conditionalPanel(condition = "input.reg_clas == 'classification'",
                        wellPanel(
                         radioButtons("balancing",
                                      label = "Do you want your dataset to be balanced?",
                                      choices = list("Yes" = "balancing",
                                                     "No" = "no_balancing"),
                                      selected = "no_balancing")
                         )))),
                fluidRow(
                  column(12,
                     conditionalPanel(condition = "input.balancing == 'balancing'",
                        wellPanel(
                                      h4("Select the column with the dependent variable.")
                        )
                     ))),
                fluidRow(
                  column(12,
                    conditionalPanel(condition = "input.balancing == 'balancing'",
                      wellPanel(tags$b("Shall all classes be balanced?"),
                                 checkboxInput("all_classes",
                                               "All Classes", TRUE),
                                 
                                   checkboxGroupInput("balanced_classes",
                                                       p("Select the classes that should be balanced."),
                                                       c())
                      )))),
                fluidRow(
                  column(12,
                    conditionalPanel(condition = "input.balancing == 'balancing'",
                       wellPanel(radioButtons("balance_process",
                                              p("How do you want your data to be balanced?"),
                                              choices = list("Oversampling" = "oversampling",
                                                             "Balancing with noise" = "balancing_with_noise"))
                       )))),
                fluidRow(
                  column(12,
                    wellPanel(
                    radioButtons("new_datapoints",
                                 p("How many new datapoints do you want to generate based on your dataset?"),
                                 choices = list("2x the amount" = "double",
                                                "3x the amount" = "triple",
                                                "4x the amount" = "quadruple",
                                                "Individual amount" = "individual"),
                                 selected = "double"),
                    conditionalPanel(condition = "input.new_datapoints == 'individual'",
                                     numericInput("amount_new_datapoints", "Give the amount of new datapoints:", value = 100)),
                    tags$hr(),
                    downloadButton("download", label = "Download Data")
                    )))),

                
                
                column(8,
                  DTOutput("tbl_classification"),
                  plotOutput("classes_diagram")
                )
           ),
           
           ############ Tab 4 ###############
           tabPanel("Inspect",
                    fluidPage(
                      DTOutput("table")
                    )
                    # sidebarLayout(
                    #   sidebarPanel(
                    #     radioButtons("direction_of_generating",
                    #                  p("In which direction should the noise be added to your datatable?"),
                    #                  choices = list("Horziontal" = "horizontal",
                    #                                 "Vertical" = "vertical")
                    #     ),
                    #     radioButtons("replacement",
                    #                  p("How do you want your datapoints to be selected by random?"),
                    #                  choices = list("with replacement" = "replacement",
                    #                                 "without replacement" = "no_replacement"))
                    #   ),
                    #   mainPanel(
                    #     # Hier werde ich die ursprüngliche Datentabelle, die Noise-Daten
                    #     # und die neue Datentabelle anzeigen lassen
                    #     tableOutput("frequencies"),
                    #     tableOutput("table"),
                    #     textOutput("noise_array")
                    #     
                    #   )
                    # )
           )
    )
  )
  
  

  
  
  
  server <- function(input, output, session) {
    
    # navigation through tabs via "Next"-Buttons
    observeEvent(input$toTab2, {
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Noise")
    })
    
    observeEvent(input$toTab3, {
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Augmentation")
    })
    
    dataset_input <- reactive({
      if (is.null(input$file1)) return(NULL)
      DatasetInput(inFile = input$file1)
    })
    
    output$fileUploaded <- reactive({
      return(!is.null(dataset_input()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    # function to be able to select all variables of the datatable
    selected_variables <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
        if (input$select_all_variables == TRUE){
          dataset <- dataset_input()
          return(1:ncol(dataset))
        } else {
          return()
        }
    })
    
    # plot the table of the first tab
    output$tbl <- renderDT(
      dataset_input(), options = list(lengthChange = FALSE, scrollX = TRUE), 
      selection = list(target = 'column', selected = selected_variables())
    )
    
    
    selected_variables_classification <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
    })
    
    # plot the table of the third tab
    output$tbl_classification <- renderDT(
      dataset_input(), options = list(lengthChange = FALSE, scrollX = TRUE), 
      selection = list(target = 'column', selected = selected_variables_classification())
    )
    
    
    # update the choices for rounded variables and the variables without negative values
    observe({
      
      req(length(input$tbl_columns_selected) >= 1)
      dataset <- na.omit(dataset_input())
      
      if (max(input$tbl_columns_selected) <= ncol(dataset)) {
        selected_variables <- input$tbl_columns_selected
        
        # remove all non-numeric variables (for that we need the two auxiliary variables)
        selected_variables_copy <- selected_variables
        for (variable in selected_variables_copy) {
          if (!(is.numeric(dataset[,variable]))) {
            selected_variables <- selected_variables[-which(selected_variables == variable)]
          }
        }
        selected_variables <- sort(selected_variables)
        
        # get the column names of the choosen columns
        column_names <- names(dataset)[selected_variables]
        
        updateSelectInput(session, 'rounded_variables', 'Which variables should be rounded?',
                          choices = column_names)
        updateSelectInput(session, 'negative_values', 'Which variables must not contain negative values?',
                          choices = column_names)
      }
    })
    
    
    # generates noise by random but with a global seed variable
    random <- repeatable(sample)
    
    # generate noise for augmentation
    generate_noise <- reactive({
      if (input$balancing == "balancing") {
        balancing <- TRUE
      } else {
        balancing <- FALSE
      }
      
      if (input$set_seed == TRUE) {
        seed = input$seed
      } else {
        seed = random(1:10000,1)
      }
        noise_array <- GenerateNoise(seed, input$noise_color, new_datapoints = input$new_datapoints,
                                     tbl_columns_selected = input$tbl_columns_selected, 
                                     amount_new_datapoints = input$amount_new_datapoints,
                                     inFile = input$file1, balancing = FALSE, 
                                     balanced_classes = input$balanced_classes,
                                     tbl_classification_columns_selected = input$tbl_classification_columns_selected)
        
      return(noise_array)
    })
    
    
    output$noise_plot <- renderPlot({
      noise <- try(generate_noise())
     
      if ("try-error" %in% class(noise)) {
        noise <- noise(kind=("power"), alpha = as.numeric(input$noise_color))@left
      }
      
      if (length(input$tbl_columns_selected) == 0) {
        noise <- noise(kind=("power"), alpha = as.numeric(input$noise_color))@left
      }
      
      plot(noise[1:100], type="l")
    })
    
    
    # bar chart of the classes
    output$classes_diagram <- renderPlot({
      req(length(input$tbl_classification_columns_selected) == 1)
      
      dependent_variable <- input$tbl_classification_columns_selected
      dataset <- na.omit(dataset_input())
      all_classes <- sort(unique(dataset[,dependent_variable]))
      
      # calculate the frequencies of all classes
      frequencies_of_classes <- as.data.frame(table(dataset[,input$tbl_classification_columns_selected]))
      par(mar=c(8, 7.1, 4.1, 2.1))
      barplot(frequencies_of_classes$Freq, names.arg = frequencies_of_classes$Var1, horiz = TRUE, las = 1, col="#69b3a2")
    })
    
    
    # update checkboxInput in Tab 3 to select the classes that should be balanced
    observe({
      # only changes the output, if one and only one column is selected
      req(length(input$tbl_classification_columns_selected) == 1)
      
      dependent_variable <- input$tbl_classification_columns_selected
      dataset <- na.omit(dataset_input())
      if (max(input$tbl_classification_columns_selected) <= ncol(dataset)) {
        all_classes <- sort(unique(dataset[,dependent_variable]))
        most_frequent_class <- all_classes[which.max(tabulate(match(dataset[,dependent_variable],all_classes)))]
        
        # update groupCheckBox to select the classes that should be balnaced
        updateCheckboxGroupInput(session, "balanced_classes",
                                 label = "Select the classes that should be balanced.",
                                 choices = all_classes[all_classes != most_frequent_class],
                                 selected = if (input$all_classes == 1) {all_classes[all_classes != most_frequent_class]}
                                            else NULL
        )
      }
    })
    
    
    # if regression is selected, the balance checkbox has to be "No"
    observe({
      if (input$reg_clas == "regression") {
        updateRadioButtons(session, "balancing",
                           label = "Do you want your dataset to be balanced?",
                           choices = list("Yes" = "balancing",
                                          "No" = "no_balancing"),
                           selected = "no_balancing")
      }
    })
    
    
    # update the choices for the noise selection by the selected variables
    observe({
      
      req(length(input$tbl_columns_selected) >= 1)
      dataset <- na.omit(dataset_input())
      
      if (max(input$tbl_columns_selected) <= ncol(dataset)) {
        selected_variables <- input$tbl_columns_selected

        # remove all non-numeric variables (for that we need the two auxiliary variables)
        selected_variables_copy <- selected_variables
        for (variable in selected_variables_copy) {
          if (!(is.numeric(dataset[,variable]))) {
            selected_variables <- selected_variables[-which(selected_variables == variable)]
          }
        }
        selected_variables <- sort(selected_variables)

        # get the column names of the choosen columns
        column_names <- names(dataset)[selected_variables]
        
        updateSelectInput(session, 'violet_noise', 'Violet Noise',
                          choices = column_names)
        updateSelectInput(session, 'blue_noise', 'Blue Noise',
                          choices = column_names)
        updateSelectInput(session, 'white_noise', 'White Noise',
                          choices = column_names)
        updateSelectInput(session, 'pink_noise', 'Pink Noise',
                          choices = column_names)
        updateSelectInput(session, 'red_noise', 'Red Noise',
                          choices = column_names)
      }
    })
    
 
    # function to generate the validation message from Tab 2
    validate_message <- function() {
      req(length(input$tbl_columns_selected) >= 1)
      req(input$same_noise == "different")
      dataset <- na.omit(dataset_input())
      text_output <- ""
      if (max(input$tbl_columns_selected) <= ncol(dataset)) {
        selected_columns <- input$tbl_columns_selected

        # remove all non-numeric variables (for that we need the two auxiliary variables)
        selected_columns_copy <- selected_columns
        for (column in selected_columns_copy) {
          if (!(is.numeric(dataset[,column]))) {
            selected_columns <- selected_columns[-which(selected_columns == column)]
          }
        }
        selected_columns <- sort(selected_columns)
        
        # get the column names of the choosen columns
        all_variables <- names(dataset)[selected_columns]
        selected_variables <- c(input$violet_noise, input$blue_noise, input$white_noise, input$pink_noise, input$red_noise)
        
        check_one <- TRUE
        
        if (length(setdiff(all_variables, selected_variables)) != 0){
          text_output <- "Please assign the following variables to a noise."
          for (index in 1:length(setdiff(all_variables, selected_variables))) {
            text_output <- paste(text_output, paste("- ", setdiff(all_variables, selected_variables)[index]), sep = "\n")
          }
          check_one <- FALSE
        }
        
        # if a variable is assigned to more than one noise -> warning message
        duplicates <- unique(selected_variables[duplicated(selected_variables)])
        if (length(unique(selected_variables)) != length(selected_variables)) {
          if (check_one) {
            text_output <- "Please assign the following variables only to one noise."
            for (index in 1:length(duplicates)) {
              text_output <- paste(text_output, paste("- ", duplicates[index]), sep = "\n")
            }
          } else {
            text_output <- paste(text_output, "Please assign the following variables only to one noise.", sep = "\n")
            for (index in 1:length(duplicates)) {
              text_output <- paste(text_output, paste("- ", duplicates[index]), sep = "\n")
            }
          }
          check_one <- FALSE
        }
        
        if (check_one) text_output <- ""
      }
      
      return(text_output)
    }
    
    
    # error message for the selection of the noises to the variables
    output$validate <- renderText({
      req(input$same_noise == "different")
      if (length(input$tbl_columns_selected) == 0) {
        "No columns selected in 'Dataset'."
      } else {
        validate_message()
      }
    })
    
    
    # disable Tab 3, if there is a validate message
    observe({
      req(length(input$tbl_columns_selected) >= 1)
      req(input$same_noise == "different")
      if (nchar(validate_message()) != 0) {
        shinyjs::js$disableTab("Augmentation")
        shinyjs::disable("toTab3")
      } else {
        shinyjs::js$enableTab("Augmentation")
        shinyjs::enable("toTab3")
      }
    })
    
    
    # enable Tab 3, if same_noise is selected
    observe({
      req(length(input$tbl_columns_selected) >= 1)
      if (input$same_noise == "same") {
        shinyjs::js$enableTab("Augmentation")
        shinyjs::enable("toTab3")
      }
    })
    
    
    # disable Tab 3, if no column in Tab 1 is selected
    observe({
      req(input$file1)
      if (length(input$tbl_columns_selected) == 0) {
        shinyjs::js$disableTab("Augmentation")
        shinyjs::disable("toTab3")
      } else {
        shinyjs::js$enableTab("Augmentation")
        shinyjs::enable("toTab3")
      }
    })
    
    
    # download the data file
    output$download <- downloadHandler(
      filename = function() {
        filename_ending <- tail(strsplit(input$file1$name, ".", fixed = TRUE)[[1]], n=1)
        new_filename <- paste(substr(input$file1$name, 1, nchar(input$file1$name) - nchar(filename_ending) - 1), "_augmented.", sep = "")
        paste(new_filename, filename_ending, sep = "")
       },
       content = function(con) {
         write.csv(generate_new_datatable(), con, row.names = FALSE)
       }
    )
    

    # concatenate the original datatable and the noise datatable
    generate_new_datatable <- reactive({
      req(length(input$tbl_columns_selected) >= 1)
      
      if (input$set_seed == TRUE) {
        seed = input$seed
      } else {
        seed <- random(70100:1400000,1)
      }
      
      if (input$balancing == "balancing") {
        balancing <- TRUE
      } else {
        balancing <- FALSE
      }
      
      if (input$same_noise == "same"){
        same_noise = TRUE
      } else {
        same_noise = FALSE
      }

      table <- DataAugmentationWithColoredNoise(same_noise = same_noise, tbl_columns_selected = input$tbl_columns_selected, 
                                inFile = input$file1,
                                tbl_classification_columns_selected = input$tbl_classification_columns_selected, 
                                balanced_classes = input$balanced_classes, balance_process = input$balance_process,
                                direction_of_generating = "vertical", 
                                seed = seed, noise_color = input$noise_color,
                                violet_noise = input$violet_noise, blue_noise = input$blue_noise,
                                white_noise = input$white_noise, pink_noise = input$pink_noise,
                                red_noise = input$red_noise, new_datapoints = input$new_datapoints, 
                                amount_new_datapoints = input$amount_new_datapoints, balancing = balancing,
                                replacement = "no_replacement", rounded_variables = input$rounded_variables,
                                negative_values = input$negative_values)
      
      return(table)
    })
    
    
    # colors the rows of the data table depending on their index
    coloring <- reactive({
      req(length(input$tbl_columns_selected) >= 1)
      dataset <- na.omit(dataset_input())
      fnc <- sub("TWO", nrow(dataset), fnc)
      fnc
    })
    
    
    output$table <- renderDataTable(
      DT::datatable(generate_new_datatable(),
      options = list(pageLength = amount_of_all_data(), rowCallback = coloring(), 
                     scrollX = TRUE))
    )
    
    
    # this function gives the amount of all datapoints, that are in the resulting dataset
    amount_of_all_data <- reactive({
      req(length(input$tbl_columns_selected) >= 1)
      dataset <- na.omit(dataset_input())
      if (input$new_datapoints == "double") {
        return(2*(nrow(dataset) + amount_of_balanced_data()))
      } else if (input$new_datapoints == "triple") {
        return(3*(nrow(dataset) + amount_of_balanced_data()))
      } else if (input$new_datapoints == "quadruple") {
        return(4*(nrow(dataset) + amount_of_balanced_data()))
      } else if (input$new_datapoints == "individual") {
        return(nrow(dataset) + amount_of_balanced_data() + input$amount_new_datapoints)
      }
    })
    
    
    amount_of_balanced_data <- reactive({
      if(input$balancing != "balancing" || input$reg_clas == "regression") {
        return(0)
      }
      dataset <- na.omit(dataset_input())
      
      all_classes <- sort(unique(dataset[,input$tbl_classification_columns_selected]))
      most_frequent_class <- all_classes[which.max(tabulate(match(dataset[,input$tbl_classification_columns_selected],all_classes)))]
      
      # calculate the frequencies of all classes
      frequencies_of_classes <- as.data.frame(table(dataset[,input$tbl_classification_columns_selected]))

      # calculate the amount of the most frequent class
      amount_of_most_frequent_class <- frequencies_of_classes[frequencies_of_classes$Var1 == most_frequent_class,"Freq"]
      
      # choose only the selected classes in the checkbox input
      frequencies_of_classes <- frequencies_of_classes[frequencies_of_classes$Var1 %in% input$balanced_classes, ]
      
      # calculate the amount of new data that is needed by each selected class in new_data_of_classes
      new_data_of_classes <- frequencies_of_classes
      
      #new_data_of_classes$Var1 <- lapply(new_data_of_classes$Var1, as.numeric)
      for (row in 1:nrow(new_data_of_classes)){
        new_data_of_classes$Freq[row] = amount_of_most_frequent_class - new_data_of_classes$Freq[row]
      }
      
      # sum the amount of new data that is needed to balance the dataset
      amount_new_data <- sum(new_data_of_classes$Freq)
      return(amount_new_data)
    })
    
  }
  
  shinyApp(ui, server)
}
