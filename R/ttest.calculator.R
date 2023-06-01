#'
#'
#' @import BayesFactor
#' @import shiny
#' @import shinyjs
#' @import DT


library(shiny)
library(shinyjs) # Enabling or disabling input fields
library(DT) # Better data tables
library(BayesFactor)

ui <- fluidPage(

  shinyjs::useShinyjs(), # Enabling or disabling input fields

  # App title ----
  titlePanel("Bayesian t-test Calculator"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: File ----
      fileInput(inputId = "file",
                label = "Upload CSV File",
                accept = ".csv"),

      # Input: Add Header Row ----
      checkboxInput(inputId = "header",
                    label = "Header Row?",
                    value = TRUE),

      tags$hr(), # Draw line

      # Input: Select Variable 1
      selectInput(inputId = "variable_1",
                  label = "Variable 1",
                  choices = NULL),

      # Input: Select Variable 2
      selectInput(inputId = "variable_2",
                  label = "Variable 2 (Two-sample Designs Only)",
                  choices = NULL),

      tags$hr(), # Draw line

      # Input: Type of t-test ----
      radioButtons(inputId = "type",
                   label = "Analysis type",
                   choices = list("One Sample" = 1,
                                  "Dependent Samples" = 2,
                                  "Independent Samples" = 3),
                   selected = 3),

      # Input: Type of hypothesis test ----
      radioButtons(inputId = "hypothesis",
                   label = "Hypothesis",
                   choices = list("Choose type of analysis above" = 1,
                                  "Choose type of analysis above" = 2,
                                  "Choose type of analysis above" = 3),
                   selected = 1),

      # Input: Prior Scale Parameter (r) ----
      numericInput(inputId = "test_value",
                   label = "Test Value (One-Sample Design Only)",
                   value = 0),

      # Input: Prior Scale Parameter (r) ----
      numericInput(inputId = "user_r",
                   label = "Cauchy Scale Parameter",
                   value = 0.707),

      # Input: BF Robustness Check (Yes/No) ----
      checkboxInput(inputId = "robustness",
                    label = "Bayes Factor Robustness Check")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Divide main panel into tabs ----
      tabsetPanel(

        # Tab 1: Data Frame Visualization ----
        tabPanel("Data",

                 DT::dataTableOutput("data_table"),

        ),

        # Tab 2: Results ----
        tabPanel("Results",

                 # Output: Bayes Factor  ----
                 textOutput(outputId = "BF_text"),

                 # Output: Posterior Plot ----
                 plotOutput(outputId = "posterior_plot"),

                 # Output: BF Robustness Check Plot ----
                 plotOutput(outputId = "robustness_check_plot")

        )
      )
    )
  )
)

server <- function(input, output) {

  # Load File ----
  df <- reactive({

    # Validate File ----
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)

    ext <- tools::file_ext(inFile$datapath)
    req(inFile)
    validate(need(ext == "csv", "Please upload a csv file"))

    # Read file ----
    read.csv(inFile$datapath, header = input$header)

  })

 # Render Data Table
  output$data_table <- DT::renderDataTable({

    if (is.null(df())) {
      return(NULL)
    }

    df()

  })

  # Update UI based on analysis type to correctly name hypothesis ----
  observeEvent(input$type, {

    # One Sample ----
    if(input$type == 1) {
      updateRadioButtons(inputId = "hypothesis",
                         choices = hypothesis_choices[1:3])
    }
    # Dependent Samples ----
    else if (input$type == 2) {
      updateRadioButtons(inputId = "hypothesis",
                         choices = hypothesis_choices[4:6])
    }
    # Independent Samples ----
    else {
      updateRadioButtons(inputId = "hypothesis",
                         choices = hypothesis_choices[7:9])
    }
  })

  # Update UI to include variable names of introduced data: Variable 1 ----
  observeEvent(list(input$file, input$header), {

    updateSelectInput(inputId = "variable_1",
                      choices = names(df()))

  })

  # Update UI to include variable names of introduced data: Variable 2 ----
  observeEvent(list(input$file, input$header), {

    updateSelectInput(inputId = "variable_2",
                      choices = names(df()))

  })

  # Enable and disable test value and variable 2 fields based on test ----
  observeEvent(input$type, {
    if (input$type != 1) {
      shinyjs::disable("test_value")
      shinyjs::enable("variable_2")
    } else {
      shinyjs::enable("test_value")
      shinyjs::disable("variable_2")
    }

  })

  # Main results reactive expression ----

  model <- reactive({

    # Defining selected columns and test value (one-sample design) ----
    if (input$type == 1) {
      x <- df()[input$variable_1]
      y <- NULL
      mu <- input$test_value
    } else {
      x <- df()[input$variable_1]
      y <- df()[input$variable_2]
      mu <- 0
    }

    # Define nullInterval based on type of hypothesis selected ----
    nullInterval <- if(input$hypothesis == 2) {
      c(0, Inf)
    } else if (input$hypothesis == 3) {
      c(-Inf, 0)
    } else {
      NULL
    }

    # Assign the selected test and scale parameter for the Cauchy prior ----
    paired <- if(input$type == 2) {TRUE} else {FALSE}
    rscale <- input$user_r

    # Analyze data to extract Bayes Factor ----
    result <- ttestBF(x = unlist(x),
                      y = unlist(y),
                      mu = mu,
                      nullInterval = nullInterval,
                      paired = paired,
                      rscale = rscale)

    # Extract Bayes factor and save it into an object ----
    BF <- paste0("Bayes Factor is ", round(extractBF(result)[[1]][1], 3),
           " with prior a Cauchy prior scale of ", rscale, ".")

    # Sample from the posterior ----
    samples <- posterior(model = result[1],
                         iterations = 5000)

    # Save posterior plot ----
    plot(samples[, "delta"],
         trace = FALSE,
         main = "")
    posterior_plot <- recordPlot()


    # BF robustness check ----
    robustness_check_plot <-
    if (input$robustness) {
      robustness.check(x = unlist(x),
                       y = unlist(y),
                       mu = mu,
                       nullInterval = nullInterval,
                       paired = paired)
      recordPlot()
    } else {
      NULL
    }


    # Save all objects in the model() reactive ----
    list(BF = BF,
         posterior_plot = posterior_plot,
         robustness_check_plot = robustness_check_plot)


  })

  # Render Bayes factor and prior scale text ----
  output$BF_text <- renderText({

    model()$BF

  })

  # Render posterior plot ----
  output$posterior_plot <- renderPlot({

    model()$posterior_plot

  })

  # Render BF robustness check plot ----
  output$robustness_check_plot <- renderPlot({

    model()$robustness_check_plot

  })

}

# Utils

hypothesis_choices <- list("1" = 1, "2" = 2, "3" = 3,
                           "4" = 1, "5" = 2, "6" = 3,
                           "7" = 1, "8" = 2, "9" = 3)

names(hypothesis_choices) <- c(paste("\U2260", "test value"),
                               "> test value",
                               "< test value",
                               paste("measure 1", "\U2260", "measure 2"),
                               paste("measure 1", ">", "measure 2"),
                               paste("measure 1", "<", "measure 2"),
                               paste("group 1", "\U2260", "group 2"),
                               paste("group 1", ">", "group 2"),
                               paste("group 1", "<", "group 2"))

#' This function plots the change in the Bayes Factor under different possible priors
#'
#' @title Bayesian t-test calculator App
#' @author Roy Michael Moore, \email{roy.moore@@student.uva.nl}
#' @export

ttest.calculator <- function() {
  shinyApp(ui = ui, server = server)
}

