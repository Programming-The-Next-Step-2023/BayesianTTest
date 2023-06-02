#' @import BayesFactor
#' @import shiny
#' @import shinyjs
#' @import DT


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

                 DT::dataTableOutput("data_table")

        ),

        # Tab 2: Results ----
        tabPanel("Results",

                 # Output: Bayes Factor  ----
                 textOutput(outputId = "BF_text"),

                 # Output: Prior Plot ----
                 plotOutput(outputId = "prior_plot"),

                 # Output: Posterior Plot ----
                 plotOutput(outputId = "posterior_plot"),

                 # Output: BF Robustness Check Plot ----
                 plotOutput(outputId = "robustness_check_plot")

        ),

        # Tab 3: Bayes Factor Interpretation ----
        tabPanel("BF Interpretation",

                 datatable(
                   data.frame(matrix(
                     data = c(" > 100",
                              "30 - 100",
                              "10 - 30",
                              "3 - 10",
                              "1 - 3",
                              "1",
                              "1/3 - 1",
                              "1/10 - 1/3",
                              "1/30 - 1/10",
                              "1/100 - 1/30",
                              " < 1/100",
                              "Extreme evidence for the alternative",
                              "Very strong evidence for the alternative",
                              "Strong evidence for the alternative",
                              "Moderate evidence for the alternative",
                              "Anecdotal evidence for the alternative",
                              "No evidence",
                              "Anecdotal evidence for the null",
                              "Moderate evidence for the null",
                              "Strong evidence for the null",
                              "Very strong evidence for the null",
                              "Extreme evidence for the null"),

                     ncol = 2,
                     nrow = 11,
                     dimnames = list(NULL, c("Bayes Factor", "Interpretation"))
                   )),
                   options = list(
                     dom = "t",
                     ordering = FALSE,
                     paging = FALSE,
                     searching = FALSE),
                   selection = 'none',
                   callback = JS(
                     "$('table.dataTable.no-footer').css('border-bottom',
                     'none');"),
                   class = 'row-border',
                   escape = FALSE,
                   rownames = FALSE,
                   filter = "none",
                   width = 425),

                 helpText("The Bayes Factor can also be flipped by dividing
                          it by 1. If done so, values greater than one
                          represent evidence in favor of the null hytpothesis.")
        )
      )
    )
  ))


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


    # Sample from the posterior ----
    samples <- posterior(model = result[1],
                         iterations = 5000)

    # Extract Bayes factor and save it into an object ----
    BF <-
      paste0(
        "Bayes Factor in favor of the alternative is ",
        round(extractBF(result)[[1]][1], 3),
        ", with prior a Cauchy prior scale of ",
        rscale,
        ". ",
        "The 95% credible interval of the posterior is [",
        round(quantile(samples[, "delta"], probs = 0.025), 3),
        ", ",
        round(quantile(samples[, "delta"], probs = 0.975), 3),
        "]."
      )

    # Save posterior plot ----
    plot(samples[, "delta"],
         trace = FALSE,
         main = "Posterior Plot",
         bty = "n",
         show.obs = FALSE,
         xlab = "Effect Size",
         ylab = "Probability Density")
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

  # Render prior plot ----
  output$prior_plot <- renderPlot({

    # Plot prior based on type of hypothesis selected ----
    # Two-tailed test
    prior_plot <- if(input$hypothesis == 1) {
      x <- seq(-3, 3, by = 0.05)
      plot(x = x,
           y = dcauchy(x, scale = input$user_r),
           type = "l",
           main = "Prior Plot",
           bty = "n",
           xlab = "Effect Size",
           ylab = "Probability Density")
    }
    # Right-tailed test
    else if (input$hypothesis == 2) {
      x <- seq(0, 3, by = 0.05)
      plot(x = x,
           y = dcauchy(x, scale = input$user_r) * 2,
           type = "l",
           main = "Prior Plot",
           bty = "n",
           xlab = "Effect Size",
           ylab = "Probability Density")
    }
    # Left-tailed test
    else {
      x <- seq(-3, 0, by = 0.05)
      plot(x = x,
           y = dcauchy(x, scale = input$user_r) * 2,
           type = "l",
           main = "Prior Plot",
           bty = "n",
           xlab = "Effect Size",
           ylab = "Probability Density")
    }

    prior_plot

  })

  # Render BF robustness check plot ----
  output$robustness_check_plot <- renderPlot({

    model()$robustness_check_plot

  })

}


# Export App ----
#' This function runs the Bayesian t-test calculator
#'
#' @title Bayesian t-test calculator App
#' @author Roy Michael Moore, \email{roy.moore@@student.uva.nl}
#' @export

# Wrap the app as a function the user can call
ttest.calculator <- function() {

  shinyjs::useShinyjs() # Set up shinyjs

  shinyApp(ui = ui, server = server)

}

