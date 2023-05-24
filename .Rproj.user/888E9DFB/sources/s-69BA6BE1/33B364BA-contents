ui <- fluidPage(

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

      tags$hr(),

      # Input: Select Variable 1
      actionButton(inputId = "variable_1",
                   label = "Variable 1",
                   style = "background-color: lightgreen"),
      bsTooltip(id = "variable_1",
                title = "Select Variable 1"),

      # Input: Select Variable 2
      actionButton(inputId = "variable_2",
                   label = "Variable 2",
                   style = "background-color: lightblue"),
      bsTooltip(id = "variable_2",
                title = "Select Variable 2"),
      tags$hr(),

      # Input: Type of t-test ----
      radioButtons(inputId = "type",
                label = "Analysis type",
                choices = list("One Sample" = 1,
                               "Dependent Samples" = 2,
                               "Independent Samples" = 3),
                selected = 3),

      # Input: Type of test ----
      radioButtons(inputId = "hypothesis",
                   label = "Hypothesis",
                   choices = list("Unequal" = 1, # placeholder, make it reactive
                                  ">" = 2,  # placeholder, make it reactive
                                  "<" = 3),  # placeholder, make it reactive
                   selected = 1),


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

                 tableOutput("contents"),

        ),

        # Tab 2: Results ----
        tabPanel("Results",

                 # Output: Bayes Factor  ----
                 textOutput(outputId = "BFText"),

                 # Output: Posterior Plot ----
                 plotOutput(outputId = "posteriorPlot")

        )
      )
    )
  )
)
