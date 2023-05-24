#########################
## Main Data Witch App ##
#########################

# Load required packages and R files
library(shiny)
source("app_pivot_longer.R")
source("app_pivot_wider.R")

# UI 
ui <- navbarPage(title=div(img(src="data_witch_logo.png", height = 100),
                           tags$head(
                              tags$style(HTML(paste(".navbar-nav > li > a, .navbar-brand {
                                padding-top:0px !important; 
                                padding-bottom: 0 !important;
                                height: 80px;
                                font-size: 20px;
                                display: table-cell;
                                vertical-align: middle;
                              }
                              .navbar { 
                                min-height:25px !important;
                              }.navbar-nav li a:hover, .navbar-nav > .active > a {
                                color: #fff !important;
                                background-color:", lightblue, "!important;
                              }
                             ")))
                             )
                           ),
                 
                 tabPanel("Pivot Longer",
                            ui_pivot_longer() # imported from app_pivot_longer.R
                          ),
                 
                 tabPanel("Pivot Wider",
                            ui_pivot_wider() # imported from app_pivot_wider.R
                          )
                 )

# Server
server <- function(input, output, session) {
  
  # Pivot Longer App
  server_pivot_longer(input, output, session) # imported from app_pivot_longer.R
  
  # Pivot Wider App
  server_pivot_wider(input, output, session) # imported from app_pivot_wider.R
  
}

shinyApp(ui = ui, server = server)
