
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Jensen's method - advanced edition"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      HTML("<p>You may change the path loadings using this table. Note that variance cannot exceed 1. If you try, then the s column will display 'NaN'.</p>"),
      rHandsontableOutput("table"),
      HTML("<p>Because it takes a while to simulate the data based on the loading matrix, the server will only do this when you tell it to.</p>"),
      actionButton("update", "Update!")
      ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Bi-factor SEMplot",
          plotOutput("bi_factor")
        )
        
      )

    )
    
  )
  
))
