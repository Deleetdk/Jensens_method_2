
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
      rHandsontableOutput("loadings_table"),
      HTML("<p>Because it takes a while to simulate the data based on the loading matrix, the server will only do this when you tell it to.</p>"),
      actionButton("update", "Update!")
      ),

    # Show a plot of the generated distribution
    mainPanel(
      HTML("<p>This is the advanced version of the Jensen's method visualization. This one is less explanatory and more exploratory. Basically it acts as an easy to use simulator for user-driven exploration. The user can enter the factor loadings in the table. The uniqueness (specificity; variance not explained by common factors) for each subtest will automatically be calculated in the last column (\"s\"). Multiple structural equation models are automatically fit to the data and displayed in the Structural plots tab.</p>"),
      tabsetPanel(
        tabPanel(
          "Structural plots",
          HTML("<p>Confirmatory factor analysis: bi-factor solution</p>"),
          plotOutput("SEM_bi"),
          HTML("<p>Confirmatory factor analysis: hierarchical solution</p>"),
          plotOutput("SEM_hier"),
          HTML("<p>Confirmatory factor analysis: single factor</p>"),
          plotOutput("SEM_single"),
          HTML("<p>Exploratory factor analysis: single factor</p>"),
          plotOutput("EFA_single")
        )
        
      )

    )
    
  )
  
))
