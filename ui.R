
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Two_Zero_Four_Eight"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      p("Use start button to START the game and use Up, Down, Right and Left buttons for moves"),
      actionButton("up", label = "Up",icon=icon("arrow-up",class="fa fa-arrow-up",lib = "font-awesome")),
      
      actionButton("down", label = "Down",icon=icon("arrow-down",class="fa fa-arrow-down",lib = "font-awesome")),
      
      actionButton("left", label = "Left",icon=icon("arrow-left",class="fa fa-arrow-left",lib = "font-awesome")),
      
      actionButton("right", label = "Right",icon=icon("arrow-right",class="fa fa-arrow-right",lib = "font-awesome")),
      
      actionButton("start", label = "START")
      
    ),
    
    mainPanel(
      tableOutput("table"),
      textOutput("text")
    )
  )
))
