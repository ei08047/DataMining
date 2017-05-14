library(shiny)

# Define UI for application that plots random distributions 
shinyUI(
  navbarPage("My Application",
             tabPanel(
               "task 1",
               pageWithSidebar(
                 headerPanel("find movie by title"),
                 # Sidebar with a slider input for number of observations
                 sidebarPanel(
                   textInput("titleExp", "Title", "", NULL, "insert title here"),
                   numericInput("obs", "Number of observations to view:", 10)
                 ),
                 mainPanel(
                   tableOutput("TitleMatchesTable")
                 )
               )
             ),
             tabPanel(
               "task 2",
             pageWithSidebar(
               headerPanel("find movie Info by id"),
               sidebarPanel(
                 textInput("titleId", "TitleId", "", NULL, "insert title Id")
               ),
               mainPanel(
                 textOutput("MovieRating"),
                 textOutput("MovieNumReviews"),
                 tableOutput("MovieCast")
                 #imageOutput("Poster")
               )
             )
             ),
             tabPanel("task 3",
                pageWithSidebar(
                  headerPanel("summary"),
                  sidebarPanel(
                    numericInput("numStars", "number of stars to view", 5)
                  ),
                  mainPanel(
                    textOutput("Stars")
                  )
                )
                      
                      
                      )
  )
)
