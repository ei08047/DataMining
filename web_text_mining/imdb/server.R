library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  
  
  # Return the requested title
  titleInput <- reactive({
      input$titleExp
  })
  
  numObs <- reactive({
    input$obs
  })
  
  idInput <- reactive({
    input$titleId
  })
  
  
  output$TitleMatchesTable <-renderTable({
    t <- getMoviesByTitle(titleInput())
    ids <- getMoviesIds(titleInput())
    x <- list(head(ids,input$obs),head(t,input$obs))
  },colnames=FALSE)
  
  output$MovieRating <- renderText({
    movie <- movieById(idInput())
    rating <- getRating(movie)
    rating <- paste("rating:", rating)
    rating
  })
  
  output$MovieCast <- renderTable({
    movie <- movieById(idInput())
    cast <- getCast(movie)
    x <- list(head(cast))
  },colnames=FALSE)
  
  output$MovieNumReviews <- renderText({
    movie <- movieById(idInput())
    numReviews <- getNumReviews(movie)
    numReviews <- paste("num Reviews", numReviews)
    numReviews
  })
  
  output$Poster <- renderImage({
    movie <- movieById(idInput())
    poster <- getPoster(movie)
    poster
  })
  
  output$Stars <- renderText({
    stars <- paste("missing", " stuff")
    stars
  })
  
})