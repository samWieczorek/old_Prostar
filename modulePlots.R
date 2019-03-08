

modulePlots <- function(input, output, session, dataIn){
  ns <- session$ns
  
 
  
  data1 <- reactive({hist(rnorm(8), main = "Data 1")})
  
  data2 <- reactive({hist(rnorm(80), main = "Data 2")})
  data3 <- reactive({hist(rnorm(100), main = "Data 3")})
  
  
  output$plot3small <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    hist(rnorm(100), main = "Data 3")
    # Generate the PNG
    png(outfile, width = 200, height = 150)
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 150,
         height = 80,
         alt = "This is alternate text")
  }, deleteFile = TRUE)


  
  output$plot2small <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 200, height = 150)
    boxplot(iris, main = "Data 2")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 150,
         height = 80,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$plot1small <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 200, height = 150)
    hist(rnorm(8), main = "Data 1")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 150,
         height = 80,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  

  
  output$plot2large <- renderPlot({
    boxplot(iris, main = "Data 2")
  })

  output$plot1large <- renderPlot({
    hist(rnorm(8), main = "Data 1")
    
  })
  
  
  output$plot3large <- renderPlot({
    hist(rnorm(100), main = "Data 3")

  })
  
  
  return(NULL)
}