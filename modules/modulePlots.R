

modulePlots <- function(input, output, session, dataIn, llPlots){
  ns <- session$ns
  
  .width <- 50
  .height <- 50
  
  
  output$vignettes <- renderUI({
    
    ll <- list(NULL)
    for (i in 1:length(llPlots())) {
      n <- llPlots()[i]
      ll[[i]] <- tags$div( style="display:inline-block;",
                         imageOutput(ns(paste0("plot",n,"small")), height='60', width='50'))
    }
    
    offset <- 6
    for (i in 1:length(llPlots())){
      n <- llPlots()[i]
      ll[[i+offset]] <- shinyBS::bsModal(paste0("modalExample",n), "Your plot", ns(paste0("plot",n,"small")), size = "large",plotOutput(ns(paste0("plot",n,"large"))))
  }
    
   
    ll
  })
  
  
  data1 <- reactive({hist(rnorm(8), main = "Data 1")})
  
  data2 <- reactive({hist(rnorm(80), main = "Data 2")})
  data3 <- reactive({hist(rnorm(100), main = "Data 3")})
  
 
  output$plot3small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','desc_boxplot.png'))
  
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)


  output$plot2small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  output$plot1small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','filter1.png'))
    
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  output$plot4small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','desc_violinplot.png'))
    
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  output$plot5small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','desc_density.png'))
    
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  output$plot6small <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images','desc_heatmap.png'))
    
    # Return a list containing the filename
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)

  
  output$plot2large <- renderPlot({
    boxplot(iris, main = "Data 2")
  })

  output$plot1large <- renderPlot({
    hist(rnorm(8), main = "Data 1")
   })
  
  
  output$plot3large <- renderPlot({
    hist(rnorm(100), main = "Data 3")
  })
  
  
  output$plot4large <- renderPlot({
    boxplot(iris, main = "Plot 4 large")
  })
  
  output$plot5large <- renderPlot({
    hist(rnorm(8), main = "Plot 5 large")
  })
  
  
  output$plot6large <- renderPlot({
    hist(rnorm(100), main = "Plot 6 large")
  })
  
  return(NULL)
}