output$plotvarDistsmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_varDist.jpg'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)





output$plotvarDistlarge <- renderPlot({
  hist(rnorm(100), main = "Plot 6 large")
})
