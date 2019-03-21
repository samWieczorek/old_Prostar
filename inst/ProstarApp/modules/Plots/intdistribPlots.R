output$plotintensitysmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)



output$plotintensitylarge <- renderPlot({
  hist(rnorm(8), main = "Data 1")
})
