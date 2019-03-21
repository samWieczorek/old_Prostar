#source(file.path(".", "modules/Plots/modulePCAPlots.R"),  local = TRUE)$value

output$plotpcasmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_pca.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)



callModule(module=pcaPlots, 'pcaPlots', data=reactive({dataIn()}))

output$plotpcalarge <- renderUI({
 # modulepcaPlotsUI('pcaPlots')
})