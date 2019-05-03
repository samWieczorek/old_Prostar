source(file.path(".", "modules/Plots/moduleMSnSetExplorer.R"),  local = TRUE)$value


output$plotquantiTablesmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_quantiData.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)




callModule(module=MSnSetExplorer, 'msnsetExplorer',
           data = reactive({GetCurrentMSnSet()}))

output$plotquantiTablelarge <- renderUI({
  MSnSetExplorerUI(ns('msnsetExplorer'))
})
