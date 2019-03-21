source(file.path(".", "modules/Plots/moduleGroupMVPlots.R"),  local = TRUE)$value

callModule(missingValuesPlots, "MVPlots_DS", data = reactive({dataIn()}))

output$plotmvsmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_mv.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)




output$plotmvlarge <- renderUI({
  tagList(
    helpText("These barplots display the distribution of missing values in the dataset."),
    missingValuesPlotsUI(ns("MVPlots_DS"))
  )
})
