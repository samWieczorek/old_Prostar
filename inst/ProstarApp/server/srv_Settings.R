callModule(modulePopover,"modulePopover_numPrecision", data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">Numerical precisions</font></strong>")),
                                                                        content= "Set the number of decimals to display for numerical values.")))

observe({
  shinyjs::toggle("defineColorsUI", condition=!is.null(rv$current.obj))
})


output$settings_nDigits_UI <- renderUI({
  numericInput("settings_nDigits", "", value=rv$settings_nDigits, min=0, width="100px")
})

observeEvent(input$settings_nDigits,{ rv$settings_nDigits <-input$settings_nDigits })


observe({
  shinyjs::onclick("btn_configConditionsColors",{
    shinyjs::toggle(id = "defineColorsForConditionsUI", anim = TRUE)}
  )
  
  shinyjs::onclick("btn_configMVColors",{
    shinyjs::toggle(id = "defineColorsForMVUI", anim = TRUE)}
  )
  
  
  shinyjs::onclick("btn_configVolcanoColors",{
    shinyjs::toggle(id = "defineColorsForVolcanoUI", anim = TRUE)}
  )
  
  
  shinyjs::onclick("btn_configFCColors",{
    shinyjs::toggle(id = "defineColorsForFCUI", anim = TRUE)}
  )
})



##########
output$defineColorsUI <- renderUI({
  
  
  bsCollapse(id = "collapseExample", open = "",
             bsCollapsePanel("Colors for conditions", uiOutput("defineColorsForConditionsUI"), style = "primary"),
             bsCollapsePanel("Colors for missing values", tagList(
               colourpicker::colourInput("colMEC", "Select colour for MEC", "orange",showColour = "background"),
               colourpicker::colourInput("colPOV", "Select colour for POV", "lightblue", showColour = "background")
             ), style = "primary"),
             bsCollapsePanel("Colors for volcanoplots", colourpicker::colourInput("colVolcanoIn", "Select colour for selected entities", "orange",showColour = "background"),
                             colourpicker::colourInput("colVolcanoOut", "Select colour for filtered out entities", "gray", showColour = "background"), style = "primary"),
             bsCollapsePanel("logFC distribution", "todo", style = "primary")
             
  )


})




output$defineColorsForConditionsUI <- renderUI({
  
  tagList(
    fluidRow(
    column(width=3,radioButtons("typeOfPalette", "Type of palette for conditions",
                                choices=c("predefined"="predefined", "custom"="custom"), selected=GetTypeOfPalette())),
    column(width=6,highchartOutput("displayPalette", width="300px", height = "200px"))
  ),
  
  hidden(selectInput("choosePalette", "Palette", choices=listBrewerPalettes,selected=GetDefaultPalette(),width='200px')),
  
  hidden(uiOutput("customPaletteUI")),
  hr()
  )
})



GetDefaultPalette <- reactive({ rv$choosePalette})
observeEvent(input$choosePalette, {rv$choosePalette <-input$choosePalette })


GetTest <- reactive({
  req(rv$current.obj)
  rv$whichGroup2Color
  
  nbConds <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  pal <- rep('#000000', length(Biobase::pData(rv$current.obj)$Condition))
  
  
  nbColors <- NULL
  temp <- NULL
  if (is.null(rv$whichGroup2Color) || (rv$whichGroup2Color=="Condition")){
           nbColors <- length(unique(Biobase::pData(rv$current.obj)$Condition))
           nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
           nbColors <- max(nbColors,nbConds)
           palette <- NULL
           for(i in 1:nbConds){palette <- c(palette,input[[paste0("customColorCondition_",i)]])}
           for (i in 1:ncol(Biobase::exprs(rv$current.obj))){
             temp[i] <- palette[ which(pData(rv$current.obj)$Condition[i] == unique(Biobase::pData(rv$current.obj)$Condition))]
           }
           
   } else if (rv$whichGroup2Color=="Replicate"){
       nbColors <- length((Biobase::pData(rv$current.obj)$Condition))
       for(i in 1:nbColors){temp <- c(temp,input[[paste0("customColorCondition_",i)]])}
        }

  temp
  
})


GetWhichGroup2Color <- reactive({rv$whichGroup2Color})

observeEvent(input$whichGroup2Color,{
  rv$whichGroup2Color <- input$whichGroup2Color
  rv$PlotParams$paletteConditions <- GetExamplePalette()
})


############
GetExamplePalette <- reactive({
  rv$whichGroup2Color
  req(rv$current.obj)
  rv$typeOfPalette
   #req(input$typeOfPalette)
  pal <- rep('#000000', length(Biobase::pData(rv$current.obj)$Condition))
  
  nbConds <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  # if (is.null(rv$typeOfPalette)){
  #   if (is.null(input$whichGroup2Color) || (input$whichGroup2Color == "Condition") ){
  #     nbMinColors <-  3
  #     nbColors <- max(3,nbConds)
  #     palette <- RColorBrewer::brewer.pal(nbColors,listBrewerPalettes[1])[1:nbConds]
  #     temp <- NULL
  #     for (i in 1:ncol(Biobase::exprs(rv$current.obj))){
  #       temp[i] <- palette[ which(pData(rv$current.obj)$Condition[i] == unique(Biobase::pData(rv$current.obj)$Condition))]
  #     }
  #     
  #   }  else if (input$whichGroup2Color == "Replicate"){
  #     nbConds <- length(Biobase::pData(rv$current.obj)$Condition)
  #     temp <- RColorBrewer::brewer.pal(nbConds,listBrewerPalettes[1])
  #     
  #   }
  #   pal[1:length(temp)] <- temp 
  #   }
  # else {
    switch(rv$typeOfPalette,
    predefined={
      if ((rv$whichGroup2Color == "Condition") ){
       # nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
      nbColors <- max(3,nbConds)
      palette <- RColorBrewer::brewer.pal(nbColors,rv$choosePalette)[1:nbConds]
      temp <- NULL
      for (i in 1:ncol(Biobase::exprs(rv$current.obj))){
        temp[i] <- palette[ which(pData(rv$current.obj)$Condition[i] == unique(Biobase::pData(rv$current.obj)$Condition))]
      }
     
    }  else if (rv$whichGroup2Color == "Replicate"){
      nbConds <- length(Biobase::pData(rv$current.obj)$Condition)
      temp <- RColorBrewer::brewer.pal(nbConds,rv$choosePalette)
    }
      
    },
    custom = {
      nbConds <- length(unique(Biobase::pData(rv$current.obj)$Condition))
      pal <- rep('#000000', length(Biobase::pData(rv$current.obj)$Condition))
      
      
      nbColors <- NULL
      temp <- NULL
      if (is.null(rv$whichGroup2Color) || (rv$whichGroup2Color=="Condition")){
        nbColors <- length(unique(Biobase::pData(rv$current.obj)$Condition))
        nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
        nbColors <- max(nbColors,nbConds)
        palette <- NULL
        for(i in 1:nbConds){palette <- c(palette,input[[paste0("customColorCondition_",i)]])}
        for (i in 1:ncol(Biobase::exprs(rv$current.obj))){
          temp[i] <- palette[ which(pData(rv$current.obj)$Condition[i] == unique(Biobase::pData(rv$current.obj)$Condition))]
        }
        
      } else if (rv$whichGroup2Color=="Replicate"){
        nbColors <- length((Biobase::pData(rv$current.obj)$Condition))
        for(i in 1:nbColors){temp <- c(temp,input[[paste0("customColorCondition_",i)]])}
      }
 
    }
  )

  if (!is.null(temp)) {pal[1:length(temp)] <- temp}
  pal
})

observeEvent(input$typeOfPalette,{
  shinyjs::toggle("choosePalette", condition=input$typeOfPalette=="predefined")
  shinyjs::toggle("customPaletteUI", condition=input$typeOfPalette=="custom")
  rv$typeOfPalette <- input$typeOfPalette
})


GetTypeOfPalette <- reactive({rv$typeOfPalette})


output$customPaletteUI <- renderUI({
  rv$whichGroup2Color
  ll <- list()
  nbColors <- NULL
  switch(rv$whichGroup2Color,
    Condition={
      nbColors <- length(unique(Biobase::pData(rv$current.obj)$Condition))
      labels <- unique(Biobase::pData(rv$current.obj)$Condition)
    },
    Replicate={
      nbColors <- length((Biobase::pData(rv$current.obj)$Condition))
      labels <- Biobase::pData(rv$current.obj)$Condition
    }
  )
  
  for (i in 1:nbColors) {
    ll <- list(ll,
               colourpicker::colourInput(paste0("customColorCondition_",i), 
                                         labels[i],
                                         '#000000',
                                         showColour = "background"))
  }
  
  ll
})



observeEvent(c(rv$choosePalette,rv$typeOfPalette,rv$current.obj,GetTest(), rv$whichGroup2Color), {rv$PlotParams$paletteConditions <- GetExamplePalette()})
observeEvent(input$settings_nDigits,{rv$nDigits <- input$settings_nDigits})

observeEvent(input$colMEC, {rv$colorsTypeMV$MEC <- input$colMEC})
observeEvent(input$colPOV, { rv$colorsTypeMV$POV <- input$colPOV})
observeEvent(input$colVolcanoIn, {rv$colorsVolcanoplot$In <- input$colVolcanoIn})
observeEvent(input$colVolcanoOut, {rv$colorsVolcanoplot$Out <- input$colVolcanoOut})

output$displayPalette <- renderHighchart({
  #req(input$chooseNbColors)
  #GetTest()
  rv$PlotParams$paletteConditions
  nbConds <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  
  highchart() %>%
    my_hc_chart(chartType = "column") %>%
    hc_add_series(data = data.frame(y= abs(1+rnorm(ncol(Biobase::exprs(rv$current.obj))))), type="column", colorByPoint = TRUE) %>%
    hc_colors(rv$PlotParams$paletteConditions) %>%
    hc_plotOptions( column = list(stacking = "normal"),
                    animation=list(duration = 1)) %>%
     hc_legend(enabled = FALSE) %>%
    hc_yAxis(labels=FALSE,title = list(text = "")) %>%
    hc_xAxis(categories = 1:nbConds, title = list(text = ""))
     
  
})