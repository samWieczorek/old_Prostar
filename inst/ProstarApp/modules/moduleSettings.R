


moduleSettingsUI  <- function(id){
  ns <- NS(id)
  
  tabPanel(title="Global settings",
           value="GlobalSettingsTab",
           # selectInput("settings_InteractivePlots",
           #             "Type of plots",
           #             choices = c("Interactive (nice but slower)" = "Interactive",
           #                     "Static (faster)" = "Static")),
           tabsetPanel(
             tabPanel("Miscallenous",
                      div(
                        div(
                          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                          modulePopoverUI(ns("modulePopover_numPrecision"))
                        ),
                        div(
                          style="display:inline-block; vertical-align: middle;",
                          uiOutput(ns("settings_nDigits_UI"))
                        )
                      ),
                      tags$br(),tags$hr(),
                      tags$p(style="font-size: 18px;", tags$b("Figure export options")),
                      tagList(
                        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                                  selectInput(ns("sizePNGplots"), "Size of images (PNG)", choices = c("1200 * 800"), width='150px')),
                        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                                  selectInput(ns("resoPNGplots"), "Resolution", choices = c(150), width='100px'))
                      )),
             tabPanel("Colors",
                      div(id = ns('showInfoColorOptions'), tags$p("Color customization is available after data loading only.")),
                      hidden(uiOutput(ns("defineColorsUI")))
             )
           )
  )
  
  
}


moduleSettings  <- function(input, output, session, dataIn){
  ns <- session$ns
  


rv.settings <- reactiveValues(
  nDigits = 10,
  colorsVolcanoplot = list(In=orangeProstar, Out='lightgrey'),
  colorsTypeMV = list(MEC=orangeProstar, POV='lightblue'),
  choosePalette = 'Dark2',
  typeOfPalette = 'predefined',
  whichGroup2Color = 'Condition'
)




callModule(modulePopover,"modulePopover_numPrecision", data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">Numerical precisions</font></strong>")),
                                                                            content= "Set the number of decimals to display for numerical values.")))

observe({
  shinyjs::toggle("defineColorsUI", condition=!is.null(dataIn()))
  shinyjs::toggle("showInfoColorOptions", condition=is.null(dataIn()))
})


output$settings_nDigits_UI <- renderUI({
  numericInput("settings_nDigits", "", value=rv.settings$nDigits, min=0, width="100px")
})

observeEvent(input$settings_nDigits,{ rv.settings$nDigits <- input$settings_nDigits })


# observeEvent(input$shinythemeSelector,{
#   tags$script("$('#shinythemeSelector')\n  .on('change', function(el) {\n      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';\n    }\n\n    // Find the <link> element with that has the bootstrap.css\n    var $link = $('link').filter(function() {\n      var theme = $(this).attr('href');\n      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');\n      return $.inArray(theme, allThemes) !== -1;\n    });\n\n    // Set it to the correct path\n    $link.attr('href', curThemePath);\n  });")
#   #theme = shinytheme(input$shinythemeSelector)
# })

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
               colourpicker::colourInput("colMEC", "Select colour for MEC", orangeProstar,showColour = "background"),
               colourpicker::colourInput("colPOV", "Select colour for POV", "lightblue", showColour = "background")
             ), style = "primary"),
             bsCollapsePanel("Colors for volcanoplots", 
                             colourpicker::colourInput("colVolcanoIn", "Select colour for selected entities", 
                                                       rv.settings$colorsVolcanoplot$In,
                                                       showColour = "background"),
                             colourpicker::colourInput("colVolcanoOut", "Select colour for filtered out entities", rv.settings$colorsVolcanoplot$Out, showColour = "background"), 
                             style = "primary"),
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



GetDefaultPalette <- reactive({ rv.settings$choosePalette})
observeEvent(input$choosePalette, {rv.settings$choosePalette <-input$choosePalette })


GetTest <- reactive({
  req(dataIn())
  rv.settings$whichGroup2Color
  
  nbConds <- length(unique(Biobase::pData(dataIn())$Condition))
  pal <- rep('#000000', length(Biobase::pData(dataIn())$Condition))
  
  
  nbColors <- NULL
  temp <- NULL
  if (is.null(rv.settings$whichGroup2Color) || (rv.settings$whichGroup2Color=="Condition")){
    nbColors <- length(unique(Biobase::pData(dataIn())$Condition))
    nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
    nbColors <- max(nbColors,nbConds)
    palette <- NULL
    for(i in 1:nbConds){palette <- c(palette,input[[paste0("customColorCondition_",i)]])}
    for (i in 1:ncol(Biobase::exprs(dataIn()))){
      temp[i] <- palette[ which(pData(dataIn())$Condition[i] == unique(Biobase::pData(dataIn())$Condition))]
    }
    
  } else if (rv.settings$whichGroup2Color=="Replicate"){
    nbColors <- length((Biobase::pData(dataIn())$Condition))
    for(i in 1:nbColors){temp <- c(temp,input[[paste0("customColorCondition_",i)]])}
  }
  
  temp
  
})


GetWhichGroup2Color <- reactive({rv.settings$whichGroup2Color})

observeEvent(input$whichGroup2Color,{
  rv.settings$whichGroup2Color <- input$whichGroup2Color
  rv$PlotParams$paletteConditions <- GetExamplePalette()
})


############
GetExamplePalette <- reactive({
  rv.settings$whichGroup2Color
  req(dataIn())
  rv.settings$typeOfPalette
  #req(input$typeOfPalette)
  pal <- rep('#000000', length(Biobase::pData(dataIn())$Condition))
  
  nbConds <- length(unique(Biobase::pData(dataIn())$Condition))
  # if (is.null(rv$typeOfPalette)){
  #   if (is.null(input$whichGroup2Color) || (input$whichGroup2Color == "Condition") ){
  #     nbMinColors <-  3
  #     nbColors <- max(3,nbConds)
  #     palette <- RColorBrewer::brewer.pal(nbColors,listBrewerPalettes[1])[1:nbConds]
  #     temp <- NULL
  #     for (i in 1:ncol(Biobase::exprs(dataIn()))){
  #       temp[i] <- palette[ which(pData(dataIn())$Condition[i] == unique(Biobase::pData(dataIn())$Condition))]
  #     }
  #     
  #   }  else if (input$whichGroup2Color == "Replicate"){
  #     nbConds <- length(Biobase::pData(dataIn())$Condition)
  #     temp <- RColorBrewer::brewer.pal(nbConds,listBrewerPalettes[1])
  #     
  #   }
  #   pal[1:length(temp)] <- temp 
  #   }
  # else {
  switch(rv.settings$typeOfPalette,
         predefined={
           if ((rv.settings$whichGroup2Color == "Condition") ){
             # nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
             nbColors <- max(3,nbConds)
             palette <- RColorBrewer::brewer.pal(nbColors,rv.settings$choosePalette)[1:nbConds]
             temp <- NULL
             for (i in 1:ncol(Biobase::exprs(dataIn()))){
               temp[i] <- palette[ which(pData(dataIn())$Condition[i] == unique(Biobase::pData(dataIn())$Condition))]
             }
             
           }  else if (rv.settings$whichGroup2Color == "Replicate"){
             nbConds <- length(Biobase::pData(dataIn())$Condition)
             temp <- RColorBrewer::brewer.pal(nbConds,rv.settings$choosePalette)
           }
           
         },
         custom = {
           nbConds <- length(unique(Biobase::pData(dataIn())$Condition))
           pal <- rep('#000000', length(Biobase::pData(dataIn())$Condition))
           
           
           nbColors <- NULL
           temp <- NULL
           if (is.null(rv.settings$whichGroup2Color) || (rv.settings$whichGroup2Color=="Condition")){
             nbColors <- length(unique(Biobase::pData(dataIn())$Condition))
             nbColors <-  brewer.pal.info[listBrewerPalettes[1],]$mincolors
             nbColors <- max(nbColors,nbConds)
             palette <- NULL
             for(i in 1:nbConds){palette <- c(palette,input[[paste0("customColorCondition_",i)]])}
             for (i in 1:ncol(Biobase::exprs(dataIn()))){
               temp[i] <- palette[ which(pData(dataIn())$Condition[i] == unique(Biobase::pData(dataIn())$Condition))]
             }
             
           } else if (rv.settings$whichGroup2Color=="Replicate"){
             nbColors <- length((Biobase::pData(dataIn())$Condition))
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
  rv.settings$typeOfPalette <- input$typeOfPalette
})


GetTypeOfPalette <- reactive({rv.settings$typeOfPalette})


output$customPaletteUI <- renderUI({
  rv.settings$whichGroup2Color
  ll <- list()
  nbColors <- NULL
  switch(rv.settings$whichGroup2Color,
         Condition={
           nbColors <- length(unique(Biobase::pData(dataIn())$Condition))
           labels <- unique(Biobase::pData(dataIn())$Condition)
         },
         Replicate={
           nbColors <- length((Biobase::pData(dataIn())$Condition))
           labels <- Biobase::pData(dataIn())$Condition
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



observeEvent(c(rv.settings$choosePalette,rv.settings$typeOfPalette,dataIn(),GetTest(), rv.settings$whichGroup2Color), {rv$PlotParams$paletteConditions <- GetExamplePalette()})

observeEvent(input$colMEC, {rv.settings$colorsTypeMV$MEC <- input$colMEC})
observeEvent(input$colPOV, { rv.settings$colorsTypeMV$POV <- input$colPOV})
observeEvent(input$colVolcanoIn, {rv.settings$colorsVolcanoplot$In <- input$colVolcanoIn})
observeEvent(input$colVolcanoOut, {rv.settings$colorsVolcanoplot$Out <- input$colVolcanoOut})

output$displayPalette <- renderHighchart({
  #req(input$chooseNbColors)
  #GetTest()
  rv$PlotParams$paletteConditions
  nbConds <- length(unique(Biobase::pData(dataIn())$Condition))
  
  highchart() %>%
    my_hc_chart(chartType = "column") %>%
    hc_add_series(data = data.frame(y= abs(1+rnorm(ncol(Biobase::exprs(dataIn()))))), type="column", colorByPoint = TRUE) %>%
    hc_colors(rv$PlotParams$paletteConditions) %>%
    hc_plotOptions( column = list(stacking = "normal"),
                    animation=list(duration = 1)) %>%
    hc_legend(enabled = FALSE) %>%
    hc_yAxis(labels=FALSE,title = list(text = "")) %>%
    hc_xAxis(categories = 1:nbConds, title = list(text = ""))
})

return(rv.settings)

}