source(file.path(".", "modules/DataManager/moduleDesignExample.R"),  local = TRUE)$value



rv.buildDesign <- reactiveValues(
  hot = NULL,
  designChecked = NULL,
  newOrder = NULL,
  conditionsChecked = NULL,
  designSaved = NULL
)



observeEvent(input$linkToFaq1, {
  updateTabsetPanel(session, 'navPage', "faqTab")
})




color_renderer <- reactive({
  conds <- rv.buildDesign$hot$Condition
  pal <- rv.prostar$settings()$examplePalette
  txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
  c <- 1
  for (i in 1:length(conds)){
    if (conds[i] != "")
      txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
  }
  txt <- paste0(txt,"}")
  
  return (txt)
})



output$convertFinalStep <- renderUI({
  req(rv.buildDesign$designChecked)
  if (!(rv.buildDesign$designChecked$valid)){return(NULL)}
  tagList(
    uiOutput(ns("checkAll_convert"), width="50"),
    htmlOutput("msgAlertCreateMSnset"),
    hr(),
    textInput(ns("filenameToCreate"),"Enter the name of the study"),
    actionButton(ns("createMSnsetButton"),"Convert data", class = actionBtnClass),
    uiOutput(ns("warningCreateMSnset"))
    
  )
})

#----------------------------------------------------------
observeEvent(input$btn_checkConds,{
  input$convert_reorder
  
  if (length(grep("Bio.Rep", colnames(rv.buildDesign$hot))) > 0)  { return(NULL)}
  
  if (isTRUE(input$convert_reorder)) {
    rv.buildDesign$newOrder <- order(rv.buildDesign$hot["Condition"])
    rv.buildDesign$hot <- rv.buildDesign$hot[rv.buildDesign$newOrder,]
  }
  
  rv.buildDesign$conditionsChecked <- DAPAR::check.conditions(rv.buildDesign$hot$Condition)
  
})



#----------------------------------------------------------
observeEvent(input$eData.box,{
  rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                        Condition = rep("",length(input$eData.box)),
                        stringsAsFactors = FALSE)
  
  
})

#-------------------------------------------------------------
output$hot <- renderRHandsontable({
  rv.buildDesign$hot
  input$chooseExpDesign
  
  if (is.null(rv.buildDesign$hot)){
    rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                          Condition = rep("",length(input$eData.box)),
                          stringsAsFactors = FALSE)
  }
  
  hot <- rhandsontable::rhandsontable(rv.buildDesign$hot,rowHeaders=NULL, 
                                      fillHandle = list(direction='vertical', 
                                                        autoInsertRow=FALSE,
                                                        maxRows=nrow(rv.buildDesign$hot))) %>%
    rhandsontable::hot_rows(rowHeights = 30) %>%
    rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                    allowColEdit = FALSE,
                                    allowInsertRow = FALSE,
                                    allowInsertColumn = FALSE,
                                    allowRemoveRow = TRUE,
                                    allowRemoveColumn = FALSE,
                                    autoInsertRow=FALSE     ) %>%
    rhandsontable:: hot_cols(renderer = color_renderer()) %>%
    rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
  
  if (!is.null(input$chooseExpDesign)) {
    switch(input$chooseExpDesign,
           FlatDesign = {
             if ("Bio.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
           },
           twoLevelsDesign = {
             if ("Tech.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
           } ,
           threeLevelsDesign = {
             if ("Analyt.Rep" %in% colnames(rv.buildDesign$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
           }
    )
  }
  hot
  
})





#----------------------------------------------------------
output$UI_checkConditions  <- renderUI({
  
  req(rv.buildDesign$hot)
  rv.buildDesign$conditionsChecked
  input$convert_reorder
  
  if ((sum(rv.buildDesign$hot$Condition == "")==0) && (input$convert_reorder != "None")){
    tags$div(
      tags$div(style="display:inline-block;",
               actionButton(ns("btn_checkConds"), "Check conditions", class = actionBtnClass)
      ),
      
      tags$div(style="display:inline-block;",
               if(!is.null(rv.buildDesign$conditionsChecked)){
                 
                 if (isTRUE(rv.buildDesign$conditionsChecked$valid)){
                   img <- "images/Ok.png"
                   txt <- "Correct conditions"
                 }else {
                   img <- "images/Problem.png"
                   txt <- "Invalid conditions"
                 }
                 tagList(
                   tags$div(
                     tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                     tags$div(style="display:inline-block;",tags$p(txt))
                   ),
                   if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){
                     tags$p(rv.buildDesign$conditionsChecked$warn)
                   }
                 )
               }
      )
    )
  } else {
    tagList(
      br(),
      br()
    )
    
  }
})



#------------------------------------------------------------------------------
output$UI_hierarchicalExp <- renderUI({
  req(rv.buildDesign$conditionsChecked)
  if (!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)
  } else {
    tagList(
      div(
        div(
          # edit1
          style="display:inline-block; vertical-align: middle;",
          tags$b("2 - Choose the type of experimental design and complete it accordingly")
        ),
        div(
          # edit2
          style="display:inline-block; vertical-align: middle;",
          tags$button(id="btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
        )
      ),
      
      radioButtons(ns("chooseExpDesign"), "",
                   choices = c("Flat design (automatic)" = "FlatDesign" ,
                               "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                               "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                   selected=character(0))
    )
  }
  
})






#------------------------------------------------------------------------------
output$viewDesign <- renderUI({
  
  rv.buildDesign$designSaved
  if (isTRUE(rv.buildDesign$designSaved)){return(NULL)}
  
  tagList(
    h4("Design"),
    rHandsontableOutput(ns("hot"))
  )
})


callModule(moduleDesignExample,"buildDesignExampleThree", 3)
callModule(moduleDesignExample,"buildDesignExampleTwo", 2)


#------------------------------------------------------------------------------
output$designExamples <- renderUI({
  input$chooseExpDesign
  print(input$chooseExpDesign)
  switch(input$chooseExpDesign,
         FlatDesign = 
         {
           tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
         },
         twoLevelsDesign =  {
           tagList(
             h4("Example for a 2-levels design"),
             moduleDesignExampleUI("buildDesignExampleTwo")
           )
         },
         threeLevelsDesign =  {
           tagList(
             h4("Example for a 3-levels design"),
             moduleDesignExampleUI("buildDesignExampleThree")
           )
         }
  )
})


#------------------------------------------------------------------------------
observe({
  shinyjs::onclick("btn_helpDesign",{
    shinyjs::toggle(id = "showExamples", anim = TRUE)}
  )
})

#------------------------------------------------------------------------------
observeEvent(input$chooseExpDesign, {
  rv.buildDesign$hot
  rv.buildDesign$designChecked <- NULL
  switch(input$chooseExpDesign,
         FlatDesign = {
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                 Bio.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                 stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                 Tech.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                 stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           #if (length(grep("Tech.Rep", colnames(rv.buildDesign$hot))) > 0) { return(NULL)}
           rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                 Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                 Tech.Rep = rep("",nrow(rv.buildDesign$hot)),
                                 Analyt.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                 stringsAsFactors = FALSE)
         }
  )
})




#------------------------------------------------------------------------------
observeEvent(input$hot,{ rv.buildDesign$hot <-  hot_to_r(input$hot)})



#------------------------------------------------------------------------------
observeEvent(input$btn_checkDesign,{ 
  rv.buildDesign$designChecked <- DAPAR::check.design(rv.buildDesign$hot)
  print(rv.buildDesign$designChecked)

})

#------------------------------------------------------------------------------
output$checkDesign <- renderUI({
  req(input$chooseExpDesign)
  rv.buildDesign$designChecked
  req(rv.buildDesign$conditionsChecked)
  
  if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)}
  switch(isolate({input$chooseExpDesign}),
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv.buildDesign$hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv.buildDesign$hot$Bio.Rep == "")+sum(rv.buildDesign$hot$Tech.Rep == "")) > 0) {return(NULL)}}
  )
  
  
  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton(ns("btn_checkDesign"), "Check design", class = actionBtnClass)
    ),
    
    tags$div(
      style="display:inline-block;",
      if(!is.null(rv.buildDesign$designChecked)){
        
        if (isTRUE(rv.buildDesign$designChecked$valid)){
          shinyjs::enable("createMSnsetButton")
          img <- "images/Ok.png"
          txt <- "Correct design"
          #rvNavProcess$Done[4] <- TRUE
        }else {
          img <- "images/Problem.png"
          txt <- "Invalid design"}
        tagList(
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          ),
          if(!isTRUE(rv.buildDesign$designChecked$valid)){
            shinyjs::disable("createMSnsetButton")
            tags$p(rv.buildDesign$designChecked$warn)
          } else {
            shinyjs::enable("createMSnsetButton")
            #rvNavProcess$Done[4] <- TRUE
          }
        )
      } else {
        shinyjs::disable("createMSnsetButton")
      }
    )
    
  )
  
  
})





