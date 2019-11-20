# 
# observeEvent(input$linkToFaq1, {
#   updateTabsetPanel(session, 'navPage', "faqTab")
# })
# 
# 
# 
# color_renderer <- reactive({
#   conds <- rv$hot$Condition
#   pal <- rv$PlotParams$paletteConditions
#   
#   txt <- "function (instance, td, row, col, prop, value, cellProperties) {
#   Handsontable.renderers.TextRenderer.apply(this, arguments);"
#   c <- 1
#   for (i in 1:length(conds)){
#     if (conds[i] != "")
#       txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
#   }
#   txt <- paste0(txt,"}")
#   
#   return (txt)
# })
# 
# 
# 
# 
# #----------------------------------------------------------
# observeEvent(input$btn_checkConds,{
#   req(rv$widgets$Convert$datafile)
#   input$convert_reorder
#   
#   if (length(grep("Bio.Rep", colnames(rv$hot))) > 0)  { return(NULL)}
#   
#   if (input$convert_reorder== "Yes") {
#     rv$newOrder <- order(rv$hot["Condition"])
#     rv$hot <- rv$hot[rv$newOrder,]
#   }
#   
#   rv$conditionsChecked <- DAPAR::check.conditions(rv$hot$Condition)
#   
# })
# 
# 
# 
# #----------------------------------------------------------
# observeEvent(input$eData.box,{
#   rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
#                         Condition = rep("",length(input$eData.box)),
#                         stringsAsFactors = FALSE)
#   
#   
# })
# 
# #-------------------------------------------------------------
# output$hot <- renderRHandsontable({
#   rv$hot
#   input$chooseExpDesign
#   
#   if (is.null(rv$hot)){
#     rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
#                           Condition = rep("",length(input$eData.box)),
#                           stringsAsFactors = FALSE)
#   }
#   
#   hot <- rhandsontable::rhandsontable(rv$hot,rowHeaders=NULL, 
#                                       fillHandle = list(direction='vertical', 
#                                                         autoInsertRow=FALSE,
#                                                         maxRows=nrow(rv$hot))) %>%
#     rhandsontable::hot_rows(rowHeights = 30) %>%
#     rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
#                                     allowColEdit = FALSE,
#                                     allowInsertRow = FALSE,
#                                     allowInsertColumn = FALSE,
#                                     allowRemoveRow = TRUE,
#                                     allowRemoveColumn = FALSE,
#                                     autoInsertRow=FALSE     ) %>%
#     rhandsontable:: hot_cols(renderer = color_renderer()) %>%
#     rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
#   
#   if (!is.null(input$chooseExpDesign)) {
#     switch(input$chooseExpDesign,
#            FlatDesign = {
#              if ("Bio.Rep" %in% colnames(rv$hot))
#                hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
#            },
#            twoLevelsDesign = {
#              if ("Tech.Rep" %in% colnames(rv$hot))
#                hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
#            } ,
#            threeLevelsDesign = {
#              if ("Analyt.Rep" %in% colnames(rv$hot))
#                hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
#            }
#     )
#   }
#   hot
#   
# })
# 
# 
# 
# 
# 
# #----------------------------------------------------------
# output$UI_checkConditions  <- renderUI({
#   
#   req(rv$hot)
#   rv$conditionsChecked
#   input$convert_reorder
#   
#   if ((sum(rv$hot$Condition == "")==0) && (input$convert_reorder != "None")){
#     tags$div(
#       tags$div(style="display:inline-block;",
#                actionButton("btn_checkConds", "Check conditions", class = actionBtnClass)
#       ),
#       
#       tags$div(style="display:inline-block;",
#                if(!is.null(rv$conditionsChecked)){
#                  
#                  if (isTRUE(rv$conditionsChecked$valid)){
#                    img <- "images/Ok.png"
#                    txt <- "Correct conditions"
#                  }else {
#                    img <- "images/Problem.png"
#                    txt <- "Invalid conditions"
#                  }
#                  tagList(
#                    tags$div(
#                      tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
#                      tags$div(style="display:inline-block;",tags$p(txt))
#                    ),
#                    if(!isTRUE(rv$conditionsChecked$valid)){
#                      tags$p(rv$conditionsChecked$warn)
#                    }
#                  )
#                }
#       )
#     )
#   } else {
#     tagList(
#       br(),br(),
#       br(),
#       br()
#     )
#     
#   }
# })
# 
# 
# 
# #------------------------------------------------------------------------------
# output$UI_hierarchicalExp <- renderUI({
#   req(rv$conditionsChecked)
#   if (!isTRUE(rv$conditionsChecked$valid)){return(NULL)
#   } else {
#     tagList(
#       div(
#         div(
#           # edit1
#           style="display:inline-block; vertical-align: middle;",
#           tags$b("2 - Choose the type of experimental design and complete it accordingly")
#         ),
#         div(
#           # edit2
#           style="display:inline-block; vertical-align: middle;",
#           tags$button(id="btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
#         )
#       ),
#       
#       radioButtons("chooseExpDesign", "",
#                    choices = c("Flat design (automatic)" = "FlatDesign" ,
#                                "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
#                                "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" )
#       )
#     )
#   }
#   
# })
# 
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------------
# output$viewDesign <- renderUI({
#   
#   rv$designSaved
#   if (isTRUE(rv$designSaved)){return(NULL)}
#   
#   tagList(
#     h4("Design"),
#     rHandsontableOutput("hot")
#   )
# })
# 
# 
# callModule(moduleDesignExample,"buildDesignExampleThree", 3)
# callModule(moduleDesignExample,"buildDesignExampleTwo", 2)
# 
# 
# #------------------------------------------------------------------------------
# output$designExamples <- renderUI({
#   input$chooseExpDesign
#   print(paste0("in designExamples renderUI, chooseExpDesign = ",input$chooseExpDesign))
#   switch(input$chooseExpDesign,
#          FlatDesign = 
#            {
#              tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
#            },
#          twoLevelsDesign =  {
#            tagList(h4("Example for a 2-levels design"),
#                    moduleDesignExampleUI("buildDesignExampleTwo")
#            )
#          },
#          threeLevelsDesign =  {
#            tagList(
#              h4("Example for a 3-levels design"),
#              moduleDesignExampleUI("buildDesignExampleThree")
#            )
#          }
#   )
# })
# 
# 
# #------------------------------------------------------------------------------
# observe({
#   shinyjs::onclick("btn_helpDesign",{
#     shinyjs::toggle(id = "showExamples", anim = TRUE)}
#   )
# })
# 
# #------------------------------------------------------------------------------
# observeEvent(input$chooseExpDesign, {
#   req(rv$hot)
#   rv$designChecked <- NULL
#   switch(input$chooseExpDesign,
#          FlatDesign = {
#            rv$hot  <- data.frame(rv$hot[,1:2],
#                                  Bio.Rep = seq(1:nrow(rv$hot)),
#                                  stringsAsFactors = FALSE)
#          },
#          twoLevelsDesign = {
#            rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
#                                  Tech.Rep = seq(1:nrow(rv$hot)),
#                                  stringsAsFactors = FALSE)
#          },
#          threeLevelsDesign = {
#            #if (length(grep("Tech.Rep", colnames(rv$hot))) > 0) { return(NULL)}
#            rv$hot  <- data.frame(rv$hot[,1:2],
#                                  Bio.Rep = rep("",nrow(rv$hot)),
#                                  Tech.Rep = rep("",nrow(rv$hot)),
#                                  Analyt.Rep = seq(1:nrow(rv$hot)),
#                                  stringsAsFactors = FALSE)
#          }
#   )
# })
# 
# 
# 
# 
# #------------------------------------------------------------------------------
# observeEvent(input$hot,{ rv$hot <-  hot_to_r(input$hot)})
# 
# 
# 
# #------------------------------------------------------------------------------
# observeEvent(input$btn_checkDesign,{ rv$designChecked <- DAPAR::check.design(rv$hot)})
# 
# #------------------------------------------------------------------------------
# output$checkDesign <- renderUI({
#   req(input$chooseExpDesign)
#   req(rv$widgets$Convert$datafile)
#   rv$designChecked
#   req(rv$conditionsChecked)
#   
#   if(!isTRUE(rv$conditionsChecked$valid)){return(NULL)}
#   switch(isolate({input$chooseExpDesign}),
#          FlatDesign = {},
#          twoLevelsDesign = { if (sum(rv$hot$Bio.Rep == "") > 0) {return(NULL)}},
#          threeLevelsDesign = {if ((sum(rv$hot$Bio.Rep == "") + sum(rv$hot$Tech.Rep == "")) > 0) {return(NULL)}}
#   )
#   
#   
#   tags$div(
#     tags$div(
#       style="display:inline-block;",
#       actionButton("btn_checkDesign", "Check design", class = actionBtnClass)
#     ),
#     
#     tags$div(
#       style="display:inline-block;",
#       if(!is.null(rv$designChecked)){
#         
#         if (isTRUE(rv$designChecked$valid)){
#           shinyjs::enable("createMSnsetButton")
#           img <- "images/Ok.png"
#           txt <- "Correct design"
#           # rvModProcess$moduleConvertDone[4] <- TRUE
#         }else {
#           img <- "images/Problem.png"
#           txt <- "Invalid design"}
#         tagList(
#           tags$div(
#             tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
#             tags$div(style="display:inline-block;",tags$p(txt))
#           ),
#           if(!isTRUE(rv$designChecked$valid)){
#             shinyjs::disable("createMSnsetButton")
#             tags$p(rv$designChecked$warn)
#           } else {
#             shinyjs::enable("createMSnsetButton")
#           }
#         )
#       } else {
#         shinyjs::disable("createMSnsetButton")
#       }
#     )
#     
#   )
#   
#   
# })







observeEvent(input$linkToFaq1, {
  updateTabsetPanel(session, 'navPage', "faqTab")
})



color_renderer <- reactive({
  conds <- rv$hot$Condition
  pal <- rv$PlotParams$paletteConditions
  
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
  req(rv$designChecked)
  if (!(rv$designChecked$valid)){return(NULL)}
  tagList(
    uiOutput("checkAll_convert", width="50"),
    htmlOutput("msgAlertCreateMSnset"),
    hr(),
    textInput("filenameToCreate","Enter the name of the study"),
    actionButton("createMSnsetButton","Convert data", class = actionBtnClass),
    uiOutput("warningCreateMSnset")
    
  )
})

#----------------------------------------------------------
observeEvent(input$btn_checkConds,{
  input$convert_reorder
  
  if (length(grep("Bio.Rep", colnames(rv$hot))) > 0)  { return(NULL)}
  
  if (isTRUE(input$convert_reorder)) {
    rv$newOrder <- order(rv$hot["Condition"])
    rv$hot <- rv$hot[rv$newOrder,]
  }
  
  rv$conditionsChecked <- DAPAR::check.conditions(rv$hot$Condition)
  
})



#----------------------------------------------------------
observeEvent(input$eData.box,{
  rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                        Condition = rep("",length(input$eData.box)),
                        stringsAsFactors = FALSE)
  
  
})

#-------------------------------------------------------------
output$hot <- renderRHandsontable({
  rv$hot
  input$chooseExpDesign
  
  if (is.null(rv$hot)){
    rv$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                          Condition = rep("",length(input$eData.box)),
                          stringsAsFactors = FALSE)
  }
  
  hot <- rhandsontable::rhandsontable(rv$hot,rowHeaders=NULL, 
                                      fillHandle = list(direction='vertical', 
                                                        autoInsertRow=FALSE,
                                                        maxRows=nrow(rv$hot))) %>%
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
             if ("Bio.Rep" %in% colnames(rv$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
           },
           twoLevelsDesign = {
             if ("Tech.Rep" %in% colnames(rv$hot))
               hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
           } ,
           threeLevelsDesign = {
             if ("Analyt.Rep" %in% colnames(rv$hot))
               hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
           }
    )
  }
  hot
  
})





#----------------------------------------------------------
output$UI_checkConditions  <- renderUI({
  
  req(rv$hot)
  rv$conditionsChecked
  input$convert_reorder
  
  if ((sum(rv$hot$Condition == "")==0) && (input$convert_reorder != "None")){
    tags$div(
      tags$div(style="display:inline-block;",
               actionButton("btn_checkConds", "Check conditions", class = actionBtnClass)
      ),
      
      tags$div(style="display:inline-block;",
               if(!is.null(rv$conditionsChecked)){
                 
                 if (isTRUE(rv$conditionsChecked$valid)){
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
                   if(!isTRUE(rv$conditionsChecked$valid)){
                     tags$p(rv$conditionsChecked$warn)
                   }
                 )
               }
      )
    )
  } else {
    tagList(
      br(),br(),
      br(),
      br()
    )
    
  }
})



#------------------------------------------------------------------------------
output$UI_hierarchicalExp <- renderUI({
  req(rv$conditionsChecked)
  if (!isTRUE(rv$conditionsChecked$valid)){return(NULL)
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
      
      radioButtons("chooseExpDesign", "",
                   choices = c("Flat design (automatic)" = "FlatDesign" ,
                               "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                               "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                   selected=character(0))
    )
  }
  
})






#------------------------------------------------------------------------------
output$viewDesign <- renderUI({
  
  rv$designSaved
  if (isTRUE(rv$designSaved)){return(NULL)}
  
  tagList(
    h4("Design"),
    rHandsontableOutput("hot")
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
           tagList(h4("Example for a 2-levels design"),
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
  rv$hot
  rv$designChecked <- NULL
  switch(input$chooseExpDesign,
         FlatDesign = {
           rv$hot  <- data.frame(rv$hot[,1:2],
                                 Bio.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv$hot  <- data.frame(rv$hot[,1:2],Bio.Rep = rep("",nrow(rv$hot)),
                                 Tech.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           #if (length(grep("Tech.Rep", colnames(rv$hot))) > 0) { return(NULL)}
           rv$hot  <- data.frame(rv$hot[,1:2],
                                 Bio.Rep = rep("",nrow(rv$hot)),
                                 Tech.Rep = rep("",nrow(rv$hot)),
                                 Analyt.Rep = seq(1:nrow(rv$hot)),
                                 stringsAsFactors = FALSE)
         }
  )
})




#------------------------------------------------------------------------------
observeEvent(input$hot,{ rv$hot <-  hot_to_r(input$hot)})



#------------------------------------------------------------------------------
observeEvent(input$btn_checkDesign,{ rv$designChecked <- DAPAR::check.design(rv$hot)})

#------------------------------------------------------------------------------
output$checkDesign <- renderUI({
  req(input$chooseExpDesign)
  rv$designChecked
  req(rv$conditionsChecked)
  
  if(!isTRUE(rv$conditionsChecked$valid)){return(NULL)}
  switch(isolate({input$chooseExpDesign}),
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv$hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv$hot$Bio.Rep == "")+sum(rv$hot$Tech.Rep == "")) > 0) {return(NULL)}}
  )
  
  
  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton("btn_checkDesign", "Check design", class = actionBtnClass)
    ),
    
    tags$div(
      style="display:inline-block;",
      if(!is.null(rv$designChecked)){
        
        if (isTRUE(rv$designChecked$valid)){
          shinyjs::enable("createMSnsetButton")
          img <- "images/Ok.png"
          txt <- "Correct design"
        }else {
          img <- "images/Problem.png"
          txt <- "Invalid design"}
        tagList(
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
            tags$div(style="display:inline-block;",tags$p(txt))
          ),
          if(!isTRUE(rv$designChecked$valid)){
            shinyjs::disable("createMSnsetButton")
            tags$p(rv$designChecked$warn)
          } else {
            shinyjs::enable("createMSnsetButton")
          }
        )
      } else {
        shinyjs::disable("createMSnsetButton")
      }
    )
    
  )
  
  
})
