


output$updateDesign_UI_checkConditions  <- renderUI({
    
    req(rv$updateDesign_hot)
    rv$updateDesign_conditionsChecked
    
    
    if (sum(rv$updateDesign_hot$Condition == "")==0){
        tags$div(
            tags$div(style="display:inline-block;",
                     actionButton("updateDesign_btn_checkConds", "Check conditions")
            ),
            
            tags$div(style="display:inline-block;",
                     if(!is.null(rv$updateDesign_conditionsChecked)){
                         
                         if (isTRUE(rv$updateDesign_conditionsChecked$valid)){
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
                             if(!isTRUE(rv$updateDesign_conditionsChecked$valid)){
                                 tags$p(rv$updateDesign_conditionsChecked$warn)
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





#-----------------------------------------------------------
observeEvent(input$updateDesign_btn_resetDesign, {
    req(input$updateDesign_chooseExpDesign)
    
    switch(input$updateDesign_chooseExpDesign,
           FlatDesign = { },
           twoLevelsDesign = {
               if ("Tech.Rep" %in% colnames(rv$updateDesign_hot)){
                   rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],Bio.Rep = rep("",nrow(rv$updateDesign_hot)),
                                                     Tech.Rep = seq(1:nrow(rv$updateDesign_hot)),
                                                     stringsAsFactors = FALSE)
                       }
           },
           threeLevelsDesign = {
               if ("Analyt.Rep" %in% colnames(updateDesign_hot)){
                   rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],
                                                      Bio.Rep = rep("",nrow(rv$updateDesign_hot)),
                                                      Tech.Rep = rep("",nrow(rv$updateDesign_hot)),
                                                      Analyt.Rep = seq(1:nrow(rv$updateDesign_hot)),
                                                      stringsAsFactors = FALSE)
                   
                   }
           }
    )
    
    
})




#############################################################
observeEvent(input$btn_SaveDesign,{
  rv$updateDesign_hot

  rv$current.obj <- rv$current.obj[, rv$updateDesign_newOrder]
  tmp <-  rv$updateDesign_hot
  rownames(tmp) <- colnames(Biobase::exprs(rv$current.obj))
  # if (is.character(rownames(tmp))){
  #     rownames(tmp) <- gsub(".", "_", rownames(tmp), fixed=TRUE)
  #     
  # }
  Biobase::pData(rv$current.obj) <- tmp
  loadObjectInMemoryFromConverter()

  rv$updateDesign_designSaved <- TRUE
  shinyjs::disable("updateDesign_btn_checkConds")
  shinyjs::disable("updateDesign_btn_checkDesign")
  shinyjs::disable("btn_SaveDesign")
  shinyjs::disable("updateDesign_chooseExpDesign")
  
  rv$current.obj@experimentData@other$Prostar_Version = installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
  rv$current.obj@experimentData@other$DAPAR_Version = installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
  
  NeedsUpdate()
  BuildNavbarPage()
})


#############################################################
observe({
  req(input$updateDesign_hot)
  rv$updateDesign_hot <-  hot_to_r(input$updateDesign_hot)

})



 output$designUpdated <- renderUI({
  req(rv$updateDesign_designSaved)

  if(isTRUE(rv$updateDesign_designSaved)){
    tagList(
        p("The design has been updated. You can export the dataset as a MSnset file then close/reopen Prostar")
    )
  }
})

#############################################################
output$updateDesign_SaveDesign <- renderUI({
  req(rv$updateDesign_designChecked)
  rv$updateDesign_designSaved

  if (!isTRUE(rv$updateDesign_designChecked$valid)){return(NULL)}
  if (isTRUE(rv$updateDesign_designSaved)){return(NULL)}

  fluidRow(
      column(width=6,tags$b("3 - Click the button to update the design.")),
      column(width=6,tags$div(
          tags$div(style="display:inline-block;",
                   actionButton("btn_SaveDesign","Save design")
          ),
          
          tags$div(style="display:inline-block;",
                   if(isTRUE(rv$updateDesign_designSaved)){
                       tags$div(
                           tags$div(style="display:inline-block;",tags$img(src = "images/Ok.png", height=25)),
                           tags$div(style="display:inline-block;",tags$p("Design updated"))
                       )
                   }
          )
      ) )
  )
  

})



updateDesign_color_renderer_NewDesign <- reactive({
  conds <- rv$updateDesign_hot$Condition
  pal <- RColorBrewer::brewer.pal(length(unique(conds)),"Dark2")
  
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


#############################################################
observeEvent(input$updateDesign_btn_checkConds,{
  
  if (length(grep("Bio.Rep", colnames(rv$updateDesign_hot))) > 0) { return(NULL)}
  
  rv$updateDesign_newOrder <- order(rv$updateDesign_hot["Condition"])
  rv$updateDesign_hot <- rv$updateDesign_hot[rv$updateDesign_newOrder,]
  rv$updateDesign_conditionsChecked <- DAPAR::check.conditions(rv$updateDesign_hot$Condition)
  
})


#-------------------------------------------------------------
output$updateDesign_hot <- renderRHandsontable({
  rv$updateDesign_hot
  input$updateDesign_chooseExpDesign
  
  n <- nrow(Biobase::pData(rv$current.obj))
    if (is.null(rv$updateDesign_hot)){
        rv$updateDesign_hot  <- data.frame(Sample.name = as.character(Biobase::pData(rv$current.obj)$Sample.name),
                                           Condition = rep("", n),
                                           stringsAsFactors = FALSE)
    }
  
  updateDesign_hot <-
    rhandsontable::rhandsontable(rv$updateDesign_hot,rowHeaders=NULL, 
                                 fillHandle = list(direction='vertical', 
                                                   autoInsertRow=FALSE,
                                                   maxRows=nrow(rv$updateDesign_hot)
                                                   )
                                 ) %>%
    rhandsontable::hot_rows(rowHeights = 30) %>%
    rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,
                     allowInsertColumn = FALSE,
                     allowRemoveRow = TRUE,
                     allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%

    rhandsontable::hot_cols(renderer = updateDesign_color_renderer_NewDesign()) %>%
    rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
  
  if (!is.null(input$updateDesign_chooseExpDesign)) {
    switch(input$updateDesign_chooseExpDesign,
           FlatDesign = {
               if ("Bio.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
               }
               },
           twoLevelsDesign = {
               if ("Tech.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)}
           },
           threeLevelsDesign = {
               if ("Analyt.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)}
           }
    )
  }
  updateDesign_hot
  
})




#------------------------------------------------------------------------------

output$updateDesign_UI_hierarchicalExp <- renderUI({
  req(rv$updateDesign_conditionsChecked)

  if (!isTRUE(rv$updateDesign_conditionsChecked$valid)){return(NULL)}

  tagList(
    tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;",
                  tags$b("2 - Choose the type of experimental design and complete it accordingly")
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
        tags$button(id="updateDesign_btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
      )
    ),

    radioButtons("updateDesign_chooseExpDesign", "",
                 choices = c("Flat design (automatic)" = "FlatDesign" ,
                             "2 levels (complete Bio.Rep column)" = "twoLevelsDesign" ,
                             "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                 selected=character(0))
  )


  })

callModule(moduleDesignExample,"updateDesignExampleTwo", 2)

callModule(moduleDesignExample,"updateDesignExampleThree", 3)



output$updateDesign_designExamples <- renderUI({

  switch(input$updateDesign_chooseExpDesign,
         FlatDesign = h4("No example"),
         twoLevelsDesign = moduleDesignExample2UI("updateDesignExampleTwo"),
         threeLevelsDesign = moduleDesignExample3UI("updateDesignExampleThree")
         )
})



observe({
  shinyjs::onclick("updateDesign_btn_helpDesign",{
    shinyjs::toggle(id = "updateDesign_exLevels", anim = TRUE)}
  )
})



observeEvent(input$updateDesign_chooseExpDesign,{
   # req(input$updateDesign_chooseExpDesign)

  rv$updateDesign_designChecked <- NULL
  n <- nrow(rv$updateDesign_hot)
     switch(input$updateDesign_chooseExpDesign,
         FlatDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],
                                              Bio.Rep = seq(1:n),
                                              stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],Bio.Rep = rep("",n),
                                              Tech.Rep = seq(1:n),
                                              stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],
                                              Bio.Rep = rep("",n),
                                              Tech.Rep = rep("",n),
                                              Analyt.Rep = seq(1:n),
                                              stringsAsFactors = FALSE)
         }
  )



 })


# 

observeEvent(input$updateDesign_btn_checkDesign,{
  rv$updateDesign_designChecked <- DAPAR::check.design(rv$updateDesign_hot)

})


# 
output$updateDesign_checkDesign <- renderUI({
  req(input$updateDesign_chooseExpDesign)
  rv$updateDesign_designChecked
  
  if (!isTRUE(rv$updateDesign_conditionsChecked$valid)){return(NULL)}
  switch(isolate({input$updateDesign_chooseExpDesign}),
         FlatDesign = {},
         twoLevelsDesign = { if (sum(rv$updateDesign_hot$Bio.Rep == "") > 0) {return(NULL)}},
         threeLevelsDesign = {if ((sum(rv$updateDesign_hot$Bio.Rep == "")+sum(rv$updateDesign_hot$Tech.Rep == "")) > 0) {return(NULL)}}
  )
  

  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton("updateDesign_btn_checkDesign", "Check design")
    ),

    tags$div(
      style="display:inline-block;",
      if(!is.null(rv$updateDesign_designChecked)){

        if (isTRUE(rv$updateDesign_designChecked$valid)){
          shinyjs::enable("updateDesign_SaveDesign")
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
          if(!isTRUE(rv$updateDesign_designChecked$valid)){
            shinyjs::disable("updateDesign_SaveDesign")
            tags$p(rv$updateDesign_designChecked$warn)
          } else {
            shinyjs::enable("updateDesign_SaveDesign")
          }
        )
      } else {
        shinyjs::disable("updateDesign_SaveDesign")
      }
    )

  )


})



output$viewNewDesign <- renderUI({
  
  tagList(
    h4("Design"),
    rHandsontableOutput("updateDesign_hot")
  )
})




