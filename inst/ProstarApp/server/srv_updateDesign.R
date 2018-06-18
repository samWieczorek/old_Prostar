


observe({
    input$updateDesign_chooseExpDesign
    stacktrace <- reactlog::traceInvalidation()
    dependencies <- reactlog::listDependencies()

})




output$updateDesignScreen <- renderUI({
   
  tagList(
    fluidRow(
      column(width=6, tagList(
          tags$div(
              tags$div(style="display:inline-block;",actionButton("updateDesign_btn_checkConds", "1 - Check conditions (Label)")),
              tags$div(style="display:inline-block;",uiOutput("updateDesign_UI_checkConditions_Icon"))),
          
        uiOutput("updateDesign_checkDesign"),
        uiOutput("updateDesign_SaveDesign"),
        uiOutput("designUpdated")
        )),
      column(width=6, uiOutput("updateDesign_UI_hierarchicalExp"))
    ),
    hr(),
    

    tags$div(
      tags$div(style="display:inline-block;",
               tagList(
                   h4("Current design"),
                   rHandsontableOutput("updateDesign_currentDesign")
               )
      ),
      tags$div(style="display:inline-block;",
               tagList(
                   #actionButton("updateDesign_btn_resetDesign", "Reset design"),
                   uiOutput("viewNewDesign",width="100%")
               )
      ),
      tags$div(style="display:inline-block;",
               shinyjs::hidden(
                div(id = "updateDesign_exLevels",uiOutput("updateDesign_designExamples")))
      )
      
     
    )
  )
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
  rownames(tmp) <- tmp$Experiment
  if (is.character(rownames(tmp))){
      rownames(tmp) <- gsub(".", "_", rownames(tmp), fixed=TRUE)
      
  }
  Biobase::pData(rv$current.obj) <- tmp

  rv$updateDesign_designSaved <- TRUE
  shinyjs::disable("updateDesign_btn_checkConds")
  shinyjs::disable("updateDesign_btn_checkDesign")
  shinyjs::disable("btn_SaveDesign")
  shinyjs::disable("updateDesign_chooseExpDesign")


})


#############################################################
observe({
  req(input$updateDesign_hot)
  rv$updateDesign_hot <-  hot_to_r(input$updateDesign_hot)

})



 output$designUpdated <- renderUI({
  req(rv$updateDesign_designSaved)

  if(isTRUE(rv$updateDesign_designSaved)){
    h4("The design has been updated")
  }
})

#############################################################
output$updateDesign_SaveDesign <- renderUI({
  req(rv$updateDesign_designChecked)
  rv$updateDesign_designSaved

  if (!isTRUE(rv$updateDesign_designChecked$valid)){return(NULL)}
  if (isTRUE(rv$updateDesign_designSaved)){return(NULL)}

      tags$div(
          tags$div(style="display:inline-block;",
                    actionButton("btn_SaveDesign","3 - Save design")
          ),

          tags$div(style="display:inline-block;",
        if(isTRUE(rv$updateDesign_designSaved)){
          tags$div(
            tags$div(style="display:inline-block;",tags$img(src = "images/Ok.png", height=25)),
            tags$div(style="display:inline-block;",tags$p("Design updated"))
          )
        }
    )
)

})

#############################################################
updateDesign_color_renderer_currentDesign <- reactive({
  conds <- Biobase::pData(rv$current.obj)$Label
  pal <- brewer.pal(length(unique(conds)),"Dark2")
  
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



updateDesign_color_renderer_NewDesign <- reactive({
  conds <- rv$updateDesign_hot$Label
  pal <- brewer.pal(length(unique(conds)),"Dark2")
  
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
  
  rv$updateDesign_newOrder <- order(rv$updateDesign_hot["Label"])
  rv$updateDesign_hot <- rv$updateDesign_hot[rv$updateDesign_newOrder,]
  rv$updateDesign_conditionsChecked <- DAPAR::check.conditions(rv$updateDesign_hot$Label)
  hideTab(inputId ="navPage", target = "open")

  
})


#-------------------------------------------------------------
output$updateDesign_hot <- renderRHandsontable({
  #req(rv$current.obj)
  rv$updateDesign_hot
  #input$updateDesign_chooseExpDesign
  
  
    if (is.null(rv$updateDesign_hot)){
        rv$updateDesign_hot  <- data.frame(Experiment = as.character(Biobase::pData(rv$current.obj)$Experiment),
                                           Label = Biobase::pData(rv$current.obj)$Label,
                                           stringsAsFactors = FALSE)
    }
  
  updateDesign_hot <-
    rhandsontable(rv$updateDesign_hot,rowHeaders=NULL, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                                         maxRows=nrow(rv$updateDesign_hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,
                     allowInsertColumn = FALSE,
                     allowRemoveRow = TRUE,
                     allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(renderer = updateDesign_color_renderer_NewDesign()) %>%
    hot_col(col = "Experiment", readOnly = TRUE)
  
  if (!is.null(input$updateDesign_chooseExpDesign)) {
    switch(input$updateDesign_chooseExpDesign,
           FlatDesign = {
               if ("Bio.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% hot_col(col = "Bio.Rep", readOnly = TRUE)
               }
               },
           twoLevelsDesign = {
               if ("Tech.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% hot_col(col =  "Tech.Rep", readOnly = TRUE)}
           },
           threeLevelsDesign = {
               if ("Analyt.Rep" %in% colnames(updateDesign_hot)){
                   updateDesign_hot <- updateDesign_hot %>% hot_col(col = "Analyt.Rep", readOnly = TRUE)}
           }
    )
  }
  updateDesign_hot
  
})





output$updateDesign_UI_checkConditions_Icon  <- renderUI({
    req(rv$updateDesign_conditionsChecked)
    
     if (isTRUE(rv$updateDesign_conditionsChecked$valid)){
        tagList(
            tags$div(
                tags$div(style="display:inline-block;",tags$img(src = "images/Ok.png", height=25)),
                 tags$div(style="display:inline-block;",tags$p("Correct conditions"))
                 ))
    } else {
         tagList(
            tags$div(
                tags$div(style="display:inline-block;",tags$img(src = "images/Problem.png", height=25)),
                 tags$div(style="display:inline-block;",tags$p("Invalid conditions"))
                ),
                tags$p(rv$updateDesign_conditionsChecked$warn))
    }

})



output$updateDesign_UI_hierarchicalExp <- renderUI({
  req(rv$updateDesign_conditionsChecked)

  if (!isTRUE(rv$updateDesign_conditionsChecked$valid)){return(NULL)}

  tagList(
    tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;",
        HTML("<strong><font size=\"5\">Experimental design</font></strong>")
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
        tags$button(id="updateDesign_btn_helpDesign", tags$sup("[?]"), class="Prostar_tooltip")
      )
    ),

    radioButtons("updateDesign_chooseExpDesign", "",
                 choices = c("Flat design" = "FlatDesign" ,
                             "2 levels design" = "twoLevelsDesign" ,
                             "3 levels design" = "threeLevelsDesign" ))
  )


  })



output$updateDesign_designExamples <- renderUI({

  switch(input$updateDesign_chooseExpDesign,
         FlatDesign = {},
         twoLevelsDesign = tagList(
             h4("Example for a 2-levels design"),
             rHandsontableOutput("updateDesign_twolevelsExample")
         ),
         threeLevelsDesign = tagList(
             h4("Example for a 3-levels design"),
             rHandsontableOutput("updateDesign_threelevelsExample")
         )
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

     switch(input$updateDesign_chooseExpDesign,
         FlatDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],
                                              Bio.Rep = seq(1:nrow(rv$updateDesign_hot)),
                                              stringsAsFactors = FALSE)
         },
         twoLevelsDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],Bio.Rep = rep("",nrow(rv$updateDesign_hot)),
                                              Tech.Rep = seq(1:nrow(rv$updateDesign_hot)),
                                              stringsAsFactors = FALSE)
         },
         threeLevelsDesign = {
           rv$updateDesign_hot  <- data.frame(rv$updateDesign_hot[,1:2],
                                              Bio.Rep = rep("",nrow(rv$updateDesign_hot)),
                                              Tech.Rep = rep("",nrow(rv$updateDesign_hot)),
                                              Analyt.Rep = seq(1:nrow(rv$updateDesign_hot)),
                                              stringsAsFactors = FALSE)
         }
  )



 })


# 
output$updateDesign_twolevelsExample <- renderRHandsontable({

  df <- data.frame(Experiment= paste0("Sample ",as.character(1:14)),
                   Label = c(rep( "A", 4), rep("B", 4), rep("C", 6)),
                   Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                   Tech.Rep = c(1:14),
                   stringsAsFactors = FALSE)


  pal <- brewer.pal(3,"Dark2")

  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

  if(col==1 && (row>=0 && row<=3)) {td.style.background = '#1B9E77';}
  if(col==1 && (row>=4 && row<=7)) {td.style.background = '#D95F02';}
  if(col==1 && (row>=8 && row<=14)) {td.style.background = '#7570B3';}


  if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13))
  {td.style.background = 'lightgrey';}

  if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12))
  {td.style.background = 'lightgrey';}
}"

  rhandsontable(df,rowHeaders=NULL, fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                      maxRows=nrow(rv$updateDesign_hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)

  })



output$updateDesign_threelevelsExample <- renderRHandsontable({

  df <- data.frame(Experiment= paste0("Sample ",as.character(1:16)),
                   Label = c(rep( "A", 8), rep("B", 8)),
                   Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                   Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                   Analyt.Rep = c(1:16),
                   stringsAsFactors = FALSE)


  pal <- brewer.pal(2,"Dark2")

  color_rend <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);

  if(col==1 && (row>=0 && row<=7))
  {td.style.background = '#1B9E77';}

  if(col==1 && (row>=8 && row<=15))
  {td.style.background = '#D95F02';}

  if(col==2 && (row==0||row==1||row==2||row==3||
  row==8||row==9||row==10||row==11))
  {td.style.background = 'lightgrey';}

  if(col==3 && (row==0||row==1||row==4||row==5||
  row==8||row==9||row==12||row==13))
  {td.style.background = 'lightgrey';}


  if(col==4 && (row==0||row==2||row==4||row==6||
  row==8||row==10||row==12||row==14))
  {td.style.background = 'lightgrey';}
}"
 rhandsontable(df,rowHeaders=NULL,fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                    maxRows=nrow(rv$updateDesign_hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,allowInsertColumn = FALSE,
                     allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(readOnly = TRUE,renderer = color_rend)

  })






observeEvent(input$updateDesign_btn_checkDesign,{
  #rv$updateDesign_hot

  rv$updateDesign_designChecked <- DAPAR::check.design(rv$updateDesign_hot)

})


# 
output$updateDesign_checkDesign <- renderUI({
  #req(input$updateDesign_chooseExpDesign)
 # req(rv$updateDesign_hot)
  rv$updateDesign_designChecked
  req(rv$updateDesign_conditionsChecked)

  if (!isTRUE(rv$updateDesign_conditionsChecked$valid)){return(NULL)}


  tags$div(
    tags$div(
      style="display:inline-block;",
      actionButton("updateDesign_btn_checkDesign", "2 - Check design")
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


#-------------------------------------------------------------
output$updateDesign_currentDesign <-renderRHandsontable({
  req(rv$current.obj)
  rhandsontable(Biobase::pData(rv$current.obj),
                rowHeaders=NULL, 
                fillHandle = list(direction='vertical', 
                                  autoInsertRow=FALSE,
                                  maxRows=nrow(rv$updateDesign_hot))) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                     allowInsertRow = FALSE,
                     allowInsertColumn = FALSE,
                     allowRemoveRow = TRUE,
                     allowRemoveColumn = FALSE,
                     autoInsertRow=FALSE     ) %>%
    hot_cols(renderer = updateDesign_color_renderer_currentDesign(),readOnly = TRUE) 
})



output$viewNewDesign <- renderUI({
  if (isTRUE(rv$updateDesign_designSaved)){return(NULL)}
  
  tagList(
    h4("New design"),
    rHandsontableOutput("updateDesign_hot")
  )
})




