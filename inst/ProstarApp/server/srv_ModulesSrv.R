


#################### MODULES DEFINITION #################################
module_Not_a_numeric <- function(input, output, session, n){
  
  output$msg_not_numeric <- renderUI({
    n()
    if (is.na(as.numeric(n()))){
      tags$p("Please choose a number")
    }
  })
}



moduleDesignExample <- function(input, output, session, n){
  
  
  output$nlevelsExample <- renderRHandsontable({
    
    
    if (n == 2){
      df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                       Condition = c(rep("A", 4), rep("B", 4), rep("C", 6)),
                       Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                       Tech.Rep = c(1:14),
                       stringsAsFactors = FALSE)
      
      
      pal <- DAPAR::ExtendPalette(3, listBrewerPalettes[1])
      color_rend <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.TextRenderer.apply(this, arguments);
                         
                         if(col==1 && (row>=0 && row<=3)) {td.style.background = '",pal[1], "';}
                         if(col==1 && (row>=4 && row<=7)) {td.style.background = '",pal[2], "';}
                         if(col==1 && (row>=8 && row<=14)) {td.style.background = '",pal[3], "';}
                         
                         
                         if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13)) 
                         {td.style.background = 'lightgrey';}
                         
                         if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12)) 
                         {td.style.background = 'lightgrey';}
                    }")
      
    } else if (n == 3){
      df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                       Condition = c(rep( "A", 8), rep("B", 8)),
                       Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                       Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                       Analyt.Rep = c(1:16),
                       stringsAsFactors = FALSE)
      
      
      pal <-DAPAR::ExtendPalette(3, listBrewerPalettes[1])
      
      color_rend <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           
                           if(col==1 && (row>=0 && row<=7)) {td.style.background = '",pal[1], "';}
                           
                           if(col==1 && (row>=8 && row<=15))  {td.style.background = '",pal[2], "';}
                           
                           if(col==2 && (row==0||row==1||row==2||row==3||row==8||row==9||row==10||row==11)) 
                           {td.style.background = 'lightgrey';}
                           
                           if(col==3 && (row==0||row==1||row==4||row==5|| row==8||row==9||row==12||row==13)) 
                           {td.style.background = 'lightgrey';}
                           
                           
                           if(col==4 && (row==0||row==2||row==4||row==6|| row==8||row==10||row==12||row==14)) 
                           {td.style.background = 'lightgrey';}
                            }")
      
    }
    
    rhandsontable::rhandsontable(df,rowHeaders=NULL,fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                                      maxRows=nrow(rv$hot))) %>%
      rhandsontable::hot_rows(rowHeights = 30) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                                      allowInsertRow = FALSE,allowInsertColumn = FALSE,
                                      allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                                      autoInsertRow=FALSE     ) %>%
      rhandsontable::hot_cols(readOnly = TRUE,renderer = color_rend)
    
    
    
  })
  
  
}






moduleDetQuantImpValues <- function(input, output, session, quant,factor) {
  
  output$detQuantValues_DT <- renderDataTable(server=TRUE,{
    req(rv$current.obj, quant(), factor())
    
    values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), quant()/100, factor())
    DT::datatable(as.data.frame(t(values$shiftedImpVal)),
                  rownames = FALSE,
                  options = list(initComplete = initComplete(),
                                 dom = 't',
                                 bLengthChange = FALSE))
    
  })
}








#------------------------------------------------------------



#------------------------------------------------------------
moduleDensityplot <- function(input, output, session, data) {
  
  #outputOptions(output, 'Densityplot', suspendWhenHidden=FALSE)
  
  output$Densityplot <- renderHighchart({
    #req(rv$current.obj)
    data()
     rv$PlotParams$paletteForConditions
    rv$PlotParams$legendForSamples
    tmp <- NULL
    isolate({
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(GetCurrentObjName(),".densityplot")
        tmp <- DAPAR::densityPlotD_HC(data(), 
                                      legend = rv$PlotParams$legendForSamples,
                                      pal = rv$PlotParams$paletteForConditions)
        future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
      })
    })
    tmp
  })
}


#------------------------------------------------------------
moduleBoxplot <- function(input, output, session, data, pal) {
  
  observeEvent(input$choosePlot, {
    switch(input$choosePlot,
           boxplot={
             shinyjs::hide('viewViolinPlot')
             shinyjs::show('BoxPlot')
           },
           violinplot={
             shinyjs::hide('BoxPlot')
             shinyjs::show('viewViolinPlot')
           }
    )
  })
  
  
  output$BoxPlot <- renderHighchart({
    #req(rv$current.obj)
    data()
    rv$current.obj.name
    rv$PlotParams$paletteForConditions
    rv$PlotParams$legendForSamples

   tmp <- NULL
    isolate({
      pattern <- paste0(GetCurrentObjName(),".boxplot")
      tmp <- boxPlotD_HC(data(), 
                         conds = Biobase::pData(data())$Condition,
                         legend = rv$PlotParams$legendForSamples, 
                         pal = pal())
      #future(createPNGFromWidget(tmp,pattern))
      
      
    })
    tmp
  })
  
  output$viewViolinPlot<- renderImage({
    #req(rv$current.obj)
    data()
    rv$PlotParams$legendForSamples
    rv$PlotParams$paletteForConditions
    tmp <- NULL
    
    isolate({
      
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')
      
      # Generate a png
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      pattern <- paste0(GetCurrentObjName(),".violinplot")
      tmp <- DAPAR::violinPlotD(data(), 
                                conds = Biobase::pData(data())$Condition,
                                legend = rv$PlotParams$legendForSamples, 
                                pal = pal()
      )
      #future(createPNGFromWidget(tmp,pattern))
      dev.off()
    })
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  
}



moduleMVPlots <- function(input, output, session, data, title, pal, pattern) {
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(data())

    hc_mvTypePlot2(obj=data(), 
                   pattern = pattern,
                   title=title(), 
                   pal = pal()
                   )
  })
  
  
  
  
  output$WarnForImageNA <- renderUI({
    
    tryCatch(
      {
        wrapper.mvImage(data())
      },
      warning = function(w) { p(conditionMessage(w))},
      error = function(e) {p(conditionMessage(e))},
      finally = {
        #cleanup-code 
      })
    
  })
  
  output$plot_showImageNA <- renderImage({
    #req(wrapper.mvImage(data()))
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    png(outfile)
    wrapper.mvImage(data())
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
}


moduleFilterStringbasedOptions <- function(input, output, session) {
  
  output$FilterStringbasedOptions <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    
    tagList(
      h4("String based filtering options")
      ,hr()
      ,h4("Filter contaminants"),
      uiOutput("id_Contaminants"),
      uiOutput("choosePrefixContaminants"),
      br(),
      h4("Filter reverse"),
      uiOutput("id_Reverse"),
      uiOutput("choosePrefixReverse"),
      br(),
      #actionButton("resetFilterParamsButton","Reset parameters"),
      actionButton("performFilteringContaminants",
                   "Perform string-based filtering", class = actionBtnClass)
    )
    
    
    
    
    
    
  })
}







moduleInsertMarkdown <- function(input, output, session,url){
  
  ns <- session$ns
  output$insertMD <- renderUI({
    print(url)
    tryCatch(
      {
        includeMarkdown(url)
      }
      , warning = function(w) {
        #conditionMessage(w)
        tags$p("URL not found. Please check your internet connection.")
        #shinyjs::info(paste("URL not found",":",conditionMessage(w), sep=" "))
      }, error = function(e) {
        shinyjs::info(paste("Error :","in moduleInsertMarkdown",":", conditionMessage(e), sep=" "))
      }, finally = {
        #cleanup-code 
      })
    
  })
  
}
