
#################### MODULES DEFINITION #################################

# moduleSymbolicFilter <- function(input, output, session, choice) {
#     
# 
#     output$SymbolicFilter <- renderUI({
#        # req(rv$current.obj)
#         #data <- c("",colnames(Biobase::fData(rv$current.obj)))
#         fluidRow(
#           column(width = 4, selectInput(session$ns("cname"), "Column name", choices = choice())),
#           column(width = 4, textInput(session$ns("tagName"), "Tag", value = ""))
#         )
#     })
#     
#     output$SymFilterTag <- renderText({
#         "toto"
#     })
# }




moduleVolcanoplot <- function(input, output, session){
  
  
  # output$nbSelectedItems <- renderUI({
  #   rv$seuilLogFC
  #   input$diffAnaMethod
  #   req(rv$current.obj)
  #   rv$resAnaDiff
  #   
  #   if(is.null(rv$resAnaDiff)|| is.null(rv$resAnaDiff$FC) || is.null(rv$resAnaDiff$P_Value)){return()}
  #   if (is.null( input$diffAnaMethod) || (input$diffAnaMethod == G_noneStr)){
  #     return()}
  #   if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
  #     return()}
  #   
  #   p <- NULL
  #   p <- rv$resAnaDiff
  #   upItemsPVal <- NULL
  #   upItemsLogFC <- NULL
  #   
  #   
  #   upItemsLogFC <- which(abs(p$FC) >= rv$seuilLogFC)
  #   rv$nbTotalAnaDiff_Step1 <- nrow(Biobase::exprs(rv$current.obj))
  #   rv$nbSelectedAnaDiff_Step1 <- NULL
  #   t <- NULL
  #   
  #   t <- upItemsLogFC
  #   
  #   rv$nbSelectedAnaDiff_Step1 <- length(t)
  #   
  #   txt <- paste("Total number of ",rv$typeOfDataset, "(s) = ", 
  #                rv$nbTotalAnaDiff_Step1,"<br>",
  #                "Number of selected ",rv$typeOfDataset, "(s) = ", 
  #                rv$nbSelectedAnaDiff_Step1,"<br>",
  #                "Number of non selected ",rv$typeOfDataset, "(s) = ", 
  #                (rv$nbTotalAnaDiff_Step1 -rv$nbSelectedAnaDiff_Step1), sep="")
  #   HTML(txt)
  #   
  # })
  
  
  
  # output$selectTooltipInfo <- renderUI({
  #   rv$current.obj
  #   input$selectComparison
  #   if (is.null(rv$current.obj)){return()}
  #   if (is.null(input$selectComparison) || (input$selectComparison=="None")){return()}
  #   
  #   #selectInput("tooltipInfo", "Select the info you want to see", choices = colnames(fData(rv$current.obj)))
  #   selectizeInput("tooltipInfo",
  #                  label = "Select the info you want to see",
  #                  choices = colnames(fData(rv$current.obj)),
  #                  multiple = TRUE, width='500px')
  # })
  # 
  
  output$Infos <- renderDataTable({
    rv$current.obj
    input$eventPointClicked
    
    if (is.null(input$eventPointClicked)){return()}
    if (is.null(rv$current.obj)){return()}
    
    data <-getDataInfosVolcano()
    
    #id <-  which(is.na(data))
    if (length(data$value) == 0){
      dat <- DT::datatable(data$value, 
                           options=list(dom='t',ordering=F))
    } else {
      
      colorCode <- paste("function(row, data) {",
                         paste(sapply(1:length(data$value),function(i)
                           paste( "$(this.api().cell(0,",i,").node()).css({'background-color':'",data$color[i],"'});")
                         ),collapse = "\n"),"}" )
      
      
      dat <- DT::datatable(data$value, 
                           options=list(dom='t',
                                        ordering=F
                                        ,drawCallback=JS(colorCode)
                                        ,server = TRUE))
    }
    dat
    
  })
  
  output$volcanoPlot <-  renderHighchart({
    volcanoplot_rCharts()
  })
  
  
  
}



missingValuesPlots <- function(input, output, session) {
    
    output$histo_MV <- renderHighchart({
        histo_MV()
    })
    
    output$histo_MV_per_lines <- renderHighchart({
        histo_MV_per_lines()
    })
    
    output$histo_MV_per_lines_per_conditions <- renderHighchart({
        histo_MV_per_lines_per_conditions()
    })
}

moduleDensityplot <- function(input, output, session) {
    
    output$Densityplot <- renderHighchart({
        DensityPlot()
    })
}

moduleBoxplot <- function(input, output, session) {
    
    output$BoxPlot <- renderPlot({
        BoxPlot()
    })
}




moduleMVPlots <- function(input, output, session, data) {
    
    output$plot_viewNAbyMean <- renderHighchart({
        viewNAbyMean(data())
    })
    
    output$plot_showImageNA <- renderPlot({
        showImageNA(data())
    })
}

# moduleViewNAbyMean <- function(input, output, session) {
#     
#     output$viewNAbyMean <- renderPlot({
#         viewNAbyMean()
#     })
# }
# 



# moduleShowImageNA <- function(input, output, session) {
#     
#     output$showImageNA <- renderPlot({
#         showImageNA()
#     })
# }



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
                         "Perform string-based filtering")
        )
        
        
        
        
        
        
    })
}




moduleDatasetOverview <- function(input, output, session) {
    
    output$DatasetOverview <- renderUI({
        rv$current.obj
        if (is.null(rv$current.obj)) {return(NULL)}
        
        isolate({
            
            verb <- NULL
            plurial <- NULL
            
            
            if( dim(Biobase::exprs(rv$current.obj))[2] > 1){
                verb <- "are"
                plurial <- "s"} else {
                    verb <- "is"
                    plurial <- ""}
            
            
            
            txt1 <- paste("There ", verb, " " ,
                          dim(Biobase::exprs(rv$current.obj))[2],
                          " sample", plurial, " in your data.", sep="")
            
            if( dim(Biobase::exprs(rv$current.obj))[2] > 1){
                verb <- "are"
                plurial <- "s"} else {
                    verb <- "is"
                    plurial <- ""}
            txt2 <- paste("There ", verb, " ",
                          dim(Biobase::exprs(rv$current.obj))[1], 
                          " line", plurial, " in your data.", sep="")
            
            NA.count<-apply(data.frame(Biobase::exprs(rv$current.obj)), 
                            2, 
                            function(x) length(which(is.na(data.frame(x))==TRUE)) )
            pourcentage <- 100 * round(sum(NA.count)/
                                           (dim(Biobase::exprs(rv$current.obj))[1]*
                                                dim(Biobase::exprs(rv$current.obj))[2]), digits=4)
            txt3 <- paste("Percentage of missing values:",pourcentage , "%.")
            
            nb.empty.lines <- sum(apply(
                is.na(as.matrix(Biobase::exprs(rv$current.obj))), 1, all))
            txt4 <- NULL
            if (nb.empty.lines > 0){
                if( nb.empty.lines > 1){
                    verb <- "are"
                    plurial <- "s"} else {
                        verb <- "is"
                        plurial <- ""}
                
                
                txt4 <- paste("There ", verb, " ",
                              nb.empty.lines ," line",
                              plurial," with only NA values."
                              ,sep="")
            }
            
            tags$ul(
                tags$li(txt1), 
                tags$li(txt2), 
                tags$li(txt3),
                if (!is.null(txt4)){tags$li(txt4)}
            )
            
        })
        
      
    })
}
