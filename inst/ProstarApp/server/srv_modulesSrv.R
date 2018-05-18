
#################### MODULES DEFINITION #################################




modulePopover <- function(input, output, session, data){
    
    ns <- session$ns
    
    output$customPopover <- renderUI({

        #ns <- session$ns
        div(
            div(
                # edit1
                style="display:inline-block; vertical-align: middle;",
                h4(data()$title)
            ),
            div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip"),
            bsPopover(id = ns("q1"), title = "",
                      content = data()$content,
                      placement = "right", 
                      trigger = "click", 
                      options = list(container = "body")
            )
        )
        )
    })
}




moduleLegendColoredExprs <- function(input, output, session){}



moduleVolcanoplot <- function(input, output, session){
  output$nbSelectedItems <- renderUI({ selectedItems()  })
  output$selectTooltipInfo <- renderUI({ tooltipInfo()})
  output$Infos <- renderDataTable({ tableInfos() })
  output$volcanoPlot <-  renderHighchart({ volcanoplot_rCharts()})
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
