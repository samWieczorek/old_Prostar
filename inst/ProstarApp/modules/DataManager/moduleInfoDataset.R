moduleInfoDatasetUI <- function(id){
  ns <- NS(id)
  
  
  uiOutput(ns("info"))
  
}

  
  
moduleInfoDataset <-function(input, output, session, obj){
  ns <- session$ns
  
  
  
  
  output$info <- renderUI({
    req(obj())
    
    
    print('IN INFOOOOO')
    typeOfDataset <- obj()@experimentData@other$typeOfData
    
    if (NeedsUpdate())
    {    
      tags$div(
        tags$div(style="display:inline-block; vertical-align: top;",
                 tags$img(src = "images/Problem.png", height=25)),
        tags$div(style="display:inline-block; vertical-align: top;",
                 HTML("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
                      software functionalities. Please update the design below"))
                 )
    } else{
      
      NA.count <- length(which(is.na(Biobase::exprs(obj()))))
      nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(obj()))), 1, all))
      tagList(
        tags$h3("Info"),
        if (typeOfDataset == "protein"){
          tags$p("Note: the aggregation tool
                 has been disabled because the dataset contains 
                 protein quantitative data.")
        },
        
        if (NA.count > 0){
          tags$p("As your dataset contains missing values, you should 
                 impute them prior to proceed",br()," 
                 to the differential analysis.")
        },
        if (nb.empty.lines > 0){
          tags$p("As your dataset contains lines with no values, you 
                 should remove them with the filter",br()," tool
                 prior to proceed to the analysis of the data.")
        }
        
          )
      
        }
        })

  
  
  
  ########################################################### 
  NeedsUpdate <- reactive({
    req(obj())
    
    PROSTAR.version <- obj()@experimentData@other$Prostar_Version
    
    if (!is.null(PROSTAR.version) && (compareVersion(PROSTAR.version,"1.12.9") != -1)
        && (DAPAR::check.design(Biobase::pData(obj()))$valid))
    {return (FALSE)}
    
    else {
      return(TRUE)
    }
  })

}



