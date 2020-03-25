moduleDetQuantImpValuesUI <- function(id){
  ns <- NS(id)
  tagList(
    h5("The missing values will be imputed by the following values :"),
    
    DT::dataTableOutput(ns("detQuantValues_DT"))
  )
}


moduleDetQuantImpValues <- function(input, output, session, dataIn, quant,factor)
{
  
  output$detQuantValues_DT <- renderDataTable({
    req(dataIn(), quant(), factor())
    
    values <- getQuantile4Imp(Biobase::exprs(dataIn()), quant()/100, factor())
    DT::datatable(as.data.frame(t(values$shiftedImpVal)),
                  rownames = FALSE,
                  options = list(initComplete = initComplete(),
                                 dom = 't',
                                 bLengthChange = FALSE))
  })
}

