moduleDesignExampleUI <- function(id){
  ns <- NS(id)
  tagList(
    rHandsontableOutput(ns("nlevelsExample"))
  )
  
}



moduleDesignExample <- function(input, output, session, n, n_rows){
  ns <- session$ns
  
  
  output$nlevelsExample <- rhandsontable::renderRHandsontable({
    
    
    if (n() == 2){
      df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                       Condition = c(rep("A", 4), rep("B", 4), rep("C", 6)),
                       Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                       Tech.Rep = c(1:14),
                       stringsAsFactors = FALSE)
      
      
      pal <- RColorBrewer::brewer.pal(3,listBrewerPalettes[1])
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

  } else if (n() == 3){
    df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                     Condition = c(rep( "A", 8), rep("B", 8)),
                     Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                     Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                     Analyt.Rep = c(1:16),
                     stringsAsFactors = FALSE)
    
    
    pal <- RColorBrewer::brewer.pal(3,listBrewerPalettes[1])[1:2]
    
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
                                                                      maxRows=n_rows())) %>%
      rhandsontable::hot_rows(rowHeights = 30) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                                      allowInsertRow = FALSE,allowInsertColumn = FALSE,
                                      allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                                      autoInsertRow=FALSE     ) %>%
      rhandsontable::hot_cols(readOnly = TRUE,renderer = color_rend)
    
    
    
})
  
  
  }
