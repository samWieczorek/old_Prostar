library(shiny)
library(DT)

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) # to display NAs in DT, instead of blank square


ui <- fluidPage(
  
  dataTableOutput("buildFiltrationExample")
  
)


server <- function(input, output, session){
  
  output$buildFiltrationExample <- DT::renderDataTable({
    
    # table example
    content <- c(1,rep(NA,10),
                 rep(1,2),rep(NA,5),rep(1,3),NA,
                 rep(1,3),rep(NA,4),rep(1,4),
                 rep(1,4),rep(NA,7),
                 rep(1,5),rep(NA,2),1,rep(NA,3),
                 rep(1,6),NA,rep(1,2),NA,1
    )
    df <- as.data.frame(matrix(content, nrow=11, ncol=6))
    colnames(df) <- c("A1","A2","A3","B1","B2","B3")
    rownames(df) <- c(1:11)
    
    
    #    A1 A2 A3 B1 B2 B3
    # 1   1  1  1  1  1  1
    # 2  NA  1  1  1  1  1
    # 3  NA NA  1  1  1  1
    # 4  NA NA NA  1  1  1
    # 5  NA NA NA NA  1  1
    # 6  NA NA NA NA NA  1
    # 7  NA NA NA NA NA NA
    # 8  NA  1  1 NA  1  1
    # 9  NA  1  1 NA NA  1
    # 10 NA  1  1 NA NA NA
    # 11 NA NA  1 NA NA  1
    
    # Indices for filtered rows, to darken:
    # paramtype<-c("None", "EmptyLines", "WholeMatrix", "AllCond", "AtLeastOneCond")
    # "l_none", nothing to darken, keep all row
    # l_emptyLines <- 7
    # l_wholeMatrix <- list("th1"=c(7),"th2"=c(6,7),"th3"=c(5:7,10,11),"th4"=c(4:7,9,10,11),"th5"=c(3:12),"th6"=c(2:11))
    # l_allCond <-list("th1"=c(4:7,10),"th2"=c(3:7,9,10,11),"th3"=c(2:11))
    # l_atLeastOneCondition <- list("th1"=c(7),"th2"=c(6,7,11),"th3"=c(5:11))
    
    # Example with method=WholeMatrix/th=3 <=> at least 3 quanti values by entire row
    # index <- c(5,6,7,10,11)
    
    
    
    dt <- datatable(df,
                    options = list(
                      paging = FALSE,
                      searching = FALSE)) %>%
      formatStyle( 
        .,
        columns = 1,
        valueColumns = 0,
        target = 'row',
        backgroundColor = styleEqual(index, rep('grey', length(index))) 
      )
    
    
    dt
    
  })
}



shinyApp(ui, server)
