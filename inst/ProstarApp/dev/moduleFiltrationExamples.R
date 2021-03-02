# setwd('~/TELETRAVAIL/github_master/Prostar/inst/ProstarApp/')

library(shiny)
library(DT)
library(DAPAR)

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) # to display NAs in DT, instead of blank square

gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "WholeMatrix",
                  "For every condition" = "AllCond",
                  "At least one condition" = "AtLeastOneCond")
gFilterEmptyLines <- gFiltersList[["Empty lines"]]


# create the MSnset dataset to use DAPAR function getListNbValuesInLines
# table example
plop <- read.csv('dev/example_filtration_tab.txt', sep='\t')

metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
metadata_plop$Sample.name <- colnames(plop)
metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
metadata_plop$Bio.Rep <- c(1:6)

plop_msnset <- DAPAR::createMSnset(file = 'dev/example_filtration_tab.txt', indExpData = c(1:6), metadata = metadata_plop)


########################################################################

ui <- fluidPage(
  
  dataTableOutput("buildFiltrationExample"),
  
  selectInput("ChooseFilters","",
                  choices = gFiltersList,
                  width='200px'),
  
  uiOutput("seuilNADelete")
  
  
)


server <- function(input, output, session){
  
  
  
  output$seuilNADelete <- renderUI({
    req(input$ChooseFilters)
    
    if ((input$ChooseFilters=="None") || (input$ChooseFilters==gFilterEmptyLines)) {
      return(NULL)   
    }
    
    tagList(
      shinyjs::useShinyjs(),
      
      uiOutput('keepVal_ui')
    )
  })
  
  
  output$keepVal_ui <- renderUI({
    
    if (input$ChooseFilters %in% c('None', 'Emptylines')) {return(NULL)}
    
    tagList(
      selectInput("seuilNA", NULL,
                  choices =  DAPAR::getListNbValuesInLines(plop_msnset, 
                                                           type=input$ChooseFilters),
                  width='150px')
    )
  })
  
  
  output$buildFiltrationExample <- DT::renderDataTable({
    
    
    index<-reactiveValues(l<-NULL)
    # # table example
    # content <- c(1,rep(NA,10),
    #              rep(1,2),rep(NA,5),rep(1,3),NA,
    #              rep(1,3),rep(NA,4),rep(1,4),
    #              rep(1,4),rep(NA,7),
    #              rep(1,5),rep(NA,2),1,rep(NA,3),
    #              rep(1,6),NA,rep(1,2),NA,1
    # )
    # df <- as.data.frame(matrix(content, nrow=11, ncol=6))
    # colnames(df) <- c("A1","A2","A3","B1","B2","B3")
    # rownames(df) <- c(1:11)
    
    
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
    #################################################################
    switch(input$ChooseFilters,
           emptyLines= {
             index <- reactiveValues(l<-7)
           },
           WholeMatrix= {
             index <- reactiveValues(l<-c(5,6,7,10,11))
           },
           AllCond = { switch(input$seuilNA,
                              # 0 = {
                              #   index <- 
                              # },
                              "1" = {
                                index <- reactiveValues(l<-c(4:7,10))
                              },
                              "2" = {
                                index <- reactiveValues(l<-c(3:7,9,10,11))
                              },
                              "3" = {
                                index <- reactiveValues(l<-c(2:11))
                              },
           )},
           AtLeastOneCond = { switch(input$seuilNA,
                                     # 0 = {
                                     #   index <- 
                                     # },
                                     "1" = {
                                       index <- reactiveValues(l<-7)
                                     },
                                     "2" = {
                                       index <- reactiveValues(l<-c(6,7,11))
                                     },
                                     "3" = {
                                       index <- reactiveValues(l<-c(5:11))
                                     },
           )}
    )
    print("input$ChooseFilters")
    print(input$ChooseFilters)
    print("input$seuilNA")
    print(input$seuilNA)
    print("index")
    print(index)
    #################################################################
    if (!is.null(index()$l)){
      print("ici")
      dt <- datatable(exprs(plop_msnset),
                      options = list(
                        paging = FALSE,
                        searching = FALSE)) %>%
        formatStyle(
          .,
          columns = 1,
          valueColumns = 0,
          target = 'row',
          backgroundColor = styleEqual(index()$l, rep('grey', length(index()$l))) 
        )
      
      dt
    } else {
      print("la")
      dt <- datatable(exprs(plop_msnset),
                      options = list(
                        paging = FALSE,
                        searching = FALSE))
      
      dt
    }

    
    
    
    
  })
}



shinyApp(ui, server)
