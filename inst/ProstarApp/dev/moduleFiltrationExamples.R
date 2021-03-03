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


# table example
plop <- read.csv('dev/example_filtration_tab.txt', sep='\t')
# create the MSnset dataset to use DAPAR function getListNbValuesInLines
metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
metadata_plop$Sample.name <- colnames(plop)
metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
metadata_plop$Bio.Rep <- c(1:6)
plop_msnset <- DAPAR::createMSnset(file = 'dev/example_filtration_tab.txt', indExpData = c(1:6), metadata = metadata_plop)


########################################################################

ui <- fluidPage(
  
  fluidRow(column(4,selectInput("ChooseFilters","",
                                choices = gFiltersList,
                                selected = "None",
                                width='200px')),
           column(8,textOutput("methodInformation"))),
  
  uiOutput("seuilNADelete"),
  
  dataTableOutput("buildFiltrationExample")
  
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
                  selected = "0",
                  width='150px')
    )
  })
  
  
  output$methodInformation <- renderText({
    switch(input$ChooseFilters,
           None = {
             txt <- "No lines are removed"
           },
           EmptyLines = {
             txt <- "Lines with only NAs are removed."
           },
           WholeMatrix = { 
             txt <- paste("Lines containing at least",
                          input$seuilNA,
                          "quantitative values per row are kept.")
           },
           AllCond = {
             txt <- paste("Lines containing at least",
                          input$seuilNA,
                          "quantitative values per row, in all condition, are kept.")
           },
           AtLeastOneCond = {
             txt <- paste("Lines containing at least",
                          input$seuilNA,
                          "quantitative values per row, in at least one condition, are kept.")
           }
    )
    txt
  })
  
  output$buildFiltrationExample <- DT::renderDataTable({
    ################################################################
    # A1 A2 A3 B1 B2 B3
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
    # none: nothing to darken, keep all row
    # emptyLines: 7
    # wholeMatrix: list("th1"=c(7),"th2"=c(6,7),"th3"=c(5:7,10,11),"th4"=c(4:7,9,10,11),"th5"=c(3:11),"th6"=c(2:11))
    # allCond: list("th1"=c(4:7,10),"th2"=c(3:7,9,10,11),"th3"=c(2:11))
    # atLeastOneCondition: list("th1"=c(7),"th2"=c(6,7,11),"th3"=c(5:11))
    
    # Example with method=WholeMatrix/th=3 <=> at least 3 quanti values by entire row
    # index <- c(5,6,7,10,11)
    ################################################################
    switch(input$ChooseFilters,
           None = {
             index <- NULL
           },
           EmptyLines = {
             index <- 7
           },
           WholeMatrix = { switch(input$seuilNA,
                                  "0" = { index <- NULL },
                                  "1" = { index <- 7 },
                                  "2" = { index <- c(6,7) },
                                  "3" = { index <- c(5:7,10,11) },
                                  "4" = { index <- c(4:7,9,10,11) },
                                  "5" = { index <- c(3:11)},
                                  "6" = { index <- c(2:11)}
           )},
           AllCond = { switch(input$seuilNA,
                              "0" = { index <- NULL },
                              "1" = { index <- c(4:7,10) },
                              "2" = { index <- c(3:7,9,10,11) },
                              "3" = { index <- c(2:11) }
           )},
           AtLeastOneCond = { switch(input$seuilNA,
                                     "0" = { index <- NULL },
                                     "1" = { index <- 7 },
                                     "2" = { index <- c(6,7,11) },
                                     "3" = { index <- c(5:11) }
           )}
    )
    
    dt <- plop
    
    if (!is.null(index)){
      datatable(dt,
                options = list(
                  paging = FALSE,
                  searching = FALSE)) %>%
        formatStyle(
          .,
          columns = 1,
          valueColumns = 0,
          target = 'row',
          backgroundColor = styleEqual(index, rep('grey', length(index)) )
        )
    } else {
      datatable(dt,
                options = list(
                  paging = FALSE,
                  searching = FALSE))
    }
  })
  
}


shinyApp(ui, server)


