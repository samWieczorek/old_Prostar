library(shiny)
library(DT)
library(DAPAR)
library(shinyBS)
library(shinyjqui)

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) # to display NAs in DT, instead of blank square

gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "WholeMatrix",
                  "For every condition" = "AllCond",
                  "At least one condition" = "AtLeastOneCond")
gFilterEmptyLines <- gFiltersList[["Empty lines"]]


# # table example
# plop <- read.csv('dev/example_filtration_tab_NA.txt', sep='\t')
# # create the MSnset dataset to use DAPAR function getListNbValuesInLines
# metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
# colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
# metadata_plop$Sample.name <- colnames(plop)
# metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
# metadata_plop$Bio.Rep <- c(1:6)
# plop_msnset <- DAPAR::createMSnset(file = 'dev/example_filtration_tab_NA.txt',
#                                    indExpData = c(1:6),
#                                    indFData = c(1:6), 
#                                    metadata = metadata_plop,
#                                    pep_prot_data="peptide",
#                                    software = 'maxquant')


########################################################################

ui <- fluidPage(
  
  fluidRow(column(4,selectInput("ChooseFilters","",
                                choices = gFiltersList,
                                selected = "None",
                                width='200px')),
           column(8,textOutput("methodInformation"))),
  
  uiOutput("seuilNADelete"),
  
  actionButton("show_modal", "Open modal example"),
  
  shinyBS::bsModal("example_modal",
                   title="modal",
                   size = "large",
                   trigger="show_modal",
                   uiOutput("modal_content"))#,
  
  #dataTableOutput("buildFiltrationExample")
  
)

server <- function(input, output, session){
  
  # ###############
  # # options modal
  # jqui_resizable(paste0("#","example_modal"," .modal-content")
  #                ,options = list(minHeight = 500, minWidth=500  ))
  # 
  # jqui_draggable(paste0("#","example_modal"," .modal-content")
  #                , options = list(revert=TRUE) 
  # )
  # ###############
  
  
  plop <- read.csv('~/TELETRAVAIL/github_master/Prostar/inst/ProstarApp/dev/example_filtration_tab_NA.txt', sep='\t')
  
  
  
  output$methodInformation <- renderText({
    switch(input$ChooseFilters,
           None = {
             txt <- "No lines are removed."
           },
           EmptyLines = {
             txt <- "Lines with only NAs are removed."
           },
           WholeMatrix = {
             if(input$seuilNA=="0"){txt <- "No lines are removed."}
             else{txt <- paste("Lines containing at least",
                               input$seuilNA,
                               "quantitative values per row are kept.")}
           },
           AllCond = {
             if(input$seuilNA=="0"){txt <- "No lines are removed."}
             else{txt <- paste("Lines containing at least",
                               input$seuilNA,
                               "quantitative values per row, in all condition, are kept.")}
           },
           AtLeastOneCond = {
             if(input$seuilNA=="0"){txt <- "No lines are removed."}
             else{txt <- paste("Lines containing at least",
                               input$seuilNA,
                               "quantitative values per row, in at least one condition, are kept.")}
           }
    )
    txt
  })
  
  
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
    
    
    metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
    colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
    metadata_plop$Sample.name <- colnames(plop)
    metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
    metadata_plop$Bio.Rep <- c(1:6)
    plop_msnset <- DAPAR::createMSnset(file = '~/TELETRAVAIL/github_master/Prostar/inst/ProstarApp/dev/example_filtration_tab_NA.txt',
                                       indExpData = c(1:6),
                                       indFData = c(1:6), 
                                       metadata = metadata_plop,
                                       pep_prot_data="peptide",
                                       software = 'maxquant')
    
    
    tagList(
      selectInput("seuilNA", NULL,
                  choices =  DAPAR::getListNbValuesInLines(plop_msnset,
                                                           type=input$ChooseFilters),
                  selected = "0",
                  width='150px')
    )
  })
  
  
  output$modal_content <- renderUI({
    
    
    tagList(
      p("Table example to filter:"),
      
      uiOutput("example_tab"),
      
      actionButton("run_example", "Run Example"))
    
  })
  
  
  output$example_tab <- renderUI({
    
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
                                  "3" = { index <- c(5:7,10) },
                                  "4" = { index <- c(4:7,9,10) },
                                  "5" = { index <- c(3:10)},
                                  "6" = { index <- c(2:10)}
           )},
           AllCond = { switch(input$seuilNA,
                              "0" = { index <- NULL },
                              "1" = { index <- c(4:7) },
                              "2" = { index <- c(3:7,9,10) },
                              "3" = { index <- c(2:10) }
           )},
           AtLeastOneCond = { switch(input$seuilNA,
                                     "0" = { index <- NULL },
                                     "1" = { index <- 7 },
                                     "2" = { index <- c(6,7,10) },
                                     "3" = { index <- c(5:10) }
           )}
    )

    dt <- plop
    
    if (!is.null(index)){
      #Warning: Error in if: l'argument est de longueur nulle
      
      DT::datatable(dt,
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
      p('No filtering with these parameters.')
      
      #Warning: Error in if: l'argument est de longueur nulle
      
      DT::datatable(dt,
                    options = list(
                      paging = FALSE,
                      searching = FALSE)
      )
    }
    
  })
  
  
  # output$buildFiltrationExample <- DT::renderDataTable({
  #   ################################################################
  #   # A1 A2 A3 B1 B2 B3
  #   # 1   1  1  1  1  1  1
  #   # 2  NA  1  1  1  1  1
  #   # 3  NA NA  1  1  1  1
  #   # 4  NA NA NA  1  1  1
  #   # 5  NA NA NA NA  1  1
  #   # 6  NA NA NA NA NA  1
  #   # 7  NA NA NA NA NA NA
  #   # 8  NA  1  1 NA  1  1
  #   # 9  NA  1  1 NA NA  1
  #   # 10 NA NA  1 NA NA  1
  #   
  #   # Indices for filtered rows, to darken:
  #   # paramtype<-c("None", "EmptyLines", "WholeMatrix", "AllCond", "AtLeastOneCond")
  #   # none: nothing to darken, keep all row
  #   # emptyLines: 7
  #   # wholeMatrix: list("th1"=c(7),"th2"=c(6,7),"th3"=c(5:7,10),"th4"=c(4:7,9,10),"th5"=c(3:10),"th6"=c(2:10))
  #   # allCond: list("th1"=c(4:7),"th2"=c(3:7,9,10),"th3"=c(2:10))
  #   # atLeastOneCondition: list("th1"=c(7),"th2"=c(6,7,10),"th3"=c(5:10))
  #   
  #   # Example with method=WholeMatrix/th=3 <=> at least 3 quanti values by entire row
  #   # index <- c(5,6,7,10)
  #   ################################################################
  #   switch(input$ChooseFilters,
  #          None = {
  #            index <- NULL
  #          },
  #          EmptyLines = {
  #            index <- 7
  #          },
  #          WholeMatrix = { switch(input$seuilNA,
  #                                 "0" = { index <- NULL },
  #                                 "1" = { index <- 7 },
  #                                 "2" = { index <- c(6,7) },
  #                                 "3" = { index <- c(5:7,10) },
  #                                 "4" = { index <- c(4:7,9,10) },
  #                                 "5" = { index <- c(3:10)},
  #                                 "6" = { index <- c(2:10)}
  #          )},
  #          AllCond = { switch(input$seuilNA,
  #                             "0" = { index <- NULL },
  #                             "1" = { index <- c(4:7) },
  #                             "2" = { index <- c(3:7,9,10) },
  #                             "3" = { index <- c(2:10) }
  #          )},
  #          AtLeastOneCond = { switch(input$seuilNA,
  #                                    "0" = { index <- NULL },
  #                                    "1" = { index <- 7 },
  #                                    "2" = { index <- c(6,7,10) },
  #                                    "3" = { index <- c(5:10) }
  #          )}
  #   )
  #   
  #   dt <- plop
  #   
  #   if (!is.null(index)){
  #     datatable(dt,
  #               options = list(
  #                 paging = FALSE,
  #                 searching = FALSE)) %>%
  #       formatStyle(
  #         .,
  #         columns = 1,
  #         valueColumns = 0,
  #         target = 'row',
  #         backgroundColor = styleEqual(index, rep('grey', length(index)) )
  #       )
  #   } else {
  #     datatable(dt,
  #               options = list(
  #                 paging = FALSE,
  #                 searching = FALSE))
  #   }
  # })
  
}


shinyApp(ui, server)


