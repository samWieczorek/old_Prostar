library(shiny)
library(DT)
library(DAPAR)
library(shinyBS)
library(shinyjqui)
library(shinyjs)

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) # to display NAs in DT, instead of blank square

gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "WholeMatrix",
                  "For every condition" = "AllCond",
                  "At least one condition" = "AtLeastOneCond")
gFilterEmptyLines <- gFiltersList[["Empty lines"]]

path <- "~/Github/master/Prostar/inst/ProstarApp/"
#path <- "~/TELETRAVAIL/github_master/Prostar/inst/ProstarApp/"



########################################################################

ui <- fluidPage(
  
  selectInput("ChooseFilters","",
              choices = gFiltersList,
              selected = "None",
              width='200px'),
  
  uiOutput("seuilNADelete"),
  
  actionButton("show_modal", "Open modal example"),
  
  shinyBS::bsModal("example_modal",
                   title="Modal",
                   size = "large",
                   trigger="show_modal",
                   uiOutput("modal_content"),
                   tags$head(tags$style("#example_modal .modal-footer{ display:none}"))#,
                   # tags$head(tags$style("#example_modal .modal-header .close { display:none}"))
  )# ,
  
  # dataTableOutput("buildFiltrationExample")
  
)



server <- function(input, output, session){
  
  
  plop <- read.csv(paste0(path, 'dev/example_filtration_tab_NA.txt'), sep='\t')
  
  
  
  output$modal_content <- renderUI({
    
    tagList(
      actionButton("run_example", "Run Example"),
      dataTableOutput("example_tab")
      # 1) vide
      # 2) observeEvent(input$run_example,{
      # 3)   calcul index
      # 4)   tab avec ou sans lignes grises
      # })
    )
    
  })
  
  
  
  output$example_tab <- DT::renderDataTable({
    
    index <- NULL
    switch(input$ChooseFilters,
           None = { index <- NULL },
           EmptyLines = { index <- 7 },
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
    
    
    #DT::datatable(NULL)
    
    
    if (!is.null(index)){
      
      DT::datatable(plop,
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
      
      DT::datatable(plop,
                    options = list(
                      paging = FALSE,
                      searching = FALSE)
      )              
      
    }
  })
  
  
  
  
  ##########################################################################
  # library(shiny)
  # library(DT)
  # shinyApp(
  #   ui = fluidPage(selectInput("select", "select", choices = unique(iris$Species), multiple = T),
  #                  actionButton("go_button", "Search", 
  #                               icon = icon("arrow-circle-o-right")),
  #                  actionButton("reset_button", "Reset", 
  #                               icon = icon("repeat")),
  #                  DT::dataTableOutput('tbl')),
  #   server = function(input, output) {
  #     
  #     values <- reactiveValues(matrix = NULL)
  #     
  #     
  #     observeEvent(input$go_button, {
  #       values$matrix <- iris[iris$Species %in% input$select, ]
  #     })
  #     
  #     observeEvent(input$reset_button, {
  #       values$matrix <- NULL
  #     })
  #     
  #     
  #     output$tbl = DT::renderDataTable({
  #       print("input$go_button")
  #       print(input$go_button)
  #       print("input$reset_button")
  #       print(input$reset_button)
  #       datatable(values$matrix, options = list(lengthChange = FALSE))}
  #     )
  #   }
  # )
  ##########################################################################
  
  
  
  
  
  
  
  
  
  output$seuilNADelete <- renderUI({
    req(input$ChooseFilters)
    
    if ((input$ChooseFilters=="None") || (input$ChooseFilters==gFilterEmptyLines)) {
      return(NULL)
    }
    
    tagList(
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
    plop_msnset <- DAPAR::createMSnset(file = paste0(path, 'dev/example_filtration_tab_NA.txt'),
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
  
  
  
  
  # output$buildFiltrationExample <- DT::renderDataTable({
  #   
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
  #   
  #   
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


