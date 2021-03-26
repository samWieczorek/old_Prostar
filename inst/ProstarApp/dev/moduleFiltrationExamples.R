library(shiny)
library(DT)
library(DAPAR)
library(shinyBS)
library(shinyjqui)

gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "WholeMatrix",
                  "For every condition" = "AllCond",
                  "At least one condition" = "AtLeastOneCond")
gFilterEmptyLines <- gFiltersList[["Empty lines"]]


#path <- "~/Github/master/Prostar/inst/ProstarApp/"
path <- "~/TELETRAVAIL/github_master/Prostar/inst/ProstarApp/"


plop <- data.frame(A1 = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                   A2	 = c(1, 1, NA, NA, NA, NA, NA, 1, 1, NA),
                   A3 = c(1, 1, 1, NA, NA, NA, NA, 1, 1, 1),
                   B1 = c(1, 1, 1, 1, NA, NA, NA, NA, NA, NA),
                   B2 = c(1, 1, 1, 1, 1, NA, NA, 1, NA, NA),
                   B3 = c(1, 1, 1, 1, 1, 1, NA, 1, 1, 1)
                   )
#plop[plop==1] <- sample(10, 1)

metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
metadata_plop$Sample.name <- colnames(plop)
metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
metadata_plop$Bio.Rep <- c(1:6)
plop_msnset <- DAPAR::createMSnset(file = plop,
                                   indExpData = c(1:6),
                                   indFData = c(1:6), 
                                   metadata = metadata_plop,
                                   pep_prot_data="peptide",
                                   software = 'maxquant')



########################################################################



ui <- fluidPage(
  
  fluidRow(
    column(2,
           
           selectInput("chooseMetacellTag",
                       "metacellTag",
                       choices = c('None' = 'None',
                                   DAPAR::metacell.def(plop_msnset@experimentData@other$typeOfData)$node
                       ),
                       width='200px')
    ),
    column(2,
           uiOutput("Choose_keepOrRemove_ui")
    ),
    column(2,
           selectInput("ChooseMetacellFilters",
                       "filterScope",
                       choices = c(gFiltersList[1],
                                   "Whole Line"="WholeLine",
                                   gFiltersList[3:length(gFiltersList)]),
                       selected = "None",
                       width='200px')
    ),
    column(6,
           uiOutput("MetacellFilters_widgets_set2_ui")
    )
  ),
  
  actionButton("show_modal", "View example"),
  
  shinyBS::bsModal("example_modal",
                   title="Modal",
                   size = "large",
                   trigger="show_modal",
                   uiOutput("modal_content"),
                   tags$head(tags$style("#example_modal .modal-footer{ display:none}"))
  )# ,
  
  # dataTableOutput("buildFiltrationExample")
  
)



server <- function(input, output, session){
  
  
  
  ###############
  # options modal
  jqui_draggable(paste0("#","example_modal"," .modal-content"),
                 options = list(revert=FALSE)
  )
  ###############
  
  
  
  output$Choose_keepOrRemove_ui <- renderUI({
    
    radioButtons("ChooseKeepRemove",
                 "Type of filter operation",
                 choices = setNames(nm = c("delete", "keep")),
                 selected = "delete")
    
  })
  
  
  output$MetacellFilters_widgets_set2_ui <- renderUI({
    req(!(input$ChooseMetacellFilters %in% c("None", "WholeLine")))
    
    
    fluidRow(
      column(4,
             radioButtons('choose_val_vs_percent',
                          "choose_val_vs_percent",
                          choices = setNames(nm=c('Value', 'Percentage')),
                          selected = 'Value'
             )
      ),
      column(4,
             selectInput("choose_metacellFilter_operator",
                         "Choose operator",
                         choices = setNames(nm=DAPAR::SymFilteringOperators()),
                         selected = "<=",
                         width='150px')
      ),
      column(4,
             uiOutput('choose_value_ui'),
             uiOutput('choose_percentage_ui')
      )
    )
  })
  
  
  output$choose_value_ui <- renderUI({
    req(input$choose_val_vs_percent == 'Value')
    
    
    tagList(
      selectInput("choose_metacell_value_th",
                  "metacell_value_th",
                  choices =  getListNbValuesInLines(plop_msnset, 
                                                    type = input$ChooseMetacellFilters),
                  selected = 0,
                  width='150px')
    )
  })
  
  
  
  output$choose_percentage_ui <- renderUI({
    req(input$choose_val_vs_percent == 'Percentage')
    
    
    tagList(
      numericInput("choose_metacell_percent_th", 
                   "metacell_percent_th",
                   min = 0,
                   max = 100,
                   value = 0,
                   width='150px')
    )
  })
  
  
  
  
  
  
  
  
  
  
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
    
    
    # change example tab after chooseMetacellTag user choice
    if (input$chooseMetacellTag != "None"){
      if (input$chooseMetacellTag %in% c("missing", "missing POV", "missing MEC")){
        plop[plop!=1] <- input$chooseMetacellTag  
      } else {
        plop[plop!="na"] <- input$chooseMetacellTag  
      }
    }
    
    
    
    if (!(input$ChooseMetacellFilters %in% c("None", "WholeLine"))){
      th <- NULL
      if (input$choose_val_vs_percent == 'Value'){
        th <- as.integer(input$choose_metacell_value_th)
      } else {
        th <- as.numeric(input$choose_metacell_percent_th)
      }
    }
    
    level <- plop_msnset@experimentData@other$typeOfData
    pattern <- input$chooseMetacellTag
    type <- input$ChooseMetacellFilters
    percent <- input$choose_val_vs_percent == 'Percentage'
    op <- input$choose_metacellFilter_operator
    conds <-  Biobase::pData(plop_msnset)$Condition
    
    index <- NULL
    print(index)
    #if (input$ChooseMetacellFilters != "None"){
      mask <- match.metacell(metadata=DAPAR::GetMetacell(plop_msnset), 
                             pattern=pattern, 
                             level=level)
      print(mask)
      
      
      index <- switch(input$ChooseMetacellFilters,
                      WholeLine = DAPAR::GetIndices_WholeLine(metacell.mask = mask),
                      WholeMatrix = DAPAR::GetIndices_WholeMatrix(metacell.mask = mask,
                                                                  op = op, 
                                                                  percent = percent, 
                                                                  th = th),
                      AllCond = DAPAR::GetIndices_BasedOnConditions(metacell.mask = mask, 
                                                                    type = type, 
                                                                    conds = conds, 
                                                                    percent = percent, 
                                                                    op = op, 
                                                                    th = th),
                      AtLeastOneCond = DAPAR::GetIndices_BasedOnConditions(metacell.mask = mask, 
                                                                           type = type,
                                                                           conds = conds, 
                                                                           percent = percent,
                                                                           op = op, 
                                                                           th = th)
      )
    #}
    print(index)
    
    
    if(input$ChooseMetacellFilters != "None" &&
       input$ChooseKeepRemove == "keep"){
      if(!is.null(index)) {
        index <- (1:nrow(plop))[-index]
      } else {
        index <- 1:nrow(plop)
      }
    }
    print(index)
    
    
    
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
  #          WholeMatrix = { switch(th,
  #                                 "0" = { index <- NULL },
  #                                 "1" = { index <- 7 },
  #                                 "2" = { index <- c(6,7) },
  #                                 "3" = { index <- c(5:7,10) },
  #                                 "4" = { index <- c(4:7,9,10) },
  #                                 "5" = { index <- c(3:10)},
  #                                 "6" = { index <- c(2:10)}
  #          )},
  #          AllCond = { switch(th,
  #                             "0" = { index <- NULL },
  #                             "1" = { index <- c(4:7) },
  #                             "2" = { index <- c(3:7,9,10) },
  #                             "3" = { index <- c(2:10) }
  #          )},
  #          AtLeastOneCond = { switch(th,
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


