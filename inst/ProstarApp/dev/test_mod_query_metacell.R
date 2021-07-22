library(shiny)
library(DAPAR)
library(DT)
library(shinyBS)
options(shiny.fullstacktrace=T)

source(file.path("../server","mod_query_metacell.R"), local=TRUE)$value
source(file.path("../server","mod_filtering_example.R"), local=TRUE)$value
source(file.path("../server","mod_popover.R"), local=TRUE)$value


actionBtnClass <- "btn-primary"

PrevNextBtnClass <- "btn-info"
optionsBtnClass <- "info"



gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "WholeMatrix",
                  "For every condition" = "AllCond",
                  "At least one condition" = "AtLeastOneCond")

utils::data(Exp1_R25_prot, package='DAPARdata')
obj <- Exp1_R25_prot[1:10,]


getDataForExprs <- function(obj, digits=NULL){
  
  if (is.null(digits))
    digits <- 2
  
   test.table <- cbind(round(Biobase::exprs(obj), digits = digits), DAPAR::GetMetacell(obj))
  return(test.table)
}



# BuildColorStyles <- function(obj, colors.def){
#   
#   level <- obj@experimentData@other$typeOfData
#   list_missing_POV_tags <- c('missing POV', 'imputed POV')
#   list_missing_MEC_tags <- c('missing MEC', 'imputed MEC')
#   list_Identified_tags <- 'identified'
#   list_Recovered_tags <- 'recovered'
#   list_Combined_tags <- 'combined'
#   
#   styles <- list(tags = NULL,
#                  colors = NULL)
#   
#   if (length(list_POV_tags) > 0){
#     styles$tags <- c(styles$tags, list_POV_tags)
#     styles$colors <- c(styles$colors, rep(colors.def$POV, length(list_POV_tags)))
#   }
#   
#   if (length(list_MEC_tags) > 0){
#     styles$tags <- c(styles$tags, list_MEC_tags)
#     styles$colors <- c(styles$colors, rep(colors.def$MEC, length(list_MEC_tags)))
#   }
#   
#   if (length(list_Identified_tags) > 0){
#     styles$tags <- c(styles$tags, list_Identified_tags)
#     styles$colors <- c(styles$colors, rep(colors.def$identified, length(list_Identified_tags)))
#   }
#   
#   if (length(list_Recovered_tags )> 0){
#     styles$tags <- c(styles$tags, list_Recovered_tags)
#     styles$colors <- c(styles$colors, rep(colors.def$recovered, length(list_Recovered_tags)))
#   }
#   
#   
#   if (length(list_Combined_tags) > 0){
#     styles$tags <- c(styles$tags, list_Combined_tags)
#     styles$colors <- c(styles$colors, rep(colors.def$combined, length(list_Combined_tags)))
#   }
#   
#   styles
# }


BuildColorStyles <- function(obj){
  styles <- list(tags = NULL,
                 colors = NULL)
mc <- metacell.def(GetTypeofData(obj))

styles$tags <- mc$node
styles$colors <- mc$color
styles
}





ui <- fluidPage(
  tagList(
    mod_query_metacell_ui('query'),
    # uiOutput('res'),
    # actionButton("performMetacellFiltering", 
    #              "Perform metacell filtering"),
    hr(),
    mod_query_metacell_ui('query2')
  )
)



server <- function(input, output, session) {
  
  
  ll.filter.names <- c("None" = "None",
                       "Whole Line" = "WholeLine",
                       "Whole matrix" = "WholeMatrix",
                       "For every condition" = "AllCond",
                       "At least one condition" = "AtLeastOneCond")
  
  
  observe({
   test <- mod_query_metacell_server(id = 'query',
                            obj = reactive({obj}),
                            list_tags = reactive({c('None' = 'None',
                                                    DAPAR::metacell.def(GetTypeofData(obj))$node
                            )}),
                            keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
                            filters = reactive({ll.filter.names}),
                            val_vs_percent = reactive({setNames(nm=c('Count', 'Percentage'))}),
                            operator = reactive({setNames(nm=DAPAR::SymFilteringOperators())})
  )
})
  
  # observe({
  #   test()$trigger
  #   print(paste0('from caller :', paste0(test()$indices, collapse = ",")))
  # })
  # 
  # observeEvent(input$performMetacellFiltering, ignoreInit = TRUE,{
  #   print(input$performMetacellFiltering)
  # })
  
  
  test_2 <- mod_query_metacell_server(id = 'query2',
                                    obj = reactive({obj}),
                                    list_tags = reactive({c('None' = 'None',
                                                            DAPAR::metacell.def(GetTypeofData(obj))$node
                                    )}),
                                    keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
                                    filters = reactive({ll.filter.names}),
                                    val_vs_percent = reactive({setNames(nm=c('Count', 'Percentage'))}),
                                    operator = reactive({setNames(nm=DAPAR::SymFilteringOperators())})
  )
  
  observe({
    test_2()
print(test_2())

})
}


shinyApp(ui, server)