




mod_query_metacell_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    div(
      fluidRow(
        column(2, uiOutput(ns('chooseMetacellTag_ui'))),
        column(2, uiOutput(ns("Choose_keepOrRemove_ui"))),
        column(2, uiOutput(ns('choose_metacellFilters_ui'))),
        column(6, tagList(
          uiOutput(ns("show_example_ui")),
          uiOutput(ns("MetacellFilters_widgets_set2_ui")))
        )
      ),
      div( style="display:inline-block; vertical-align: middle; align: center;",
           uiOutput(ns('metacellFilter_request_ui'))
      )
    )
  )
  
}





mod_query_metacell_server <- function(id,
                                      obj,
                                      list_tags = reactive({NULL}),
                                      keep_vs_remove = reactive({NULL}),
                                      filters = reactive({NULL}),
                                      val_vs_percent = reactive({NULL}),
                                      operator = reactive({NULL})
                                      ) {
  
  rv <- reactiveValues(
    indices = NULL,
    trigger = NULL,
    params = NULL,
    query = NULL
  )
  
  moduleServer(id,
    function(input, output, session) {
      ns <- session$ns
      
      callModule(modulePopover,"metacellTag_help", 
                 data = reactive(list(title = "Nature of data to filter", 
                                      content="Define xxx")))
      
      
      callModule(modulePopover,"filterScope_help", 
                 data = reactive(list(title = "Scope", 
                                      content=HTML(paste0("To filter the missing values, the choice of the lines to be kept is made by different options:"),
                                                   ("<ul>"),
                                                   ("<li><strong>None</strong>: No filtering, the quantitative data is left unchanged.</li>"),
                                                   ("<li><strong>(Remove) Empty lines</strong>: All the lines with 100% of missing values are filtered out.</li>"),
                                                   ("<li><strong>Whole Matrix</strong>: The lines (across all conditions) which contain less quantitative value than a user-defined threshold are kept;</li>"),
                                                   ("<li><strong>For every condition</strong>: The lines for which each condition contain less quantitative value than a user-defined threshold are deleted;</li>"),
                                                   ("<li><strong>At least one condition</strong>: The lines for which at least one condition contain less quantitative value than a user-defined threshold are deleted.</li>"),
                                                   ("</ul>")
                                      )
                 )))
      
      
      
      
      rv.widgets <- reactiveValues(
        MetacellTag = "None",
        MetacellFilters = "None",
        KeepRemove = 'delete',
        metacell_value_th = 0,
        metacell_percent_th = 0,
        val_vs_percent = 'Count',
        metacellFilter_operator = '<='
      )
      
      observeEvent(input$chooseMetacellTag, { rv.widgets$MetacellTag <- input$chooseMetacellTag})
      observeEvent(input$ChooseKeepRemove, {  rv.widgets$KeepRemove <- input$ChooseKeepRemove})
      observeEvent(input$ChooseMetacellFilters, {  rv.widgets$MetacellFilters <- input$ChooseMetacellFilters})
      observeEvent(input$choose_val_vs_percent, {rv.widgets$val_vs_percent <- input$choose_val_vs_percent})
      observeEvent(input$choose_metacell_value_th, { rv.widgets$metacell_value_th <- input$choose_metacell_value_th})
      observeEvent(input$choose_metacell_percent_th, {rv.widgets$metacell_percent_th <- input$choose_metacell_percent_th})
      observeEvent(input$choose_metacellFilter_operator, {  rv.widgets$metacellFilter_operator <- input$choose_metacellFilter_operator})

      output$chooseMetacellTag_ui <- renderUI({
        selectInput(ns("chooseMetacellTag"),
                  modulePopoverUI(ns("metacellTag_help")),
                  choices = list_tags(),
                  selected = rv.widgets$MetacellTag,
                  width='200px')
      })
      
      output$Choose_keepOrRemove_ui <- renderUI({
        req(rv.widgets$MetacellTag != 'None')
        radioButtons(ns("ChooseKeepRemove"),
                     "Type of filter operation",
                     choices = keep_vs_remove(),
                     selected = rv.widgets$KeepRemove)
      })
      
      
      output$choose_metacellFilters_ui <- renderUI({
        req(rv.widgets$MetacellTag != 'None')
        selectInput(ns("ChooseMetacellFilters"),
                    modulePopoverUI(ns("filterScope_help")),
                    choices = filters(),
                    selected = rv.widgets$MetacellFilters,
                    width='200px')
      })
      
      
      
      output$show_example_ui <- renderUI({
        req(rv.widgets$MetacellFilters)
        req(rv.widgets$MetacellFilters != "None")
        
        mod_filtering_example_server(id = 'filteringExample',
                                   obj = reactive({obj()}),
                                   indices = reactive({rv$indices}),
                                   params = reactive({rv.widgets}),
                                   txt = reactive({WriteQuery()})
                                   )
        
         mod_filtering_example_ui(ns('filteringExample'))
      })
      
      
      output$MetacellFilters_widgets_set2_ui <- renderUI({
        req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))

        callModule(modulePopover,"choose_val_vs_percent_help", 
                   data = reactive(list(title = paste("#/% of values to ", rv.widgets$KeepRemove),
                                        content="Define xxx")))
        
        tagList(
          fluidRow(
            column(4,
                   radioButtons(ns('choose_val_vs_percent'),
                                modulePopoverUI(ns("choose_val_vs_percent_help")),
                                choices = val_vs_percent(),
                                selected = rv.widgets$val_vs_percent
                   )
            ),
            column(4,
                   selectInput(ns("choose_metacellFilter_operator"),
                               "Choose operator",
                               choices = operator(),
                               selected = rv.widgets$metacellFilter_operator,
                               width='150px')
            ),
            column(4,
                   uiOutput(ns('choose_value_ui')),
                   uiOutput(ns('choose_percentage_ui'))
            )
          )
        )
        
      })
      
      output$choose_value_ui <- renderUI({
        req(rv.widgets$val_vs_percent == 'Count')
        req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))
        
        callModule(modulePopover,"metacell_value_th_help", 
                   data = reactive(list(title = "Count threshold", 
                                        content="Define xxx")))
        
        tagList(
          modulePopoverUI(ns("modulePopover_keepVal")),
          selectInput(ns("choose_metacell_value_th"),
                      modulePopoverUI(ns("metacell_value_th_help")),
                      choices = getListNbValuesInLines(obj(), 
                                                        type = rv.widgets$MetacellFilters),
                      selected = rv.widgets$metacell_value_th,
                      width='150px')
        )
      })
      
      
      
      output$choose_percentage_ui <- renderUI({
        req(rv.widgets$val_vs_percent == 'Percentage')
        req(!(rv.widgets$MetacellFilters %in% c("None", "WholeLine")))
        
        callModule(modulePopover,"metacell_percent_th_help", 
                   data = reactive(list(title = "Percentage threshold", 
                                        content="Define xxx")))
        tagList(
          modulePopoverUI(ns("modulePopover_keepVal_percent")),
          sliderInput(ns("choose_metacell_percent_th"), 
                       modulePopoverUI(ns("metacell_percent_th_help")),
                       min = 0,
                       max = 100,
                        step = 1,
                       value = rv.widgets$metacell_percent_th,
                       width='150px')
        )
      })
      
      
      WriteQuery <- reactive({
        if (rv.widgets$MetacellFilters == "None"){
          txt_summary <- "No filtering is processed."
        } else if (rv.widgets$MetacellFilters == "WholeLine") {
          txt_summary <- paste(rv.widgets$KeepRemove,
                               "lines that contain only",
                               rv.widgets$MetacellTag)
        } else {
          
          switch(rv.widgets$MetacellFilters,
                 "WholeMatrix" = text_method <- "the whole matrix.",
                 "AllCond" = text_method <- "each condition.",
                 "AtLeastOneCond" = text_method <- "at least one condition.")
          
          if(rv.widgets$val_vs_percent == 'Count'){
            text_threshold <- rv.widgets$metacell_value_th
          } else {
            text_threshold <- paste(as.character(rv.widgets$metacell_percent_th),
                                    " %", sep="")
          }
          
          txt_summary <- paste(rv.widgets$KeepRemove,
                               " lines where number of ",
                               rv.widgets$MetacellTag,
                               " data ",
                               rv.widgets$metacellFilter_operator,
                               " ",
                               text_threshold,
                               " in ",
                               text_method)
        }
        txt_summary
      })
      
      output$metacellFilter_request_ui <- renderUI({
        txt_summary <- paste("You are going to ", WriteQuery())
        tags$p(txt_summary, style = "font-size: small; text-align : center; color: purple;")
      })

      #Set useless widgets to default values
      observeEvent(rv.widgets$MetacellFilters == 'WholeLine',{
        rv.widgets$metacell_percent_th <- 0
        rv.widgets$metacell_value_th <- 0
        rv.widgets$val_vs_percent <- 'Percentage'
      },
      priority = 1000)
      
      
      
      observe({
        req(obj())
        
        th <- 0
        if (rv.widgets$val_vs_percent == 'Percentage') {
          th <- rv.widgets$metacell_percent_th / 100
        } else  if (rv.widgets$val_vs_percent == 'Count'){
          th <- as.integer(rv.widgets$metacell_value_th)
        }
        
        
        rv$indices <- DAPAR::GetIndices_MetacellFiltering(obj = obj(),
                                            level = DAPAR::GetTypeofData(obj()),
                                            pattern = rv.widgets$MetacellTag,
                                            type = rv.widgets$MetacellFilters,
                                            percent = rv.widgets$val_vs_percent == 'Percentage',
                                            op = rv.widgets$metacellFilter_operator,
                                            th = th)

        rv$trigger = as.numeric(Sys.time())
        rv$params <- list(MetacellTag = rv.widgets$MetacellTag,
                          KeepRemove = rv.widgets$KeepRemove,
                          MetacellFilters = rv.widgets$MetacellFilters,
                          metacell_percent_th = rv.widgets$metacell_percent_th,
                          metacell_value_th = rv.widgets$metacell_value_th,
                          val_vs_percent = rv.widgets$val_vs_percent,
                          metacellFilter_operator = rv.widgets$metacellFilter_operator)
        
        rv$query <- WriteQuery()
      })
      
      
     
    })
  reactive({list(trigger = rv$trigger,
                 indices = rv$indices,
                 params = rv$params,
                 query = rv$query
                 )
    })
}

