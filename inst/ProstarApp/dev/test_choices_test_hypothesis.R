library(shiny)
library(shinyjs)

options(shiny.fullstacktrace=T)

ui <- fluidPage(
  tagList(
    useShinyjs(),
    actionButton('go', 'Compute comparisons'),
    actionButton('validate', 'Validate'),
    actionButton('reset', 'Reset'),
    actionButton('test', '', icon('sync-alt',  lib = "font-awesome", style = "color: grey;"), 
                 style = " 
                     background-color: none; 
                     position: relative; 
                     left: 3%;
                     border-radius: 0%;
                     border-width: 0px"),
    
    fluidRow(
      column(width = 2, h3(tags$strong('Condition 1'))),
      column(width = 2, h3(tags$strong('Condition 2'))),
      column(width = 2, div(id='swap_title', h3(tags$strong('Swap'))))
    ),
    
    fluidRow(
      column(width = 2, uiOutput('cond1_ui')),
      column(width = 2, uiOutput('cond2_ui')),
      column(width = 2, uiOutput('btns_ui')
      )
    )
  )
)


server <- shinyServer(function(session, input, output) {
  utils::data(Exp1_R25_prot, package='DAPARdata')
  obj <- Exp1_R25_prot
  
  rv <- reactiveValues(
    listComp = NULL,
    keep = NULL,
    swap = NULL,
    n = NULL,
    comp.names = NULL
  )
  
  
  
  
  
  observeEvent(input$reset, ignoreInit = TRUE,{
    print("reset")
    rv$listComp <- NULL

   })
  
  
  
  
  output$cond1_ui <- renderUI({
    
    req(rv$listComp)
    lapply(seq_len(rv$n), function(i) {
      ll <- unlist(strsplit(colnames(rv$listComp$logFC)[i], split = '_'))
     # tags$div( style="vertical-align: middle; padding-bottom: 10px; text-align: center;",
                p(style="vertical-align: middle; padding-bottom: 10px; text-align: center;",gsub('[()]', '', ll[1]))
                
     # )
    })
  })
  
  
  
  
  output$cond2_ui <- renderUI({
    
    req(rv$listComp)
    lapply(seq_len(rv$n), function(i) {
      ll <- unlist(strsplit(colnames(rv$listComp$logFC)[i], split = '_'))
      tags$div( style="vertical-align: middle; padding-bottom: 10px; text-align: center;",
                p(gsub('[()]', '', ll[3]))
      )
    })
  })
  
  
  output$btns_ui <- renderUI({
    req(rv$listComp)
    lapply(seq_len(rv$n), function(i) {
      tags$div( style="vertical-align: middle; padding-bottom: 5px;",
                actionButton(paste0('compswap', i), '',
                           icon('sync',  lib = "font-awesome"), 
                           style = "background-color: none; 
                          position: relative; 
                          left: 3%;
                          border-radius: 0%;
                          border-width: 0px")
      )
    })
  })
  
  
  observeEvent(input$go, ignoreInit = TRUE,{
    print('tutu')
    rv$listComp <- DAPAR::limmaCompleteTest(exprs(obj),
                                            pData(obj),
                                            'OnevsAll')
    
    rv$n <- ncol(rv$listComp$logFC)
    #browser()
  }, priority = 1000)
  

  
  output$inputGroup = renderUI({
    print("output$inputGroup = renderUI")
    req(rv$listComp)
    #isolate({
      #browser()
      input_list <- tagList(
      
      lapply(seq_len(ncol(rv$listComp$logFC)), function(i) {
        ll <- unlist(strsplit(colnames(rv$listComp$logFC)[i], split = '_'))
        isolate({
          fluidRow(
          
          column(width = 2, p(id='pcond1', tags$strong(ll[1]))),
          column(width = 2, p(id='pcond2', tags$strong(gsub('[()]', '', ll[3])))),
          column(width = 2, checkboxInput(paste0('compkeep', i), '',
                                          value = TRUE,
                                          width = '80px')
                 ),
          column(width = 2, checkboxInput(paste0('compswap', i), '',
                                          value = FALSE,
                                          width = '80px')

        )
        
        )
        })
      })
    )
   # })


    input_list
  })
  

  
  GetShinyValue <- function(pattern){
    req(ncol(rv$listComp$logFC))
    unlist(lapply(seq_len(ncol(rv$listComp$logFC)), function(x) input[[paste0(pattern,x)]]))
     }
  
  
  observeEvent(GetShinyValue('compkeep'), ignoreInit = TRUE, {
    print("observeEvent(GetShinyValue('compkeep')")
    print(GetShinyValue('compkeep'))
    #rv$keep <- GetShinyValue('compkeep')
    #print(paste0('keep : ', paste(rv$keep, collapse = ' ')))

    for (i in seq_len(ncol(rv$listComp$logFC)))
      updateCheckboxInput(session, paste0("compkeep", i), value = GetShinyValue('compkeep')[i])

  })
  
  
  observeEvent(GetShinyValue('compswap'),  ignoreInit = TRUE, {
    print("observeEvent(GetShinyValue('compswap')")
    #rv$swap <- GetShinyValue('compswap')
    #print(paste0('swap : ', paste(rv$swap, collapse = ' ')))
    isolate({
      for (i in seq_len(ncol(rv$listComp$logFC)))
      updateCheckboxInput(session, paste0("compswap", i), value = GetShinyValue('compswap')[i])
    
    # swap results
    
    swap <- GetShinyValue('compswap')
      ind.swap <- which(swap == TRUE)
      if (length(ind.swap) > 0){
        for(i in ind.swap){
          current.comp <- colnames(rv$listComp$logFC)[i]

          # Swap comparisons names
          ll <- unlist(strsplit(current.comp, split = '_'))
          tmp.cond1 <- ll[1]
          tmp.cond2 <- gsub('[()]', '', ll[3])
          tmp.logFC <- paste0('(', tmp.cond2, ')_vs_(', tmp.cond1, ')_logFC')
          tmp.pval <- paste0('(', tmp.cond2, ')_vs_(', tmp.cond1, ')_pval')
          colnames(rv$listComp$logFC)[i] <- tmp.logFC
          colnames(rv$listComp$P_Value)[i] <- tmp.pval

          # Swap logFC values
          rv$listComp$logFC[, i] <- - rv$listComp$logFC[, i]
        }
      }
    })
  })

  # 
  # UpdateCompResults <- reactive({
  #   # Manage the swap option
  #   ind.swap <- which(rv$swap == TRUE)
  #   if (length(ind.swap) > 0){
  #     for(i in ind.swap){
  #       current.comp <- colnames(rv$listComp$logFC)[i]
  #       
  #       # Swap comparisons names
  #       ll <- unlist(strsplit(current.comp, split = '_'))
  #       tmp.cond1 <- ll[1]
  #       tmp.cond2 <- gsub('[()]', '', ll[3])
  #       tmp.logFC <- paste0('(', tmp.cond2, ')_vs_(', tmp.cond1, ')_logFC')
  #       tmp.pval <- paste0('(', tmp.cond2, ')_vs_(', tmp.cond1, ')_pval')
  #       colnames(rv$listComp$logFC)[i] <- tmp.logFC
  #       colnames(rv$listComp$P_Value)[i] <- tmp.pval
  #       
  #       # Swap logFC values
  #       rv$listComp$logFC[, i] <- - rv$listComp$logFC[, i]
  #     }
  #   }
  #   
  #   
  #   ind.delete <- which(rv$keep == FALSE)
  #   if (length(ind.delete) > 0){
  #     rv$comp.names <- colnames(rv$listComp$logFC)[-ind.delete]
  #     rv$listComp$logFC <- as.data.frame(rv$listComp$logFC[ , -ind.delete])
  #     colnames(rv$listComp$logFC) <- rv$comp.names
  #     
  #     tmp <- colnames(rv$listComp$P_Value)[-ind.delete]
  #     rv$listComp$P_Value <-  as.data.frame(rv$listComp$P_Value[ , -ind.delete])
  #     colnames(rv$listComp$P_Value) <- tmp
  #     
  #     rv$swap <- rv$swap[-ind.delete]
  #     rv$keep <- rv$keep[-ind.delete]
  #     rv$n <- ncol(rv$listComp$logFC)
  #   }
  #   
  # })
  
  # observeEvent(input$validate, {
  #   
  #   # Manage the keep option
  #   UpdateCompResults()
  #   
  #   print(str(rv$listComp))
  #    for (i in seq_len(rv$n)){
  #      shinyjs::toggle(paste0("compswap", i), condition = FALSE)
  #      shinyjs::toggle(paste0("compkeep", i), condition = TRUE)
  #    }
  # 
  # })
  
})

# Run the application
shinyApp(ui = ui, server = server)