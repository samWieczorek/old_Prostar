library(shiny)
library(shinyjs)

options(shiny.fullstacktrace=T)

ui <- fluidPage(
  tagList(
    useShinyjs(),
    actionButton('go', 'Compute comparisons'),
    actionButton('validate', 'Validate'),
    uiOutput('inputGroup')
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
  
  observeEvent(input$go, ignoreInit = TRUE,{
    print('tutu')
    rv$listComp <- DAPAR::limmaCompleteTest(Biobase::exprs(obj),
                                            Biobase::pData(obj),
                                            'OnevsAll')
    
    rv$n <- ncol(rv$listComp$logFC)
    rv$keep <- rep(TRUE, rv$n)
    rv$swap <- rep(FALSE, rv$n)
    rv$comp.names <- colnames(rv$listComp$logFC)
  }, priority = 1000)
  


  
  output$inputGroup = renderUI({
    req(rv$listComp)
    
    
      input_list <- tagList(
      fluidRow(
         column(width = 2, h3(tags$strong('Condition 1'))),
        column(width = 2, h3(tags$strong('Condition 2'))),
        column(width = 2, div(id='keep_title', h3(tags$strong('Keep')))),
        column(width = 2, div(id='swap_title', h3(tags$strong('Swap'))))
      ),
      lapply(seq_len(rv$n), function(i) {
        ll <- unlist(strsplit(rv$comp.names[i], split = '_'))
        first.cond <- ll[1]
        second.cond <- gsub('[()]', '', ll[3])
        
        fluidRow(
          
          column(width = 2, p(id='pcond1', tags$strong(if (rv$swap[i]) second.cond else first.cond))),
          column(width = 2, p(id='pcond2', tags$strong(if (rv$swap[i]) first.cond else second.cond))),
          column(width = 2, isolate({checkboxInput(paste0('compkeep', i), '',
                                          value = rv$keep[i],
                                          width = '80px')
                 })
                 ),
          column(width = 2, isolate({checkboxInput(paste0('compswap', i), '',
                                          value = rv$swap[i],
                                          width = '80px')
            })
            
        )
        
        )
      })
    )


    input_list
  })
  

  
  GetShinyValue <- function(pattern){
    req(rv$n)
    unlist(lapply(seq_len(rv$n), function(x) input[[paste0(pattern,x)]]))
     }
  
  
  observeEvent(GetShinyValue('compkeep'), ignoreInit = TRUE, {
    rv$keep <- GetShinyValue('compkeep')
    print(paste0('keep : ', paste(rv$keep, collapse = ' ')))

    for (i in seq_len(rv$n))
      shinyjs::toggleState(paste0("compswap", i), condition = rv$keep[i])

  })
  
  
  observeEvent(GetShinyValue('compswap'),  ignoreInit = TRUE, {
    rv$swap <- GetShinyValue('compswap')
    print(paste0('swap : ', paste(rv$swap, collapse = ' ')))
  })

  observeEvent(input$validate, {
    
    # Manage the keep option
    ind.delete <- which(rv$keep == FALSE)
    if (length(ind.delete) > 0){
      rv$comp.names <- colnames(rv$listComp$logFC)[-ind.delete]
      rv$listComp$logFC <- as.data.frame(rv$listComp$logFC[ , -ind.delete])
      colnames(rv$listComp$logFC) <- rv$comp.names
      
      tmp <- colnames(rv$listComp$P_Value)[-ind.delete]
      rv$listComp$P_Value <-  as.data.frame(rv$listComp$P_Value[ , -ind.delete])
      colnames(rv$listComp$P_Value) <- tmp
      
      rv$swap <- rv$swap[-ind.delete]
      rv$keep <- rv$keep[-ind.delete]
      rv$n <- ncol(rv$listComp$logFC)
    }

    # Manage the swap option
    ind.swap <- which(rv$swap == TRUE)
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
    print(str(rv$listComp))
    for (i in seq_len(rv$n)){
      shinyjs::toggle(paste0("compswap", i), condition = FALSE)
      shinyjs::toggle(paste0("compkeep", i), condition = FALSE)
    }
    shinyjs::toggle('keep_title', condition = FALSE)
    shinyjs::toggle('swap_title', condition = FALSE)
  })
  
})

# Run the application
shinyApp(ui = ui, server = server)