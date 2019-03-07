

server <- function(input, output, session){
  
  
  source(file.path(".", "moduleA.R"),  local = TRUE)$value
  source(file.path(".", "moduleB.R"),  local = TRUE)$value
  source(file.path(".", "moduleC.R"),  local = TRUE)$value
  source(file.path(".", "modulePlots.R"),  local = TRUE)$value
  
  
  pipeline_pep <- reactiveValues(
    original = 10,
    A_processed = NULL,
    B_processed = NULL,
    C_processed = NULL
  )
  
  
  rv <- reactiveValues(
    obj = 10,
    current.obj = 10,
    returnVal = NULL,
    
    screenA.id = 1,
    screenB.id = 1,
    screenC.id = 1)
  
  
  processA  <- callModule(module=moduleA, 'processA', 
                                          dataIn=reactive({rv$current.obj}),
                                          screen.id = reactive({rv$screenA.id}))
  
  processB  <- callModule(module=moduleB, 'processB', 
                                          dataIn=reactive({rv$current.obj}),
                                          screen.id = reactive({rv$screenB.id}))
  
  processC <- callModule(module=moduleC, 'processC', 
                                          dataIn=reactive({rv$current.obj}),
                                          screen.id = reactive({rv$screenC.id}))
  
  

  observeEvent(input$navPage,{
    print(paste0("input$navPage = ", input$navPage))
    print(paste0("Event on navPage : ", input$navPage))
    screen.id <- 1
    switch(input$navPage,
           
           ProcessA=
             {
               print("Select Menu Process A")
               if (!is.null(pipeline_pep$A_processed)) {
                 rv$screenA.id <- 2
               }
              print("END OF Select Menu Process A")
               },
           
           
           ProcessB=
             {
               print("Select Menu Process B")
               if (!is.null(pipeline_pep$B_processed)) {
                 rv$screenB.id <- 2
               }
                print("END OF Select Menu Process B")
               },
           
           
           ProcessC=
                {
                  print("Select Menu Process C")
                  if (!is.null(pipeline_pep$C_processed)) {
                    rv$screenC.id <- 2
                  }
                  print("END OF Menu Process C")
                }
           )
    print(paste0("END OF Event on navPage : ", input$navPage))
    
  })

  
  #
  
  
  
  # Delete <- reactive({
  #   pipeline_pep$A_processed <- NULL
  #   pipeline_pep$A_processed
  # })
  
  observeEvent(input$rst_process, {
    print("In reset process event")
    #pipeline_pep$A_processed <- Delete()
    #pipeline_pep$B_processed <- reactive({NULL})
    #pipeline_pep$C_processed <- NULL
  })
  
  
  observeEvent(rv$obj, {
    print('maj :  pipeline_pep$original <- rv$obj')
    #print(paste0("rv$returnVal() = ", rv$returnVal()))
    pipeline_pep$original <- rv$obj
    rv$current.obj <- pipeline_pep$original
  })
  
  
  
  observeEvent(processA(), {
    print('### EVENT ON : pipeline_pep$A_processed <- rv$obj')
    rv$current.obj <- processA()
    pipeline_pep$A_processed <- processA()
    
    pipeline_pep$B_processed <- NULL
    pipeline_pep$C_processed <- NULL
    
  })

  observeEvent(processB(), {
    print('### EVENT ON : pipeline_pep$B_processed <- rv$obj')
    rv$current.obj <- processB()
    pipeline_pep$B_processed <- processB()
    pipeline_pep$C_processed <- NULL
  })

  observeEvent(processC(), {
    print('### EVENT ON : pipeline_pep$C_processed <- rv$obj')
    rv$current.obj <- processC()
    pipeline_pep$C_processed <- processC()
  })
  
  # observeEvent(rv$b(), {
  #   print(paste0("resultat de callModule : ",rv$b()))
  # })
  
  
  
  output$summary <- renderUI({
   
    #rv$obj
    # rv$current.obj()
    # rv$resModuleA()
    # rv$resModuleB()
    # rv$resModuleC()
    # 
    # pipeline_pep$A_processed
    # pipeline_pep$B_processed
    # pipeline_pep$C_processed
    # 
    
    tagList(
      p(paste0('rv$obj =',rv$obj)),
      p(paste0('rv$current.obj()= ',rv$current.obj)),
      p(paste0('rv$returnVal()$name = ',rv$returnVal)),
      #p(paste0('rv$returnVal()$res = ',rv$returnVal()$res))
      p(paste0('pipeline_pep$original= ',pipeline_pep$original)),
      p(paste0('pipeline_pep$A_processed= ',pipeline_pep$A_processed)),
      p(paste0('pipeline_pep$B_processed= ',pipeline_pep$B_processed)),
      p(paste0('pipeline_pep$C_processed= ',pipeline_pep$C_processed))
    )
  
  })
}
