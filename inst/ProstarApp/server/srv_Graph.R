#callModule(moduleStaticDataTable,"OneOneDT", table2show=reactive({BuildOne2OneTab()}))
#callModule(moduleStaticDataTable,"OneMultiDT", table2show=reactive({BuildOne2MultiTab()}))

callModule(module = moduleGraphMulti2Any, "CC_Multi_Any", 
           cc=reactive({rv$CC$allPep[Get_CC_Multi2Any()]}),
           tooltip=reactive({NULL})
           )


# 
# output$CC_One_One <- renderUI({
#   
#   moduleStaticDataTableUI("OneOneDT")
# })
# 
# output$CC_One_Multi <- renderUI({
#   
#    moduleStaticDataTableUI("OneMultiDT")
# })
# 


Get_CC_One2One <- reactive({
  rv$CC$allPep
  ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
  ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
  ll.prot.one2one <- intersect(which(ll.prot == 1),which(ll.pept == 1))
  print(paste0("In Get_CC_One2One:  ", length(ll.prot.one2one)))
  ll.prot.one2one
})

Get_CC_One2multi <- reactive({
  rv$CC$allPep
  ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
  ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
  ll.prot.one2multi <- intersect(which(ll.prot == 1),which(ll.pept > 1))
  print(paste0("In Get_CC_One2multi:  ", length(ll.prot.one2multi)))
  ll.prot.one2multi
})

Get_CC_Multi2Any <- reactive({
  rv$CC$allPep
  ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
  ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
  ll.prot.multi2any <- which(ll.prot > 1)
  print(paste0("In Get_CC_Multi2Any:  ", length(ll.prot.multi2any)))
  ll.prot.multi2any
})



BuildOne2OneTab <- reactive({
  rv$CC$allPep
  table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_One2One()],function(x){data.frame(rbind(x))}))
  print(paste0("In BuildOne2OneTab:  ", nrow(table)))
  table
})

BuildOne2MultiTab <- reactive({
  rv$CC$allPep
  table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_One2multi()],function(x){data.frame(rbind(x), nPep = length(x$peptides))}))
  table <- table[c('proteins', 'nPep', 'peptides')]
  print(paste0("In BuildOne2MultiTab:  ", nrow(table)))
  table
})


BuildMulti2AnyTab <- reactive({
  rv$CC$allPep
  table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_Multi2Any()],function(x){data.frame(rbind(x), nPep = length(x$peptides))}))
  table <- table[c('proteins', 'nPep', 'peptides')]
  
  print(paste0("In BuildMulti2AnyTab:  ", nrow(table)))
  table
})





output$OneMultiDT <- renderDataTable({
    req(rv$CC$allPep)
    
      dat <- DT::datatable(BuildOne2MultiTab(),
                           rownames=FALSE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom='Bfrtip',
                                        pageLength=DT_pagelength,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2))))))

    return(dat)
})



output$OneOneDT <- renderDataTable({
  req(rv$CC$allPep)
  
  dat <- DT::datatable(BuildOne2OneTab(),
                       rownames=FALSE,
                       extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                       options=list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    pageLength=DT_pagelength,
                                    deferRender = TRUE,
                                    bLengthChange = FALSE,
                                    scrollX = 200,
                                    scrollY = 600,
                                    scroller = TRUE,
                                    orderClasses = TRUE,
                                    autoWidth=FALSE,
                                    columns.searchable=F,
                                    fixedColumns = list(leftColumns = 1),
                                    columnDefs = list(list(columns.width=c("60px"),
                                                           columnDefs.targets=c(list(0),list(1),list(2))))))
  
  return(dat)
})