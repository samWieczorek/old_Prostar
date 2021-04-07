
mod_filtering_example_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    actionButton(ns("show_filtering_example"), "Preview filtering"),
    shinyBS::bsModal(ns("example_modal"),
                     title="Example preview of the filtering result.",
                     size = "large",
                     trigger = ns("show_filtering_example"),
                     tagList(
                       uiOutput(ns('show_text')),
                       radioButtons(ns('run_btn'), 'Example dataset',
                                    choices = setNames(nm=c('original dataset', 'simulate filtered dataset'))),
                       dataTableOutput(ns("example_tab"))
                       ),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-footer{ display:none}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-dialog{ width:1000px}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-body{ min-height:700px}")))
                     )
  )
}





mod_filtering_example_server <- function(id, params, txt) {
  
  qdata <- data.frame(A_1 = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                      A_2	= c(1,  1, NA, NA, NA, NA, NA,  1,  1, NA),
                      A_3 = c(1, 1,  1,  NA, NA, NA, NA,  1,  1,  1),
                      B_1 = c(1,  1, 1,   1, NA, NA, NA, NA, NA, NA),
                      B_2 = c(1,  1, 1,   1,  1, NA, NA,  1, NA, NA),
                      B_3 = c(1,  1, 1,   1,  1,  1, NA,  1,  1,  1)
  )
  ind <- which(qdata==1, arr.ind=TRUE)
  samples <- round(runif(nrow(ind), min=0, max=100), digits = 2)
  for (i in 1:nrow(ind))
    qdata[ind[i, 1], ind[i, 2]] <- samples[i]
  
  metadata_plop <- data.frame(Sample.name = colnames(qdata),
                              Condition = c(rep("A", 3),rep("B", 3)),
                              Bio.Rep = c(1:6)
  )
#browser()
  obj <- DAPAR::createMSnset(file = qdata,
                             indExpData = c(1:6),
                             indFData = c(1:6), 
                             metadata = metadata_plop,
                             pep_prot_data = "peptide",
                             software = 'maxquant')
  
  Biobase::fData(obj)[Biobase::fData(obj)=='quanti'] <- 'identified'
  
  moduleServer(
    id,
    function(input, output, session) {
  
  
  output$show_text <- renderUI({
    h3(txt())
  })
  
  
  # ###############
  # # options modal
  # jqui_draggable(paste0("#","example_modal"," .modal-content"),
  #                options = list(revert=FALSE)
  # )
  # ###############
 
      colorsTypeMV = list(MEC = 'orange', 
                          POV = 'lightblue',
                          identified = 'white',
                          recovered = 'lightgrey',
                          combined = 'red')
      
      legendTypeMV = list(MEC = 'Missing in Entire Condition (MEC)', 
                          POV = "Partially Observed Value (POV)",
                          identified = 'Identified',
                          recovered = 'Recovered',
                          combined = 'Combined')
  
  
  ComputenMetacellFilteringIndexes <- reactive({
    
    if (!(params()$MetacellFilters %in% c("None", "WholeLine"))){
      th <- NULL
      if (params()$val_vs_percent == 'Value'){
        th <- as.integer(params()$metacell_value_th)
      } else {
        th <- as.numeric(params()$metacell_percent_th)
      }
    }
    
    level <- obj@experimentData@other$typeOfData
    pattern <- params()$MetacellTag
    type <- params()$MetacellFilters
    percent <- params()$val_vs_percent == 'Percentage'
    op <- params()$metacellFilter_operator
    conds <-  Biobase::pData(obj)$Condition
    
    index <- NULL
    
    mask <- match.metacell(metadata=DAPAR::GetMetacell(obj), 
                           pattern=pattern, 
                           level=level)
    
    
    
    index <- switch(params()$MetacellFilters,
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
    
    
    
    if(params()$MetacellFilters != "None" &&
       params()$KeepRemove == "keep"){
      if(!is.null(index)) {
        index <- (1:nrow(obj))[-index]
      } else {
        index <- 1:nrow(obj)
      }
    }
    
    index
  })
  
  
  
  rgb2col = function(rgbmat){
    ProcessColumn = function(col){
      rgb(rgbmat[1, col], 
          rgbmat[2, col], 
          rgbmat[3, col], 
          maxColorValue = 255)
    }
    sapply(1:ncol(rgbmat), ProcessColumn)
  }
  
  
  
  DarkenColors <- function(ColorsHex){
    # Convert to rgb
    # This is the step where we get the matrix
    ColorsRGB = col2rgb(ColorsHex)
    
    # Darken colors by lowering values of RGB
    ColorsRGBDark = round(ColorsRGB * 0.5)
    
    # Convert back to hex
    ColorsHexDark = rgb2col(ColorsRGBDark)
    
    return(ColorsHexDark)
    
  }
  
  output$example_tab <- DT::renderDataTable({
    index <- ComputenMetacellFilteringIndexes()
    df <- getDataForExprs(obj, NULL)
    c.tags <- BuildColorStyles(obj, colorsTypeMV)$tags
    c.colors <-  BuildColorStyles(obj, colorsTypeMV)$colors
    range.invisible <- ((ncol(df)/2)+1):ncol(df)
    
    if (!is.null(index) && input$run_btn == 'simulate filtered dataset'){
      for (i in index)
        df[i, range.invisible] <- paste0('darken_', df[i, range.invisible] )
      c.tags <- c(c.tags, paste0('darken_', c.tags))
      c.colors <- c(c.colors, DarkenColors(c.colors))
    }
    
      DT::datatable(df,
                    #rownames = FALSE,
                    options = list(
                      dom = 't',
                      server = FALSE,
                      columnDefs = list(list(targets = range.invisible, 
                                             visible = FALSE)))
                    ) %>%

        formatStyle(
          colnames(df)[1:(ncol(df)/2)],
          colnames(df)[range.invisible],
          backgroundColor = styleEqual(c.tags, c.colors)
        )

  })

  
    }
)
}

