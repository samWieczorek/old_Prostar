
mod_filtering_example_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    actionButton(ns("show_filtering_example"), "Preview filtering"),
    shinyBS::bsModal(ns("example_modal"),
                     title="Example preview of the filtering result.",
                     size = "large",
                     trigger = ns("show_filtering_example"),
                     tagList(
                       p("The darkened lines are those which respect the query."),
                       dataTableOutput(ns("example_tab"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-footer{ display:none}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-dialog{ width:1000px}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-body{ min-height:700px}")))
                     )
  )
}





mod_filtering_example_server <- function(id, params) {
  
  qdata <- data.frame(A1 = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     A2	= c(1,  1, NA, NA, NA, NA, NA,  1,  1, NA),
                     A3 = c(1, 1,  1,  NA, NA, NA, NA,  1,  1,  1),
                     B1 = c(1,  1, 1,   1, NA, NA, NA, NA, NA, NA),
                     B2 = c(1,  1, 1,   1,  1, NA, NA,  1, NA, NA),
                     B3 = c(1,  1, 1,   1,  1,  1, NA,  1,  1,  1)
  )
  #plop[plop==1] <- sample(10, 1)
  
  metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
  colnames(metadata_plop) <- c("Sample.name", "Condition", "Bio.Rep")
  metadata_plop$Sample.name <- colnames(qdata)
  metadata_plop$Condition <- c(rep("c1", 3),rep("c2", 3))
  metadata_plop$Bio.Rep <- c(1:6)
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
  
  
  
  # ###############
  # # options modal
  jqui_draggable(paste0("#","example_modal"," .modal-content"),
                 options = list(revert=FALSE)
  )
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
    
    # change example tab after chooseMetacellTag user choice
    # if (params()$MetacellTag != "None"){
    #   if (params()$MetacellTag %in% c("missing", "missing POV", "missing MEC")){
    #     plop_msnset[Biobase::exprs(plop_msnset) != 1] <- params()$MetacellTag  
    #   } else {
    #     plop_msnset[plop_msnset!="na"] <- params()$MetacellTag  
    #   }
    # }
    # 
    
    
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
    
    if (!is.null(index)){
      df <- getDataForExprs(obj, NULL)
      range <- ((ncol(df)/2)+1):ncol(df)
      for (i in index)
        df[i, range] <- paste0('darken_', df[i, range] )
      
      
      c.tags <- BuildColorStyles(obj, colorsTypeMV)$tags
      c.colors <-  BuildColorStyles(obj, colorsTypeMV)$colors
      c.tags <- c(c.tags, paste0('darken_', c.tags))
      c.colors <- c(c.colors, DarkenColors(c.colors))
      
      DT::datatable(df,
                    options = list(
                      paging = FALSE,
                      searching = FALSE,
                      columnDefs = list(list(targets = c(((ncol(df)/2)+1):ncol(df)), 
                                             visible = FALSE)))) %>%

        formatStyle(
          colnames(df)[1:(ncol(df)/2)],
          colnames(df)[((ncol(df)/2)+1):ncol(df)],
          backgroundColor = styleEqual(c.tags, c.colors),
          backgroundSize = '98% 48%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    } else {
      p('No filtering with these parameters.')
      
      DT::datatable(obj,
                    options = list(
                      paging = FALSE,
                      searching = FALSE)
      )              
      
    }
  })

  
    }
)
}

