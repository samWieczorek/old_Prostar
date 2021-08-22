
mod_filtering_example_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    actionButton(ns("show_filtering_example"), "Preview filtering", class = actionBtnClass ),
    shinyBS::bsModal(ns("example_modal"),
                     title="Example preview of the filtering result.",
                     size = "large",
                     trigger = ns("show_filtering_example"),
                     tagList(
                       uiOutput(ns('show_text')),
                       radioButtons(ns('run_btn'), 'Example dataset',
                                    choices = setNames(nm=c('original dataset', 'simulate filtered dataset'))),
                       dataTableOutput(ns("example_tab_filtered"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-footer{ display:none}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-dialog{ width:1000px}"))),
                     tags$head(tags$style(paste0("#", ns('example_modal'), " .modal-body{ min-height:700px}")))
                     )
  )
}





mod_filtering_example_server <- function(id, obj, indices, params, txt) {
  
  
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
 
      # colorsTypeMV = list(MEC = 'orange', 
      #                     POV = 'lightblue',
      #                     identified = 'white',
      #                     recovered = 'lightgrey',
      #                     combined = 'red')
      
      legendTypeMV = list(MEC = 'Missing in Entire Condition (MEC)', 
                          POV = "Partially Observed Value (POV)",
                          identified = 'Identified',
                          recovered = 'Recovered',
                          combined = 'Combined')
  

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
  
  output$example_tab_filtered <- DT::renderDataTable({
    df <- getDataForExprs(obj(), NULL)
    c.tags <- BuildColorStyles(obj())$tags
    c.colors <-  BuildColorStyles(obj())$colors
    range.invisible <- ((ncol(df)/2)+1):ncol(df)
    

    if (!is.null(indices()) && input$run_btn == 'simulate filtered dataset'){
      if (params()$KeepRemove == "keep")
        index2darken <- (1:nrow(obj()))[-indices()]
      else if (params()$KeepRemove == "delete")
        index2darken <- indices()
      
      for (i in index2darken)
        df[i, range.invisible] <- paste0('darken_', df[i, range.invisible] )
      
      c.tags <- c(c.tags, paste0('darken_', c.tags))
      c.colors <- c(c.colors, DarkenColors(c.colors))
    }
    
      DT::datatable(df,
                    #rownames = FALSE,
                    extensions = c('Scroller'),
                    options = list(
                      dom = 'Brtip',
                      pageLength = 15,
                      orderClasses = TRUE,
                      autoWidth=TRUE,
                      deferRender = TRUE,
                      bLengthChange = FALSE,
                      scrollX = 200,
                      scrollY = 500,
                      scroller = TRUE,
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

