



GetDatasetOverview <- reactive({
  print("In GetDatasetOverview")
  print(pipeline$current.obj)
  req(pipeline$current.obj)
  
  obj <- pipeline$current.dataset[[pipeline$current.indice]]
  
  columns <- c("Number of samples","Number of conditions",
               "Number of lines", "Number of missing values", "% of missing values", 
               "Number of empty lines")
  
  do <- data.frame(Definition= columns,
                   Value=rep(0,length(columns)))
  
  NA.count<- length(which(is.na(Biobase::exprs(obj)==TRUE)))
  pourcentage <- 100 * round(NA.count/(ncol(obj)*nrow(obj)), digits=4)
  nb.empty.lines <- sum(apply(
    is.na(as.matrix(Biobase::exprs(obj))), 1, all))
  
  
  val <- c(ncol((Biobase::exprs(obj))),
           length(unique(Biobase::pData(obj)$Condition)),
           nrow((Biobase::exprs(obj))),
           NA.count,
           pourcentage,
           nb.empty.lines)
  do$Value <- val
  
  do
})


GetDatasetOverview2 <- function(obj){
  req(obj)
  
  columns <- c("Number of samples","Number of conditions",
               "Number of lines", "Number of missing values", "% of missing values", 
               "Number of empty lines")
  
  do <- data.frame(Definition= columns,
                   Value=rep(0,length(columns)))
  
  NA.count<- length(which(is.na(Biobase::exprs(obj)==TRUE)))
  pourcentage <- 100 * round(NA.count/(ncol(obj)*nrow(obj)), digits=4)
  nb.empty.lines <- sum(apply(
    is.na(as.matrix(Biobase::exprs(obj))), 1, all))
  
  
  val <- c(ncol((Biobase::exprs(obj))),
           length(unique(Biobase::pData(obj)$Condition)),
           nrow((Biobase::exprs(obj))),
           NA.count,
           pourcentage,
           nb.empty.lines)
  do$Value <- val
  
  return(do)
}


initComplete <- function(){
  
  return (JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
    "}"))
}



# function to read DT inputs
shinyValue <- function(id,num) {
  unlist(lapply(seq_len(num),function(i) {
    value <- input[[paste0(id,i)]]
    if (is.null(value)) NA else value
  }))
}





buildTable <- function(text, color, colorCurrentPos){
  paste0("     ", text, "     ")
  rows.color <- rows.text <-  rows.cursor <- list()
  rows.text <- list()
  for( i in 1:length( color ) ) {
    rows.color[[i]] <-lapply( color[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 20px;" ) ))
    rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 5px;" ) ))
    rows.text[[i]] <- lapply( text[i], function( x ) tags$td( x ) ) 
  }
  
  html.table <-  tags$table(style = "width: 100%; text-align: center;border: 1;border-collapse: separate;border-spacing: 10px;padding-top: 0px;",
                            tags$tr( rows.color ),
                            tags$tr( rows.cursor ),
                            tags$tr( rows.text )
  )
  return(html.table)
  
}





shinyOutput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function for dynamic inputs in DT
shinyInput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}




# Call this function with all the regular navbarPage() parameters, plus a text parameter,
# if you want to add text to the navbar
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}







######
### Miscelllaneous functions


#' busyIndicator
busyIndicator <- function(text = "Calculation in progress..",
                          img = "images/ajax-loader.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet",
                type="text/css",href="busyIndicator/busyIndicator.css")
    ))
    ,div(class="busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.busy-indicator').show()
      }
      }, %d)
      } else {
      $('div.busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )
  }



SetCustomCSS <- function(){
  inlineCSS(".body { font-size:14px;}")
  tags$head(includeCSS("www/css/arrow.css"))
  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>"))
  tags$head(tags$style(".modal-dialog{ width:200px}"))
  tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}")))
  includeCSS("www/css/prostar.css")
  #,includeCSS("www/css/fontawesome.css")
  inlineCSS(".body { font-size:14px;}")
  inlineCSS(".rect {float: left;
            width: 100px;
            height: 20px;
            margin: 2px;
            border: 1px solid rgba(0, 0, 0, .2);}")
  inlineCSS(".green {background: #06AB27}")
  inlineCSS(".red {background: #C90404}")
  inlineCSS(".grey {background:lightgrey;}")
  inlineCSS(".modal-backdrop {z-index: 1000}")
  }


