options(shiny.trace=TRUE)
options(shiny.reactlog=TRUE)

customSidebarPanel <- function (..., width = 3) 
{
div(class = paste0("span", width), tags$form(class = "well",...))
}

library(DAPAR)
library(shiny)
library(shinyTree)
library(rhandsontable)
library(data.table)




shinyUI <- function(){
    pageWithSidebar(

    headerPanel(
        list("ProStaR"),windowTitle="ProStaR"
    ),

    # Add custom CSS & Javascript;
    sidebarPanel(
        width=3,
        tags$head(JSCSSTags()),

    #  busyIndicator(wait = 1000)
#    div(class = "busy",  
            #p("Calculation in progress.."), 
#            busyIndicator(wait = 2)
#            ),

        uiOutput("fileopened"),
        shinyTree("tree"),

        hr(),
        h5("Available datasets"),
        selectInput("datasets", "", choices = "none", width = '200px'),
        actionButton("GetDataset","Refresh dataset", styleclass = "primary"),
        actionButton("ClearDataset", "Clear all", styleclass = "primary")
        ),

    #  ---    MAIN PANEL     ----
    mainPanel(
        tags$head(
            tags$style(type="text/css", ".tab-content {overflow: visible;}")),
        dataTableOutput("toto"),
        uiOutput("test")
    )# end MainPanel

)
}
