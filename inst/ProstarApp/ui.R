

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
    
    
    shinyjs::useShinyjs(),
    #,tags$head(includeScript("google-analytics.js"))
    tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
    
    inlineCSS(appCSS),
    
    # Loading message
    div(
        br(),br(),br(),br(),br(),br(),
        id = "loading-content",
        h2("Prostar is loading, please wait...")
    ),

    
    
    
titlePanel("", windowTitle = "Prostar"),
sidebarPanelWidth()
,includeCSS("www/progressBar/progressBar.css")
,includeScript("www/progressBar/ShinyProgress2.js")
,includeCSS("www/css/prostar.css")


#,source(file.path("ui", "ui_navbarPage.R"),  local = TRUE)$value
,navbarPage(theme = shinytheme("cerulean")
            
            ,id = "navPage"
            #,""
            ,absolutePanel(
              id  = "#AbsolutePanel",
              top = 0,
              right = 50,
              width = "500px",
              height = "50px",
              draggable = FALSE,
              fixed = FALSE,
              cursor = "default"
              ,uiOutput("datasetAbsPanel" )
            )
            ,navbarMenu("Prostar",
                        source.file("ui_Home.R"),
                        source.file("ui_Settings.R")
                        )
            ,navbarMenu("Dataset manager",
                        source.file("ui_OpenMSnSetFile.R"),
                        source.file("ui_ConvertData.R"),
                        source.file("ui_DemoMode.R"),
                        source.file("ui_Export.R"),
                        source.file("ui_LogSession.R")
                        )
            ,navbarMenu("Help",
                        source.file("ui_UsefulLinks.R"),
                        source.file("ui_ReleaseNotes.R"),
                        source.file("ui_FAQ.R"),
                        source.file("ui_CheckForUpdates.R")
                        )
))
