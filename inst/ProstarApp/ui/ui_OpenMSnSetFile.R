tabPanel("Open MSnset file",
         value = "openMSnsetTab",
         tagList(
           uiOutput("openMSnsetScreen"),
           actionButton("loadMSnset", "Load MSnset",class = actionBtnClass),
           #uiOutput("updateDesign")
           p("Once the 'Load' button (above) clicked, you will be automatically redirected to Prostar home page. The dataset will be accessible within Prostar 
    interface and processing menus will be enabled. However, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
    (because successive dataset loading can make Prostar unstable). To work on another dataset, use first the 'Reload Prostar' functionality from 
    the 'Dataset manager' menu: it will make Prostar restart with a fresh R session where import functions are enabled.")
        )
                     
)