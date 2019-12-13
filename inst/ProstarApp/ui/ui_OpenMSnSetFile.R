tabPanel("Open MSnset file",
         value = "openMSnsetTab",
         tagList(
           uiOutput("openMSnsetScreen"),
           actionButton("loadMSnset", "Load MSnset",class = actionBtnClass),
           uiOutput("updateDesign")
        )
                     
)