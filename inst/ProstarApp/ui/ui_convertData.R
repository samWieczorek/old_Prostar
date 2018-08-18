tabPanel("Convert data",
        value = "convertTab",
        uiOutput("checkConvertPanel" ),
        actionButton("prevBtnConvert", "< Previous"),
        actionButton("nextBtnConvert", "Next >"),
        hr(),
        uiOutput("Convert_SelectFile"),
        uiOutput("Convert_DataId"),
        uiOutput("Convert_ExpFeatData"),
        uiOutput("Convert_BuildDesign"),
        uiOutput("Convert_Convert")
        
)
