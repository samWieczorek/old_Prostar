tabPanel("Convert data",
         value = "convertTab",
         uiOutput("checkConvertPanel" ),
         actionButton("prevBtnConvert", "< Previous", class = PrevNextBtnClass),
         actionButton("nextBtnConvert", "Next >", class = PrevNextBtnClass),
         hr(),
         uiOutput("Convert_SelectFile"),
         uiOutput("Convert_DataId"),
         uiOutput("Convert_ExpFeatData"),
         uiOutput("Convert_BuildDesign"),
         uiOutput("Convert_Convert")
)

