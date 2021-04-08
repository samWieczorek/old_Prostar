<<<<<<< HEAD

=======
# tabPanel("Convert data",
# 
#          value = "convertTab",
#          moduleProcessUI("moduleProcess_Convert")
# 
# )
>>>>>>> 3f7a010f9977e8f1f29597873b6afa82b0158f85

tabPanel("Convert data",
         value = "convertTab",
         
         tabsetPanel(
           id = "tabsetPanel_convert",
           tabPanel("1 - Select file",value = "SelectFile2Import",
                    uiOutput("Convert_SelectFile")),
           tabPanel("2 - Data id", value = "ID",
                    uiOutput("Convert_DataId")),
           tabPanel("3 - Exp. and feat. data",value = "Import1",
                    uiOutput("Convert_ExpFeatData")),
           tabPanel("4 - Samples metadata",value = "buildDesign_Tab",
                    uiOutput("Convert_BuildDesign")),
           tabPanel("5 - Convert",value = "Convert",
                    uiOutput("Convert_Convert"))
         )
)