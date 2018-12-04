tabPanel("Convert data",

         value = "convertTab",
         tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("prevBtnConvert", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
                  tags$div( style="align: center;display:inline-block; vertical-align: top;",
                            uiOutput("checkConvertPanel" )),
                  tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                            actionButton("nextBtnConvert", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%'))
                  
                  
         ),
         
   
         hr(),
         uiOutput("Convert_SelectFile"),
         uiOutput("Convert_DataId"),
         uiOutput("Convert_ExpFeatData"),
         uiOutput("Convert_BuildDesign"),
         uiOutput("Convert_Convert")

)

