tabPanel("Aggregate and Test",
         value = "testPeptideTab",
        selectInput('chooseTest', "Choose test", 
                    choices=c("None" = "None", 
                              "Aggregate"="Aggregate", 
                              'peptidomic test'='peptidomictest', 
                              'peptide-level protein test'='peptidelevelproteintest')),
        uiOutput('aggregate_UI'),
        uiOutput('peptidomicTest_UI'),
        uiOutput('peptidelevelproteintest_UI')
         
)