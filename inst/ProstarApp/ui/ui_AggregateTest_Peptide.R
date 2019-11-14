


tabPanel("Aggregate and Test",
         value = "testPeptideTab",
        selectInput('chooseTest', "Choose test", 
                    choices=c("None" = "None", 
                              "Aggregate"="Aggregate", 
                              'peptidomic test'='peptidomicTest', 
                              'peptide-level protein test'='peptideTest')),
        shinyjs::hidden(div(id="moduleProcess_Aggregation_UI",moduleProcessUI("moduleProcess_Aggregation"))),
        shinyjs::hidden(div(id="moduleProcess_HypothesisTestPeptidomic_UI",moduleProcessUI("moduleProcess_HypothesisTestPeptidomic"))),
        shinyjs::hidden(div(id="moduleProcess_HypothesisTestPeptide_UI",moduleProcessUI("moduleProcess_HypothesisTestPeptide")))
        
         
)