tabPanel("GO Analysis",
tabsetPanel(
    id = "tabsetPanelGO",
    tabPanel("GO Setup",
             #uiOutput("getUniprotIDCol"),
             uiOutput("chooseColForProtID"),
             
            fileInput("UNIPROTID_File", "Choose UNIPROT ID file"),
            uiOutput("infoIDProt_NA"),
            selectInput("Organism", "Genome Wide Annotation", choices = list_org_db),
             
             selectInput("Ontology", "Ontology",
                         choices = c("Molecular Function (MF)"="MF" , 
                                     "Biological Process (BP)" = "BP", 
                                     "Cellular Component (CC)" = "CC")),
            
            dataTableOutput("nonIdentifiedProteins")
             ),
    tabPanel("GO Classification",
             id = "tabPanelGOClassif",
             sidebarCustom(),
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "sidebar_GO",
                               height = "100%",
                               numericInput("GO_level", "Level",min = 0, max = 10, step = 1, value = 2)
                               
                            ),
                      tagList(
                      busyIndicator("Calculation in progress",wait = 0),
                      plotOutput("GOplotGroup"),
                      dataTableOutput("GODatatable")
                      
                      )
                     
                     
         )
    ),
    tabPanel("GO Enrichment",
        id = "tabPanelEnrichGO",
        sidebarCustom(),
        splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_GO1",
                              height = "100%",
                              radioButtons("universe", "Universe", choices = c("Entire organism" = "Entire organism",
                                                                               "Entire dataset" = "Entire dataset",
                                                                               "Custom" = "Custom")),
                              uiOutput("chooseUniverseFile"),
                              selectInput("PAdjustMethod", "P Adjust Method",choices = c("BH", "fdr", "none")),
                              numericInput("pvalueCutoff", "p-Value cutoff", min = 0, max = 1, step = 0.01, value = 0.01),
                              

                              actionButton("perform.GO.button",
                                           "Perform analysis"),
                              busyIndicator("Calculation in progress",wait = 0),

                              actionButton("ValidGO",
                                           "Save analysis",
                                           styleclass = "primary")
                    ),
                    tagList(
                        busyIndicator("Calculation in progress",wait = 0),
                        plotOutput("GObarplotEnrich"),
                        plotOutput("GOdotplotEnrich")
                        #plotOutput("GOEnrichMap")
                        
                        #dataTableOutput("GODatatableEn")

                    )


        )
     )
)
)