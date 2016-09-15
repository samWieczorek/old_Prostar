options(shiny.trace=FALSE)
options(shiny.reactlog=TRUE)


library(DAPAR)
library(DAPARdata)
library(shiny)
library(rhandsontable)
library(data.table)
library(shinyjs)
library(shinyAce)


heightSidebarPanel <- "600px"
test <- "Prostar"

widthLeftPanel <- "300px"
widthRightPanel <- "70%"
widthWellPanel <- "80%"
heightWellPanel <- "200px"

plotWidth <- "800px"
plotHeight <- "600px"

sidebarCustom <- function(){

tags$head(


    tags$style(type="text/css", 
    paste("#wellPanelFileOpen { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css",
    paste("#sidebarPanel_changeDataset { height:",heightSidebarPanel,"; }",
        sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_dataExplorer { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Corrmatrix { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css",
    paste("#sidebar_heatmap { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_boxplot { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css",
    paste("#sidebar_densityplot { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Filter1 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Filter2 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Filter3 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Normalization { height:",heightSidebarPanel,
        "; z-index:1000;}", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_Aggregation { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_imputation { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_DiffAna1 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_DiffAna2 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_DiffAna3 { height:",heightSidebarPanel,"; }", sep="")),
    tags$style(type="text/css", 
    paste("#sidebar_DiffAna4 { height:",heightSidebarPanel,"; }", sep="")),
    
    
    tags$style(type="text/css", 
               paste("#chooseDatasetFromDAPARdata_wellPanel { height:",heightSidebarPanel,"; }", sep="")),
    
    
    tags$style(type="text/css", "#DS { padding-top:50px;"),
    tags$style(type="text/css", "#sidebar_dataExplorer { padding-top:50px;"),
    
    tags$style(type="text/css", 
    "#wellPanelMVFilterTab1 { width: 800px; 
                                background-color:transparent;"),
    tags$style(type="text/css", 
        "#condPanelShowOptions {background-color:#f5f5f5; 
                                opacity:0.90;
                                border:1px solid #e3e3e3;
                                border-radius:4px;"),
    
    tags$style(type="text/css", "#AbsolusssstePanel { 
            background-color:orange;"),
    tags$style(type="text/css", "#wellPanlNormalization { 
            z-index:-1;overflow: visible;"),
    tags$style(type="text/css", "#DS_tabOverviewMV { 
            width:800px;"),

    tags$style(type="text/css", "#wellPanelHeatmap { 
            width: 500px;"),
    tags$style(type="text/css", "#wellPanel_Agregation { 
            width: 800px;"),
    tags$style(HTML('.action-button{
            background-color:lightblue}'))
    
    
)
}





sidebarPanelWidth <- function(){
tags$head(
    tags$style(type="text/css", "#fileopened { 
                font-weight:bold; 
                font-size:100%; 
                color:black; }")
    )
}



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(

titlePanel("", windowTitle = "Prostar"),
sidebarPanelWidth()
,includeCSS("www/progressBar/progressBar.css")
,includeScript("www/progressBar/ShinyProgress2.js")
,useShinyjs()
#,tags$head(includeScript("google-analytics.js"))
#,tags$head(includeScript("piwik.js"))



,uiOutput("disableAggregationTool")
,navbarPage(
    id = "navPage",
    absolutePanel(id  = "#AbsolutePanel",
                top = 10,
                right = 300,
                # bottom = 600,
                # left = "600px",
                width = "200px",
                height = "50px",
                draggable = FALSE,
                fixed = FALSE,
                cursor = "default",
                selectInput("datasets", 
                            "Dataset versions", 
                            choices = list("None"=""), 
                            width = '250px')
    ),

    "",


            tabPanel(test,
                    #id = "about",
                    # img(src="images/cover.jpg"
                    #     , width = "300px"
                    #     , height="300px"
                    #     ),
                    # 
                    uiOutput("aboutText")
                    ),
    
navbarMenu("Dataset manager",
           #id = "datasetManagerMenu"
    tabPanel("Open MSnset file",
        #title="Open a MSnset file",
        #icon = icon("file"),
        #id = "openMSnSet",
        value = "open",
        sidebarCustom(),
        splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
            wellPanel(id = "wellPanelFileOpen"
                ,fileInput("file", 
                        "Open a MSnset file",
                        multiple = FALSE)
            ),
            conditionalPanel(id = "wellPanelOpenFile",
                condition = "true",
                h3("Quick overview of the dataset"),
                uiOutput("overview"),
                uiOutput("infoAboutAggregationTool")
            )
        )
    ),
    tabPanel("Convert data",
        icon = icon("download"),
        value = "import",
        #width = widthWellPanel,
        helpText("These steps allow to create a MSnSet file 
            from a tabulated-text file."),
        tabsetPanel(
            #id = "tabImport",
            #width = widthWellPanel,
            tabPanel(
                #width = widthWellPanel,
                    "1 - Select file",
                    value = "SelectFile2Import",
                    fileInput("file1", "Data file (.txt, .csv, .tsv, .xls, .xlsx files)", 
                            multiple=FALSE, 
                            accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx")),
                            uiOutput("ManageXlsFiles"),
                    helpText("Hint : before importing quantification 
                                file data, check the syntax of your text 
                                file."),
                    br()
                    ,radioButtons("typeOfData", 
                            "Is it a peptide or protein dataset ?", 
                            choices=c("peptide dataset" = "peptide", 
                                    "protein dataset" = "protein")
                    )

                    ,radioButtons("checkDataLogged", 
                            "Are your data already log-transformed ?", 
                            width = widthWellPanel, 
                            choices=c("yes (they stay unchanged)" = "yes", "no (they wil be automatically transformed)"="no"), 
                            selected="no")
                    ,br()
                    ,checkboxInput("replaceAllZeros", 
                                    "Replace all 0 and NaN by NA", 
                                    value= TRUE)
                    ),
    tabPanel( "2 - Data Id",
        value = "ID",
        uiOutput("helpTextDataID"),
        radioButtons("autoID", width="500px",
            "If you choose the automatic ID, Prostar will build an index.", 
            choices=c("Auto ID" = "Auto ID", "User ID" = "user ID")),
        conditionalPanel(
            condition = 'input.autoID == "user ID"',
            uiOutput("id"),
            uiOutput("warningNonUniqueID"))
        ),
    
    tabPanel( "3 - Exp. and feat. data",
            value = "Import1",
            helpText("Select the columns that are quantitation values 
                    by clicking in the field below."),
            div(class="row"),
            div(class="span5", "Quantitative  Data",
                            uiOutput("eData",width = widthWellPanel))
            ),
    
    tabPanel( "4 - Samples metadata",
            value = "Import2",
            #width = widthWellPanel,
            helpText("Warning : it is mandatory that the column 
            \"Label\" is filled."),
            br(),
            rHandsontableOutput("hot"
                                ,width = widthWellPanel
                                ,height = "100%")
            ),
    
    tabPanel( "5 - Convert",
            value = "Convert",
            htmlOutput("msgAlertCreateMSnset"),
            textInput("filenameToCreate",
                        "Enter the name of the study"),
            actionButton("createMSnsetButton","Convert data")
            ,uiOutput("conversionDone")
            )
        )
    ),

    tabPanel("Export",
        value = "export",
        icon = icon("upload"),
        helpText("Choose the export format of the dataset and choose a name."),
        selectInput("fileformatExport", "File format", 
                    choices=  c( "MSnset","Excel")),

        br(),
        helpText("Select the columns you wante to keep as metadata. 
                 By default, in any column is specified, all meta data in your dataset
                 will be exported."),
        #div(class="row"),
        #div(class="span5", "Meta  Data",
            uiOutput("chooseMetaDataExport",width = widthWellPanel),
        br(),br(),
        uiOutput("chooseExportFilename"),
       
        br(),
        downloadButton('downloadMSnSet', 'Download'),
        
         br(),br()
         #radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
         #             inline = TRUE),
         #downloadButton('downloadReport', "Download report")
    ),

    tabPanel("Demo mode",
             id = "demo",
             sidebarCustom(),
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "chooseDatasetFromDAPARdata_wellPanel"
                                   ,uiOutput("chooseDataset")
                                   ,actionButton("loadDemoDataset", "Load demo dataset")
                         ),
                         conditionalPanel(id = "wellPanelOpenFile",
                                          condition = "true",
                                          h3("Quick overview of the dataset"),
                                          uiOutput("overviewDemoDataset"),
                                          uiOutput("showDatasetDoc")
                                         
                                         # uiOutput("infoAboutDemoDataset")
                         )
             )),
tabPanel("Log session",
         value = "ChangeDataset",
         
         
         tabsetPanel(
             #id="LogSession_tabSetPanel",
                     "test",
                     tabPanel("Log session",
                              value = "ChangeDataset",
                              sidebarCustom(),
                              conditionalPanel(
                                  id = "wellPanel_changeDataset",
                                  condition =  "true",
                                  width=widthWellPanel,
                                  DT::dataTableOutput("logSession")
                              )
                     ),
                     tabPanel("R source code", 
                              uiOutput("code")
                     )
         )
)
    ),
    
    
#########################################################    
tabPanel("Descriptive statistics",
        #id="tabView",
        icon = icon("bar-chart-o"),
        tabsetPanel(id="DS_tabSetPanel",
            #------------------------------------------------------------
            tabPanel("Overview",
                value = "DS_tabGeneral",
                
                uiOutput("overviewNewData")
            ),

            tabPanel(
                "Miss. values",
                #id = "DS_tabOverviewMV",
                value = "DS_tabOverviewMV",
                
                helpText("Those bargraph plots display some information to
                     view the distribution of missing values."),
                    fluidRow(
                    column(width = 4, 
                           plotOutput("histoMV_Image_DS")
                           ),
                    column(width = 4, 
                        plotOutput("histo.missvalues.per.lines_DS")),
                    column(width = 4, 
                        plotOutput("histo.missvalues.per.lines.per.conditions_DS"))
                )
            ),

            #-------------------------------------------------------------
            tabPanel(title="Data explorer",
                #id = "DS_DataExplorer",
                sidebarCustom(),
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_dataExplorer",
                        uiOutput("DS_sidebarPanel_tab")
                    ),
                    conditionalPanel(height = heightWellPanel,
                        condition = "true",
                        uiOutput("tabToShow")
                    )
                )
            ),

            tabPanel("Corr. matrix",
                value="DS_tabCorrMatrix",
                sidebarCustom(),
                
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_Corrmatrix",
                        sliderInput("expGradientRate",
                            "Tune to modify the gradient of color",
                            min = 2,max = 6,value = defaultGradientRate,step=0.05)
                    ),
                    conditionalPanel(id = "wellPanelCorrMat",
                                     condition = "true",
                                     plotOutput("corrMatrix",width = plotWidth,
                                                 height = plotHeight)
                    )
                )
            ),

            tabPanel("Heatmap",
                value="DS_tabHeatmap",
                sidebarCustom(),
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_heatmap",
                        uiOutput("DS_sidebarPanel_heatmap")
                    ),
                    conditionalPanel(id = "wellPanelHeatmap",
                        condition = "true",
                        width = 800,
                        HTML("For this view, it is necessary that your dataset 
                            does not contains any NA lines. <br> Please check 
                            your data and use Filtering options or missing 
                            values imputation."),
                        busyIndicator("Calculation in progress",wait = 0),
                        uiOutput("DS_PlotHeatmap")
                    )
                )
            ),

            #-----------------------------------------------------------
            tabPanel("Boxplot",
                value="DS_tabBoxplot",
                sidebarCustom(),
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_boxplot",
                    uiOutput("DS_sidebarPanel_Boxplot")
                    ),
                    conditionalPanel(id = "wellPanelBoxplot",
                        condition = "true",
                        plotOutput("viewBoxPlot_DS",width = plotWidth,
                                    height = plotHeight)
                    )
                )
            ),


            #-----------------------------------------------------------
            tabPanel("Violinplot",
                     value="DS_tabViolinplot",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_Violonplot",
                                           uiOutput("DS_sidebarPanel_Violinplot")
                                 ),
                                 conditionalPanel(id = "wellPanelViolinplot",
                                                  condition = "true",
                                                  plotOutput("viewViolinPlot_DS",width = plotWidth,
                                                             height = plotHeight)
                                 )
                     )
            ),
            
            #-----------------------------------------------------------
            tabPanel("Densityplot",
                value="DS_tabDensityplot",
                sidebarCustom(),
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_densityplot",
                        uiOutput("DS_sidebarPanel_Densityplot")
                    ),
                    conditionalPanel(id = "wellPanelBoxplot",
                        condition = "true",
                        plotOutput("viewDensityplot_DS",
                            width = plotWidth,
                            height = plotHeight)
                    )
                )
            ),

            #-----------------------------------------------------------
            tabPanel("Variance distr.", 
                value="DS_tabDistVar",
                p("This graphics shows, for each condition, the distribution 
                    of the variance of the log-intensities."),
                plotOutput("viewDistVariance",
                    width = plotWidth,
                    height = plotHeight)
            )
    )
),
            
#### NAVBAR MENU - DATA PROCESSING ################################
navbarMenu("Data processing",
           #id = "dataProcessingNvaMenu"
    tabPanel("Filter data",
    icon = icon("download"),
    tabsetPanel(
        #id = "DP_Filtering_tabSetPanel"
        tabPanel( "1 - Missing values",
            #id =  "DP_FilterMissingValues",
            value = "DP_FilterMissingValues",
                sidebarCustom(),
                splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                    wellPanel(id = "sidebar_Filter1"
                        ,uiOutput("DP_sidebar_FilterTab1")
                        
                    ),
                    conditionalPanel(id = "wellPanelMVFilterTab1",
                        condition = "true",
                        HTML("The filter below allows keeping the lines that 
contain a certain amount of quantitative data rather than NA values. <br>
The threshold to define corresponds to the number of quantitative values in a 
line and means that the lines which contain <br> at least this threshold value 
are kept. This filtering threshold may be applied on the whole  dataset, on 
each condition <br> or on at leat one condition."),
                        fluidRow(
                            column(width = 4, 
                                   plotOutput("histoMV_Image")
                                   ),
                            column(width = 4,plotOutput("histo.missvalues.per.lines_Image")),
                            column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions_Image"))
                        )
                    )
                )
        )
        ,tabPanel( "2 - String based filtering",
            id =  "DP_FilterContaminants",
            value = "DP_FilterContaminants",
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_Filter2",
                    uiOutput("DP_sidebar_FilterTab2")
                    
                ),
                conditionalPanel(id = "wellPanelMVFilterTab2",
                    condition = "true",
                    plotOutput("GlobalPieChart")
                )
            )
        )
        ,tabPanel( "3 - Visualize and Validate",
            value = "DP_FilterValidate",
            id = "sidebar_Filter3",
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_Filter3",
                    uiOutput("DP_sidebar_FilterTab3")
                    ,actionButton("ValidateFilters","Save filtered dataset",
                                  styleclass = "primary")
                ),
                conditionalPanel(id = "wellPanelMVFilterTab3"
                    ,condition = "true"
                    ,DT::dataTableOutput("VizualizeFilteredData")
                    ,helpText("After checking the data, 
                            validate the filters")
                )
            )
        )
    )
)

,tabPanel("Normalization",
    value = "Normalization",
        sidebarCustom(),
        splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
            wellPanel(id = "sidebar_Normalization"
                ,height = "100%"
                ,h4("Normalization options")
                ,uiOutput("choose_Normalization_Test")
                ,checkboxInput("plotOptions", "Show plot options", 
                                value = FALSE)
                ,actionButton("perform.normalization", 
                                "Perform normalization", 
                                width="170px")
                ,br(),br()
                ,actionButton("valid.normalization",
                                "Save normalization",
                                width="170px")
            )
            ,conditionalPanel(id = "wellPanlNormalization",
                condition = "true",
                uiOutput("helpForNormalizationMethods"),
                fluidRow(
                    column(width=6, plotOutput("viewBoxPlotNorm")),
                    column(width=6, plotOutput("viewComparisonNorm"))),
                    plotOutput("viewDensityplotNorm")
            )
        ),
    tags$head(
        tags$style(type="text/css", 
                "#AbsolutePanelPlotOptions {
                background-color:transparent;"
        )
    ),
    absolutePanel(id  = "AbsolutePanelPlotOptions",
        top = 200,
        right = 50,
        width = "200px",
        height = "50px",
        draggable = TRUE,
        fixed = FALSE,
        cursor = "move",
        uiOutput("AbsShowOptions")
    )
),

tabPanel("Miss. values imputation",
         id = "tabPanelImputation",
    value = "imputation",
        sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_imputation",
                    height = "100%"
                    ,h4("Miss. values imputation options")
                    ,br()
                    ,uiOutput("chooseImputationMethod"),
                    actionButton("perform.imputation.button",
                                "Perform imputation"),
                    actionButton("ValidImputation", 
                                "Save imputation",
                                styleclass = "primary"),
                    ## progress bar
                    br(),
                    br(),
                    uiOutput(outputId = "progressOne")
                ),
                conditionalPanel(id = "wellPanel_Imputation",
                    condition = "true",
                        helpText("Select an imputation method before 
                                performing the imputation of missing values."),
                    busyIndicator("Calculation in progress",wait = 0),
                    #imageOutput("viewNAbyMean"),
                     fluidRow(
                         column(width = 5, plotOutput("viewNAbyMean"
                                                       , height = plotHeight, width = "400px"))
                         ,column(width = 7, plotOutput("showImageNA"
                                                        ))
                     )
                    
                )
            )
),


tabPanel("Aggregation",
         id = "Aggregation",
         value="Aggregation",
             tabsetPanel(
        "agreagationTabsetPanel",
        id = "agreagationTabsetPanel",
        tabPanel("1 - Aggregate peptides",
            value = "aggregation",
            sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
               uiOutput("AggregationSideBar_Step1"),
            conditionalPanel(id = "wellPanel_Agregation",
                condition = 'true',
                uiOutput("AggregationWellPanel_Step1")
            )
            )
        ),
tabPanel("2 - Configure protein dataset",
    value = "configureProteinDataset",
    uiOutput("Aggregation_Step2")
    )
)

),


tabPanel("Differential analysis",
    tabsetPanel(
        "diffAnalysis_tabSetPanel",
        id = "diffAnalysis_tabSetPanel",
        tabPanel("1 - Volcano plot",
            value = "DiffAnalysis_Volcanoplot",
            sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_DiffAna1",
                    height = "100%"
                    ,h4("Differential analysis options")
                    ,uiOutput("diffAnalysis_sidebarPanelTab1")
                    ),
                conditionalPanel(id = "wellPanel_DifferentialAnalysisTab1",
                    condition = "true",
                    uiOutput("nbSelectedItems"),
                    plotOutput("volcanoplot", height="500px", width="600px")
                )
            )
        ),
        tabPanel("2 - p-value calibration",
            value = "DiffAnalysis_Calibrate",
            sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_DiffAna2",
                    height = "100%"
                    ,h4("Calibration")
                    ,uiOutput("diffAnalysis_sidebarPanelTab2")
                ),
                conditionalPanel(id = "wellPanel_DifferentialAnalysisTab2",
                    condition = "true",
                    htmlOutput("errMsgCalibrationPlotAll"),
                    busyIndicator("Calculation in progress",wait = 0),
                    plotOutput("calibrationPlotAll"),
                    uiOutput("errMsgCalibrationPlot"),
                    busyIndicator("Calculation in progress",wait = 0),
                    plotOutput("calibrationPlot")
                )
            )
        ),
        tabPanel("3 - FDR",
            id = "DiffAnalysis_viewFDR",
            value = "DiffAnalysis_viewFDR",
            sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_DiffAna3",
                    height = "100%"
                    ,h4("Compute FDR")
                    ,uiOutput("diffAnalysis_sidebarPanelTab3")
                ),

                conditionalPanel(id = "wellPanel_DifferentialAnalysisTab3",
                    condition = "true",
                    uiOutput("nbSelectedItemsStep3"),
                     hr(),
                    fluidRow(
                        column(width= 4, htmlOutput("equivPVal")),
                        column(width= 4, htmlOutput("showFDR"))
                    ),
                    plotOutput("volcanoplotStep3", 
                                height="500px",
                                width="600px")
                )
            )
        ), # end tabPanel(title = "3 - Visualize FDR"
        tabPanel("4 - Validate & save",
                 id = "panelDiffAnaSave",
            value = "DiffAnalysis_ValidateAndSave",
            sidebarCustom(),
            splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                wellPanel(id = "sidebar_DiffAna4",
                    height = "100%",
                    actionButton("ValidDiffAna","Save diff analysis")
                ),
                conditionalPanel(id = "wellPanel_DifferentialAnalysisTab4",
                    condition = "true",
                    DT::dataTableOutput("limmaplot"),
                    br()
                    ,uiOutput("DiffAnalysisSaved")
                    )
            )
        ) # end tabPanel(title = "4 - Validate and Save", 
    ) # end tabsetPanel
)
),

########### NAVBAR MENU -  HELP ################
tabPanel("Help",
    htmlOutput("References")
    )

)

)

