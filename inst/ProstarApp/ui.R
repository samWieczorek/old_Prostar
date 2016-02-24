options(shiny.trace=TRUE)
options(shiny.reactlog=TRUE)

customSidebarPanel <- function (..., width = 3) 
{
div(class = paste0("span", width), tags$form(class = "well",...))
}

library(DAPAR)
library(shiny)
library(rhandsontable)
library(data.table)



sidebarPanelWidth <- function(){
  tags$head(
    tags$style(type="text/css", ".well { max-width: 300px; }")
  )
}

#---------------------------------------------------------------------------------------------------------
test <- "Prostar"
shinyUI <- function(){
  navbarPage("",
             tabPanel(test),
             navbarMenu("Dataset manager",
                        tabPanel("Open MSnset file",
                                 #title="Open a MSnset file",
                                 #icon = icon("file"),
                                 value = "open",
                                 
                                 sidebarLayout(
                                   position = "left", 
                                   fluid = TRUE,
                                   sidebarPanel(
                                     sidebarPanelWidth(),
                                     fileInput("file", "Open a MSnset file",  multiple=FALSE, accept=c("MSnset", "MSnSet"))
                                   ),
                                   mainPanel(
                                     h3("Quick overview of the dataset"),
                                     uiOutput("overview")
                                      )
                                 )),
                        
                        
                        tabPanel("Convert data",
                          icon = icon("download"),
                          value = "import",
                          sidebarLayout(
                            sidebarPanel(
                              sidebarPanelWidth()
                              ),
                            mainPanel(
                           
                          helpText("These steps allow to create a MSnSet file from a tabulated-text file."),
                          tabsetPanel(id = "tabImport",
                                      tabPanel( "1 - Select file",
                                                value = "SelectFile2Import",
                                                
                                                fileInput("file1", "Data file", 
                                                          multiple=FALSE, 
                                                          accept=c(".txt", ".csv",".xls", ".xlsx")),
                                                uiOutput("ManageXlsFiles"),
                                                
                                                helpText("Hint : before importing quantification 
                                                         file data, check the syntax 
                                                         of your text file."),
                                                br(),
                                                wellPanel(
                                                  radioButtons("typeOfData", "Is it a peptide or protein dataset ?", 
                                                               choices=c("peptide dataset" = "peptide", 
                                                                         "protein dataset" = "protein")
                                                  ),
                                                  
                                                  radioButtons("checkDataLogged", "Check whether the data you want to analyze are already logged or not.
                                                               If not, they will be automatically logged", 
                                                               choices=c("yes", "no"), 
                                                               selected="no"),
                                                  br(),
                                                  checkboxInput("replaceAllZeros", 
                                                                "Replace all 0 and NaN by NA", 
                                                                value= TRUE)
                                                )
                                                ),
                                      tabPanel( "2 - Data Id",
                                                value = "ID",
                                                uiOutput("helpTextDataID"),
                                                
                                                radioButtons("autoID", width="500px",
                                                             "If you choose the automatic ID, Prostar will build an index.", 
                                                             choices=c("Auto ID" = "Auto ID", 
                                                                       "user ID" = "user ID")),
                                                
                                                conditionalPanel(
                                                  condition = 'input.autoID == "user ID"',
                                                  uiOutput("id"),
                                                  uiOutput("warningNonUniqueID"))
                                      ),
                                      tabPanel( "3 - Exp. and feat. data",
                                                value = "Import1",
                                                helpText("Select the columns that are quantitation values 
                                                         by clicking in the fiels below."),
                                                div(class="row"),
                                                div(class="span5", "Quantitative  Data",
                                                    uiOutput("eData"))
                                                ),
                                      tabPanel( "4 - Samples metadata",
                                                value = "Import2",
                                                #helpText("TODO"),
                                                helpText("Attention : it is mandatory that the column 
                                                         \"Label\" is filled."),
                                                br(),
                                                rHandsontableOutput("hot")
                                      ),
                                      tabPanel( "5 - Convert",
                                                value = "Convert",
                                                htmlOutput("msgAlertCreateMSnset"),
                                                fluidRow(
                                                  column(width = 6, 
                                                         textInput("filenameToCreate",
                                                                   "Enter the name of the study")),
                                                  column(width = 6, 
                                                         actionButton("createMSnsetButton",
                                                                      "Convert data"))
                                                  
                                                ),
                                                uiOutput("conversionDone")
                                      )
                                      )
                                      ))),

                        tabPanel("Export",
                          value = "export",
                          icon = icon("upload"),
                          sidebarLayout(
                            sidebarPanel(
                              sidebarPanelWidth()
                            ),
                            mainPanel(
                              helpText("Choose the export format of the dataset and choose a name."),
                          #helpText("TODO"),
                          selectInput("fileformatExport", "File format", 
                                      choices=  c( "MSnset","Excel")),
                          
                          conditionalPanel(
                            condition = "input.fileformatExport == 'Excel'",
                            uiOutput("selectIDforExcelExport")
                          ),
                          
                          textInput("nameExport", "Enter the name of the files to be created"),
                          downloadButton('downloadMSnSet', 'Download')
                        )
             ))),
 tabPanel("Descriptive statistics",
                      value="tabView",
                      icon = icon("bar-chart-o"),
                      sidebarLayout(
                        sidebarPanel(
                          sidebarPanelWidth(),
                         uiOutput("DS_sidebarPanel")
                          ),
                        mainPanel(
                      
                        tabsetPanel(id="DS_tabSetPanel",
                                   #------------------------------------------------------------------------
                                   tabPanel("Overview",
                                            value = "DS_tabGeneral",
                                            uiOutput("overviewNewData")
                                   ),
                                   
                                   tabPanel(title = "Miss. values",
                                            value = "DS_tabOverviewMV",
                                               helpText("Those bargraph plots display some information to view the distribution
                                                        of missing values."),
                                               fluidRow(
                                                  column(width = 4, plotOutput("histoMV_DS")),
                                                  column(width = 4, plotOutput("histo.missvalues.per.lines_DS")),
                                                  column(width = 4, plotOutput("histo.missvalues.per.lines.per.conditions_DS"))
                                                  )
                                              ),

                                   #------------------------------------------------------------------------
                                    tabPanel(title="Data explorer",
                                              value = "DS_DataExplorer",
                                             uiOutput("tabToShow")
                                     ),
                                   
                                    tabPanel(title="Corr. matrix",
                                              value="DS_tabCorrMatrix",
                                              #helpText("TODO"),
                                              plotOutput("corrMatrix", height="500px",width="800px")
                                             ),
                                   


                                     tabPanel(title="Heatmap",
                                              value="DS_tabHeatmap",
                                              #helpText("TODO"),
                                              helpText("For this view, it is necessary that your dataset does
                                                       not contains any NA lines
                                                       Please check your data and use Filtering options or
                                                       missing values imputation.",
                                                       style = "color:black"),

                                              #uiOutput("heatmapOptions"),
                                              uiOutput("DS_PlotHeatmap")
                                              
                                     ),

                                   #-----------------------------------------------------------
                                    tabPanel(title = "Boxplot",
                                              value="DS_tabBoxplot",
                                              # helpText("TODO"),
                                              plotOutput("viewBoxPlot_DS")
                                     ),


                                   #-----------------------------------------------------------
                                     tabPanel(title = "Densityplot",
                                              value="DS_tabDensityplot",
                                                plotOutput("viewDensityplot_DS", height="500px")
                                                        ) ,

                                   #-----------------------------------------------------------
                                     tabPanel(title="Variance distr.", 
                                              value="DS_tabDistVar",
                                              p("This graphics shows, for each condition,
                                                the distribution of the variance of the
                                                log-intensities."),
                                              plotOutput("viewDistVariance",
                                                         height="500px",
                                                         width="800px")
                                              )

                               )
                        )
                      )
                      ),
             navbarMenu("Data processing",
                      tabPanel("Filter data",
                                 # icon = icon("download"),
                               sidebarLayout(
                                 sidebarPanel(
                                   sidebarPanelWidth(),
                                   uiOutput("DP_Filtering_sidebarPanel")
                                 ),
                                 mainPanel(
                                  tabsetPanel(id = "DP_Filtering_tabSetPanel",
                                             tabPanel( "1 - Missing values",
                                                       value = "DP_FilterMissingValues",
                                                       
                                                       helpText("The filter below allows keeping the lines that contain 
                                                                a certain amount of quantitative data rather than NA values. 
                                                                The threshold to define correponds to the number of quantitative values in a 
                                                                line and means that the lines which contain at least this threshold value 
                                                                are kept. This filtering threshold may be applied on the whole  dataset, on 
                                                                each condition or on at leat one condition."),
                                                       fluidRow(
                                                         column(width = 4, plotOutput("histoMV")),
                                                         column(width = 4,plotOutput("histo.missvalues.per.lines")),
                                                         column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions"))
                                                                ),
                                                       actionButton("perform.filtering.MV", "Perform filtering MV")
                                                       ),
                                             tabPanel( "2 - String based filtering",
                                                       value = "DP_FilterContaminants",
                                                       plotOutput("GlobalPieChart"),
                                                      actionButton("perform.filtering.Contaminants","Perform filtering contaminants")
                                                       ),
                                             tabPanel( "3 - Visualize and Validate",
                                                       value = "DP_FilterValidate",
                                                       dataTableOutput("VizualizeFilteredData"),
                                                       helpText("After checking the data, validate the filters"),
                                                      actionButton("ValidateFilters","Save filtered dataset",styleclass = "primary")
                                                       )
                                             
                                              )
                               ) # end mainPanel
                               )),
                      tabPanel("Normalization",
                                        value = "Normalization",
                                        
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   sidebarPanelWidth(),
                                   helpText("Select a normalization method before performing normalization on the dataset"),
                                   uiOutput("choose_Normalization_Test"),
                                   actionButton("perform.normalization", "Perform normalization"),
                                   actionButton("valid.normalization",  "Save normalization", styleclass = "primary"),
                                   uiOutput("ChooseLegendForAxis"),
                                   uiOutput("nShow"),
                                   uiOutput("nGroup")
                                   ),
                                 mainPanel(
                                   uiOutput("helpForNormalizationMethods"),
                                   fluidRow(
                                     column(width=6, plotOutput("viewBoxPlot")),
                                     column(width=6, plotOutput("viewComparisonNorm"))),
                                   plotOutput("viewDensityplot")
                                 )
                               )),
                      tabPanel("Miss. values imputation",
                                        value = "imputation",
                               sidebarLayout(
                                 sidebarPanel(
                                   sidebarPanelWidth(),
                                   selectInput("missing.value.algorithm","Choose algorithm",choices = names(imputationAlgorithms)),
                                   actionButton("perform.imputation.button","Perform imputation"),
                                   actionButton("ValidImputation", "Save imputation",styleclass = "primary")
                                 ),
                                 mainPanel(
                                   helpText("Select an imputation method before performing the imputation of missing values."),
                                   busyIndicator("Calculation In progress",wait = 0),
                                   fluidRow(
                                     column(width = 4, plotOutput("viewNAbyMean")),
                                     column(width = 8, plotOutput("showImageNA"))
                                   )
                                 )
                               )
                               ),
                      tabPanel("Aggregation",
                                
                                 sidebarLayout(
                                   sidebarPanel(
                                     sidebarPanelWidth(),
                                     conditionalPanel(
                                       condition = 'input.datasets != "Aggregated"',
                                       uiOutput("chooseProteinId")
                                       
                                       ),
                                       checkboxInput("checkSharedPeptides",
                                                     "Include shared peptides",
                                                     value = FALSE),
                                       selectInput("aggregationMethod",
                                                   "Aggregation methods",
                                                   choices =  gAgregateMethod),
                                       conditionalPanel(
                                         condition='input.aggregationMethod == "sum on top n"',
                                         numericInput("nTopn", "nTopn",
                                                      value = NULL,
                                                      min = 0)),
                                     actionButton("perform.aggregation","Perform aggregation", styleclass = "primary")
                                       
                                     ),
                                   mainPanel(
                                     tabsetPanel(
                                       title = "agreagationTabsetPanel",
                                       id = "agreagationTabsetPanel",
                                       tabPanel(title = "1 - Agregate peptides",
                                                value = "aggregation",
                                                
                                            helpText("Please select first the id of protein in your dataset. Then, the stats
                                                   will be showed and it will be possible to perform the aggregation"),
                                     
                                     fluidRow(
                                       column(width=6, h4("Only unique peptides")),
                                       column(width=6, h4("All (unique & shared) peptides"))
                                     ),
                                     busyIndicator("Calculation In progress",wait = 0),
                                     fluidRow(
                                       column(width=6, plotOutput("aggregationPlotUnique")),
                                       column(width=6, plotOutput("aggregationPlotShared"))
                                     ),
                                     uiOutput("aggregationStats"),
                                     uiOutput("ObserverAggregationDone")),
                                     
                                     tabPanel(title = "2 - Configure protein dataset",
                                              value = "configureProteinDataset",
                                              helpText("Select the columns of the meta-data (related to proteins) that have to be recorded in the new protein dataset."),
                                              div(class="row"),
                                              div(class="span5", "",
                                                  uiOutput("columnsForProteinDataset"),
                                                  
                                                  fluidRow(
                                                    column(width=3,
                                                           actionButton("valid.aggregation","Save aggregation", styleclass = "primary"))
                                                  )
                                              )
                                     ))
                                     
                               
                                 ))),
                      tabPanel("Differential analysis",
                               sidebarLayout(
                                   sidebarPanel(
                                     sidebarPanelWidth(),
                                     uiOutput("diffAnalysis_sidebarPanel")
                                     ),
                                   
                                   mainPanel(
                                     tabsetPanel(
                                       title = "diffAnalysis_tabSetPanel",
                                       id = "diffAnalysis_tabSetPanel",
                                       tabPanel(title = "1 - Volcanoplot",
                                              value = "DiffAnalysis_Volcanoplot",
                                              uiOutput("nbSelectedItems"),
                                              plotOutput("volcanoplot", height="500px", width="600px")
                                     ),
                                     tabPanel(title = "2 - Calibrate Ana Diff",
                                              value = "DiffAnalysis_Calibrate",
                                              
                                              htmlOutput("errMsgCalibrationPlotAll"),
                                              busyIndicator("Calculation In progress",wait = 0),
                                              plotOutput("calibrationPlotAll"),
                                              
                                              uiOutput("errMsgCalibrationPlot"),
                                              busyIndicator("Calculation In progress",wait = 0),
                                              plotOutput("calibrationPlot")
                                     ),
                                     
                                     tabPanel(title = "3 - Visualize FDR",
                                              value = "DiffAnalysis_viewFDR",
                                              # uiOutput("calibrationResults"),
                                              uiOutput("nbSelectedItemsStep3"),
                                              br(), br(), hr(),
                                              fluidRow(
                                                column(width= 4, htmlOutput("equivPVal")),
                                                column(width= 4, htmlOutput("showFDR"))
                                              ),
                                              plotOutput("volcanoplotStep3", height="500px", width="600px")
                                     ),
                                     
                                     tabPanel(title = "4 - Validate and Save",
                                              value = "DiffAnalysis_ValidateAndSave",
                                              
                                              dataTableOutput("limmaplot"),
                                              br(),
                                              uiOutput("DiffAnalysisSaved"))
                                   ) # end tabsetPanel
                                     

                                   )
                                   )
                      )
                                 
                      ),
             navbarMenu("Help",
                        tabPanel(
                          title="About",
                          value = "tabHelpMSnset",
                          
                          #htmlOutput("aboutText")
                          uiOutput("aboutText")
                        ),
                         tabPanel(title="The MSnset format",
                                               value = "tabHelpMSnset",
                                               HTML("You can access the package 
                                                    and read the document 
                                                    <a href=\"http://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\"
                                                    title=\"here\" target=\"_blank\">here</a>. <br>")
                                      ),
                        tabPanel(title="Refs", htmlOutput("References")
                                      )
                          )
              ,
 h3("Available datasets"),
 selectInput("datasets", "", choices = list("None"="none"), width = '200px')
           
             )
  
}
