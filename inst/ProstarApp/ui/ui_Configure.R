heightSidebarPanel <- "600px"
test <- "Prostar"

widthLeftPanel <- "300px"
widthRightPanel <- "70%"
widthWellPanel <- "80%"
heightWellPanel <- "200px"

plotWidth <- "800px"
plotHeight <- "600px"

sidebarCustom <- function(){
  
    # tags$head(
    #     
    #     
    #     tags$style(type="text/css", 
    #                paste("#wellPanelFileOpen { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css",
    #     #            paste("#sidebarPanel_changeDataset { height:",heightSidebarPanel,"; }",
    #     #                  sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_dataExplorer { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Corrmatrix { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css",
    #     #            paste("#sidebar_heatmap { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_boxplot { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css",
    #     #            paste("#sidebar_densityplot { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Filter1 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Filter2 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Filter3 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Normalization { height:",heightSidebarPanel,
    #     #                  "; z-index:1000;}", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Aggregation { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_imputation { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_DiffAna1 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_DiffAna2 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_DiffAna3 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_DiffAna4 { height:",heightSidebarPanel,"; }", sep="")),
    #     # 
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Imputation1 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Imputation2 { height:",heightSidebarPanel,"; }", sep="")),
    #     # tags$style(type="text/css", 
    #     #            paste("#sidebar_Imputation3 { height:",heightSidebarPanel,"; }", sep="")),
    #     # 
    #     
    #     # tags$style(type="text/css", 
    #     #            paste("#chooseDatasetFromDAPARdata_wellPanel { height:",heightSidebarPanel,"; }", sep="")),
    #     # 
    #     
    #     tags$style(type="text/css", "#DS { padding-top:50px;"),
    #     tags$style(type="text/css", "#sidebar_dataExplorer { padding-top:50px;"),
    #     
    #     tags$style(type="text/css", 
    #                "#wellPanelMVFilterTab1 { width: 800px; 
    #                background-color:transparent;"),
    #     tags$style(type="text/css", 
    #                "#condPanelShowOptions {background-color:#f5f5f5; 
    #                opacity:0.90;
    #                border:1px solid #e3e3e3;
    #                border-radius:4px;"),
    #     
    #     tags$style(type="text/css", "#AbsolusssstePanel { background-color:orange;"),
    #     tags$style(type="text/css", "#wellPanlNormalization { 
    #                z-index:-1;overflow: visible;"),
    #     #tags$style(type="text/css", "#DS_tabOverviewMV { width:800px;"),
    #     
    #     tags$style(type="text/css", "#wellPanelHeatmap { width: 500px;"),
    #     tags$style(type="text/css", "#wellPanel_Agregation { width: 800px;")
    #    # tags$style(HTML('.action-button{background-color:blue}'))
    #     
    #     
    #     )
}





sidebarPanelWidth <- function(){
    tags$head(
        tags$style(type="text/css", "#fileopened { 
                   font-weight:bold; 
                   font-size:100%; 
                   color:black; }")
        )
    }


appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.3;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"


