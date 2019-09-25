context("Descriptive Statistics using visualTest")
library(DAPARdata)
data(Exp1_R25_prot)
test <- Exp1_R25_prot[1:100]
test <- mvFilter(test, "wholeMatrix", 6)

# test_that("diffAnaVolcanoplot", {
#     condition1 <- '25fmol'
#     condition2 <- '10fmol'
#     data <- wrapper.diffAnaLimma(test, condition1, condition2)
#     t <- diffAnaVolcanoplot(data$logFC, data$P_Value)
#     
#     expect_null(t)
#     dev.off()
# })



# test_that("diffAnaVolcanoplot_rCharts", {
#     condition1 <- '25fmol'
#     condition2 <- '10fmol'
#     
#     cond <- c(condition1, condition2)
#     data <- wrapper.diffAnaLimma(test, condition1, condition2)
#     df <- data.frame(x=data$logFC, 
#                     y = -log10(data$P_Value),
#                     index = as.character(rownames(test)),
#                     stringsAsFactors = FALSE)
#     tooltipSlot <- c("Sequence.length", "Protein.IDs")
#     df <- cbind(df,Biobase::fData(test)[tooltipSlot])
#     colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
#     if (ncol(df) > 3){
#      colnames(df)[4:ncol(df)] <- 
#      paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
#     }
#     hc_clickFunction <- JS("function(event) {
#     Shiny.onInputChange('eventPointClicked', [this.index]);}")
#     t <- diffAnaVolcanoplot_rCharts(df,threshold_logFC = 1,
#                                 threshold_pVal = 3,
#                                 conditions = cond,
#                                 clickFunction=hc_clickFunction) 
#     
#     expect_is(t, "highchart")
#     expect_is(t$x, "list")
#     expect_is(t$jsHooks, "list")
#     #dev.off()
# })
