context("Descriptive Statistics using visualTest")
library(DAPARdata)
data(Exp1_R25_prot)
test <- Exp1_R25_prot[1:100]
# 
# test_that("wrapper.mvPerLinesHisto", {
#     t <- wrapper.mvPerLinesHisto(test,showValues=FALSE)
#     expect_is(t, "matrix")
#     expect_equal(t[,1], c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7))
#     dev.off()
# })
# 
# test_that("wrapper.mvPerLinesHisto", {
#     t <- wrapper.mvPerLinesHisto(test,showValues=TRUE)
#     expect_is(t, "matrix")
#     expect_equal(t[,1], c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7))
#     dev.off()
# })
# 
# 
# test_that("mvPerLinesHisto", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     t <- mvPerLinesHisto(qData, samplesData,showValues=FALSE)
#     expect_is(t, "matrix")
#     expect_equal(t[,1], c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7))
#     dev.off()
# })
# 
# 
# 
# 
# 
# 
# test_that("wrapper.mvPerLinesHistoPerCondition, showValues = TRUE", {
#     t <- wrapper.mvPerLinesHistoPerCondition(test, showValues=TRUE)
#     expect_is(t, "matrix")
#     m <- matrix(c(1.5, 4.5, 7.5, 10.5, 2.5, 5.5, 8.5, 11.5), byrow=TRUE, ncol=4)
#     expect_equal(t, m)
#     dev.off()
# })
# 
# 
# test_that("wrapper.mvPerLinesHistoPerCondition showValues = FALSE", {
#     t <- wrapper.mvPerLinesHistoPerCondition(test, showValues=FALSE)
#     expect_is(t, "matrix")
#     m <- matrix(c(1.5, 4.5, 7.5, 10.5, 2.5, 5.5, 8.5, 11.5), byrow=TRUE, ncol=4)
#     expect_equal(t, m)
#     dev.off()
# })
# 
# 
# 
# 
# test_that("mvPerLinesHistoPerCondition showValues = FALSE", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     t <- mvPerLinesHistoPerCondition(qData, samplesData, showValues=FALSE)
#     expect_is(t, "matrix")
#     m <- matrix(c(1.5, 4.5, 7.5, 10.5, 2.5, 5.5, 8.5, 11.5), byrow=TRUE, ncol=4)
#     expect_equal(t, m)
#     dev.off()
# })
# 
# 
# 
# 
# test_that("mvPerLinesHistoPerCondition showValues = TRUE", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     t <- mvPerLinesHistoPerCondition(qData, samplesData, showValues=TRUE)
#     expect_is(t, "matrix")
#     m <- matrix(c(1.5, 4.5, 7.5, 10.5, 2.5, 5.5, 8.5, 11.5), byrow=TRUE, ncol=4)
#     expect_equal(t, m)
#     dev.off()
# })
# 
# 
# 
# 
# 
# 
# test_that("wrapper.mvHisto showValues = FALSE", {
#     t <- wrapper.mvHisto(test, showValues=FALSE)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
# test_that("wrapper.mvHisto showValues = TRUE", {
#     t <- wrapper.mvHisto(test, showValues=TRUE)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
# 
# 
# 
# test_that("mvHisto showValues = FALSE", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     labels <- Biobase::pData(test)[,"Condition"]
#     t <- mvHisto(qData, samplesData, labels, indLegend="auto", showValues=FALSE)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
# test_that("mvHisto showValues = TRUE", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     labels <- Biobase::pData(test)[,"Condition"]
#     t <- mvHisto(qData, samplesData, labels, indLegend="auto", showValues=TRUE)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
# test_that("wrapper.mvImage", {
#     t <- wrapper.mvImage(test)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
# 
# test_that("wrapper.mvImage", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     t <- mvImage(Biobase::exprs(test), labels)
#     expect_null(t)
#     dev.off()
# })
# 
# 
# 
# 
