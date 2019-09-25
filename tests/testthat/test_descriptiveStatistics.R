context("Descriptive Statistics using visualTest")
library(DAPARdata)
data(Exp1_R25_prot)
test <- Exp1_R25_prot[1:10]

# test_that("wrapper boxplot", {
#     types <- c("Condition","Analyt.Rep")
#     t <- wrapper.boxPlotD(test, types,group2Color="Replicate")
#     expect_is(t, "character")
#     expect_equal(t, c("black", "red","green3","blue","cyan","magenta", "yellow",  "gray"))
#     dev.off()
# })

# test_that("wrapper boxplot", {
#     types <- c("Condition","Analyt.Rep")
#     t <- wrapper.boxPlotD(test, types,group2Color="Condition")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red","green3","blue","cyan","magenta", "yellow",  "gray"))
#     dev.off()
# })

# test_that("boxplotD", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     types <- c("Condition","Analyt.Rep")
#     dataForXAxis <- Biobase::pData(test)[,types]
#     t <- boxPlotD(exprs(test), dataForXAxis, labels,group2Color="Condition")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red","green3","blue","cyan","magenta", "yellow",  "gray"))
#     dev.off()
# })


# test_that("boxplotD", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     types <- c("Condition","Analyt.Rep")
#     dataForXAxis <- Biobase::pData(test)[,types]
#     t <- boxPlotD(exprs(test), dataForXAxis, labels,group2Color="Replicate")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray"))
#     dev.off()
# })

# test_that("wrapper density plot", {
#     types <- c("Condition","Analyt.Rep")
#     t <- wrapper.densityPlotD(test, types)
#     expect_is(t, "list")
#     expect_equal(t$rect$w, 4.122479, tolerance=1e-2)
#     expect_equal(t$rect$h, 0.01170816, tolerance=1e-2)
#     dev.off()
# })


# test_that("density plot", {
#     types <- c("Condition","Analyt.Rep")
#     labels <- lab2Show <- Biobase::pData(test)[,"Condition"]
#     qData <- Biobase::exprs(test)
#     dataForXAxis <- Biobase::pData(test)[,types]
#     t <-densityPlotD(qData, labels)
#     expect_is(t, "list")
#     expect_equal(t$rect$w, 3.0314, tolerance=1e-2)
#     expect_equal(t$rect$h, 0.01170816, tolerance=1e-2)
#     dev.off()
# })


# test_that("wrapper violinPlot", {
#     require(vioplot)
#     require(sm)
#     types <- c("Condition","Analyt.Rep")
#     t <- wrapper.violinPlotD(test, types,group2Color="Condition")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red","green3","blue","cyan","magenta", "yellow",  "gray"))
#     dev.off()
# })

# test_that("wrapper violinPlot", {
#     require(vioplot)
#     require(sm)
#     types <- c("Condition","Analyt.Rep")
#     t <- wrapper.violinPlotD(test, types,group2Color="Replicate")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray"))
#     dev.off()
# })

# test_that("violinPlotD", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     types <- c("Condition","Analyt.Rep")
#     dataForXAxis <- Biobase::pData(test)[,types]
#     t <- violinPlotD(exprs(test), dataForXAxis, labels,group2Color="Condition")
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t, c("black", "red","green3","blue","cyan","magenta", "yellow",  "gray"))
#     dev.off()
# })


# test_that("violinPlotD", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     types <- c("Condition","Analyt.Rep")
#     dataForXAxis <- Biobase::pData(test)[,types]
#     t <- violinPlotD(exprs(test), dataForXAxis, labels,group2Color="Replicate")
# expect_is(t, "character")
# expect_equal(length(t), 8)
# expect_equal(t, c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray"))
# dev.off()
# })


# test_that("wrapper.compareNormalizationD", {
#     labels <- Biobase::pData(test)[,"Condition"]
#     objAfter <- wrapper.normalizeD2(test, "Mean Centering", "within conditions")
#    t <-  wrapper.compareNormalizationD(test, objAfter, labels)
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t[8], "gray")
#     dev.off()
# })


# test_that("compareNormalizationD", {
#     qDataBefore <- Biobase::exprs(test)
#     labels <- Biobase::pData(test)[,"Condition"]
#     qDataAfter <- normalizeD2(qDataBefore,labels,"Quantile Centering","within conditions")
#     t <- compareNormalizationD(qDataBefore, qDataAfter, labels)
#     expect_is(t, "character")
#     expect_equal(length(t), 8)
#     expect_equal(t[8], "gray")
#     dev.off()
# })



# test_that("wrapper.CVDistD", {
#     t <- wrapper.CVDistD(test)
#     expect_is(t, "list")
#     expect_equal(t$text$x, c(2.492999, 2.492999), tolerance=1e-1)
#     expect_equal(t$text$y, c( 2.470089, 2.379125), tolerance=1e-1)
#     dev.off()
# })


# test_that("CVDistD", {
#     t <- CVDistD(exprs(test), Biobase::pData(test)[,"Condition"])
#     expect_is(t, "list")
#     expect_equal(t$text$x, c(2.492999, 2.492999), tolerance=1e-1)
#     expect_equal(t$text$y, c(2.470089, 2.379125), tolerance=1e-1)
#     dev.off()
# })




# test_that("wrapper.corrMatrixD", {
#     t <- wrapper.corrMatrixD(test)
#     expect_is(t, "list")
#     expect_is(t$data[[1]], "data.frame")
#     dev.off()
# })




# test_that("corrMatrixD", {
#     qData <- Biobase::exprs(test)
#     samplesData <- Biobase::pData(test)
#     t <- corrMatrixD(qData, samplesData)
#     expect_is(t, "list")
#     expect_is(t$data[[1]], "data.frame")
#     dev.off()
# })




# test_that("wrapper.heatmapD", {
#     obj <- mvFilter(test, "wholeMatrix", 6)
#     t <- wrapper.heatmapD(obj)
#     expect_is(t$layout, "list")
#     expect_is(t$rowInd, "integer")
#     expect_is(t$col, "character")
#     dev.off()
# })





# test_that("heatmapD", {
#     obj <- mvFilter(test, "wholeMatrix", 6)
#     t <- heatmapD(exprs(obj))
#     expect_is(t$layout, "list")
#     expect_is(t$rowInd, "integer")
#     expect_is(t$col, "character")
#     dev.off()
# })




# test_that("heatmap.DAPAR", {
#     obj <- mvFilter(test, "wholeMatrix", 6)
#     t <- heatmap.DAPAR(exprs(obj))
#     expect_null(t, "list")
#     dev.off()
# })


