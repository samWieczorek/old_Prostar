context("Missing values imputation")
library(DAPARdata)
data(Exp1_R25_prot)
test <- Exp1_R25_prot[1:100]

# test_that("wrapper.mvImputation QRILC", {
#     t <- wrapper.mvImputation(test, "QRILC")
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })


# test_that("wrapper.mvImputation MLE", {
#     t <- wrapper.mvImputation(test, "MLE")
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })


# test_that("wrapper.mvImputation KNN", {
#     t <- wrapper.mvImputation(test, "KNN")
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })


# test_that("wrapper.mvImputation BPCA", {
#     t <- wrapper.mvImputation(test, "BPCA")
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })



# 
# test_that("mvImputation QRILC", {
#     t <- mvImputation(Biobase::exprs(test), "QRILC")
#     expect_is(t, "matrix")
#     expect_equal(length(which(is.na(t))), 0)
# })
# 
# 
# test_that("mvImputation MLE", {
#     t <- mvImputation(Biobase::exprs(test), "MLE")
#     expect_is(t, "matrix")
#     expect_equal(length(which(is.na(t))), 0)
# })
# 
# 
# test_that("mvImputation KNN", {
#     t <- mvImputation(Biobase::exprs(test), "KNN")
#     expect_is(t, "matrix")
#     expect_equal(length(which(is.na(t))), 0)
# })
# 
# 
# test_that("mvImputation BPCA", {
#     t <- mvImputation(Biobase::exprs(test), "BPCA")
#     
#     expect_is(t, "matrix")
#     expect_equal(length(which(is.na(t))), 0)
# })
# 
# 
# 
# 
# 
# test_that("wrapper.impute.pa", {
#     t <- wrapper.impute.pa(test)
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })


# 
# test_that("wrapper.dapar.impute.mi", {
#     test <- mvFilter(test, type="allCond", th = 1)
#     t <- wrapper.dapar.impute.mi(test, nb.iter=1)
#     expect_is(t, "MSnSet")
#     expect_equal(length(which(is.na(exprs(t)))), 0)
# })

