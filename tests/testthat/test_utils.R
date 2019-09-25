context("Utils")

require(DAPARdata)
data(Exp1_R2_pept)
obj <- Exp1_R2_pept
test <- Exp1_R2_pept[130:140]
test2 <- Exp1_R2_pept[20:28]




test_that("getProcessingInfo", {
    expect_equal(getProcessingInfo(Exp1_R2_pept), "Log2 tranformed data")
})


test_that("getNumberOfEmptyLines", {
    expect_equal(getNumberOfEmptyLines(exprs(Exp1_R2_pept)), 715)
})


test_that("getIndicesConditions", {
    labels <- Biobase::pData(Exp1_R2_pept)[,"Condition"]
    l <- list(iCond1=c(1,2,3), iCond2=c(4,5,6))
    expect_equal(getIndicesConditions(labels, "10fmol", "5fmol"), l)
})

