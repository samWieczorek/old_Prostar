context("Missing values filter")

require(DAPARdata)
data(Exp1_R2_pept)
obj <- Exp1_R2_pept
test <- Exp1_R2_pept[130:140]
test2 <- Exp1_R2_pept[20:28]


# 
# 
# test_that("getPourcentageOfMV", {
# 
#     expect_equal(round(getPourcentageOfMV(test), 2), 28.79)
# })
# 
# 
# 
# 
# 
# test_that("getIndicesOfLinesToRemove", {
#     
#     expect_equal(getIndicesOfLinesToRemove(test2, "Potential.contaminant", prefix="+"), 9)
#     expect_equal(getIndicesOfLinesToRemove(test2, "Potential.contaminant"), NULL)
# })
# 
# 
# 
# 
# test_that("getNumberOf", {
#     
#     expect_equal(getNumberOf(test2,"Potential.contaminant", "+"), 1)
#     expect_equal(getNumberOf(test2,"Potential.contaminant", "+abc"), 0)
#     expect_equal(getNumberOf(test2,"Potential.contaminant"), 0)
#     expect_equal(getNumberOf(test2, prefix="+"), 0)
#     expect_equal(getNumberOf(test2), 0)
# })
# 
# 
# 
# test_that("removeLines", {
#     
#     Mout <- matrix(c(24.8037, 24.8458, 24.9518, 24.8411, 24.8445, 24.5173,
#                      NA, 22.7895, 22.1014, NA, 23.1118, NA,
#                      NA, NA, NA, NA, NA, NA,
#                      22.2452, NA, 21.8009, NA, NA, NA,
#                      NA, 22.3721, NA, NA, NA, NA,
#                      NA, NA, NA, NA, NA, 23.1516,
#                      24.4367, 24.5145, 24.2637, 24.7409, 24.6064, 24.7815,
#                      24.2771, 24.4007, 24.1365, 24.9600, 25.0521, 25.0964),
#                    byrow=TRUE, ncol=6,
#                    dimnames = list(c(19:26), 
#                                    c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     expect_equal(round(exprs(removeLines(test2,"Potential.contaminant", "+")),4), Mout)
#     
#     expect_equal(Biobase::exprs(removeLines(test2,"Potential.contaminant")), Biobase::exprs(test2))
#     expect_equal(Biobase::fData(removeLines(test2,"Potential.contaminant")), Biobase::fData(test2))
#     expect_equal(Biobase::pData(removeLines(test2,"Potential.contaminant")), Biobase::pData(test2))
# })
# 
# 
# 
# test_that("mvFilterFromIndices", {
#     
#     expect_equal(Biobase::exprs(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::exprs(Exp1_R2_pept[c(1:100, 200:300)]))
#     expect_equal(Biobase::fData(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::fData(Exp1_R2_pept[c(1:100, 200:300)]))
#     expect_equal(Biobase::pData(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::pData(Exp1_R2_pept[c(1:100, 200:300)]))
#     
#     expect_equal(Biobase::exprs(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::exprs(Exp1_R2_pept[c(1:100, 200:300)]))
#     expect_equal(Biobase::fData(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::fData(Exp1_R2_pept[c(1:100, 200:300)]))
#     expect_equal(Biobase::pData(mvFilterFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::pData(Exp1_R2_pept[c(1:100, 200:300)]))
# })
# 
# 
# 
# 
# test_that("mvFilterFromIndices", {
#     expect_equal(Biobase::exprs(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::exprs(Exp1_R2_pept[-c(1:100, 200:300)]))
#     expect_equal(Biobase::fData(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::fData(Exp1_R2_pept[-c(1:100, 200:300)]))
#     expect_equal(Biobase::pData(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "reverse")), Biobase::pData(Exp1_R2_pept[-c(1:100, 200:300)]))
#     
#     expect_equal(Biobase::exprs(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::exprs(Exp1_R2_pept[-c(1:100, 200:300)]))
#     expect_equal(Biobase::fData(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::fData(Exp1_R2_pept[-c(1:100, 200:300)]))
#     expect_equal(Biobase::pData(deleteLinesFromIndices(Exp1_R2_pept,c(1:100, 200:300), "contaminants")), Biobase::pData(Exp1_R2_pept[-c(1:100, 200:300)]))
# })
# 
# 
# 
# 
# test_that("mvFilterFromIndices", {
#     t <- c(1,2,4,5,7,11)
#     names(t) <- c(129, 130, 132, 133, 135, 139)
#     expect_equal(mvFilterGetIndices(test, "wholeMatrix", 6), t)
# 
# })
# 
# 
# 
# 
# test_that("Missing values filter, whole Matrix, no NA lines", {
# 
# Mout <- matrix(c(28.1944,  28.0756, 28.1724, 28.3253, 28.4246, 28.3782,
#                  24.4159, 24.4346, 24.4237, 24.3140, 24.5114, 24.5450,
#                  22.6008, 22.9028,      NA, 22.8938, 23.0190,      NA,
#                  27.7759, 27.6335, 27.6969, 27.9458, 28.0809, 28.0318,
#                  24.7701, 24.7433, 24.6523, 24.5939, 24.0654, 24.3660,
#                  19.6482,      NA, 22.9006, 23.2119, 23.1454, 22.9676,
#                  26.7079, 26.6682, 26.7437, 27.4372, 27.4080, 27.4981,
#                  25.3562,      NA,      NA,      NA,      NA,      NA,
#                  NA,      NA,      NA,      NA,      NA, 22.4979,
#                  29.4798, 29.5120, 29.4984, 29.6032, 29.6513, 29.7486),
#                 byrow=TRUE, ncol=6,
#                 dimnames = list(c(129:135, 137:139), 
#                                 c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
# 
# 
# expect_equal(round(Biobase::exprs(mvFilter(test, "wholeMatrix", th=as.integer(1))), 4), Mout)
# expect_equal(round(Biobase::exprs(mvFilter(test, "wholeMatrix", th=1)), 4), Mout)
# })
# 
# 
# test_that("Missing values filter, 
#         at least 2 intensities for each condition", {
# 
# Mout <- matrix(c(28.1944, 28.0756, 28.1724, 28.3253, 28.4246, 28.3782,
#                  24.4159, 24.4346, 24.4237, 24.3140, 24.5114, 24.5450,
#                  22.6008, 22.9028, NA, 22.8938, 23.0190, NA,
#                  27.7759, 27.6335, 27.6969, 27.9458, 28.0809, 28.0318,
#                  24.7701, 24.7433, 24.6523, 24.5939, 24.0654, 24.3660,
#                  19.6482, NA, 22.9006, 23.2119, 23.1454, 22.9676,
#                  26.7079, 26.6682, 26.7437, 27.4372, 27.4080, 27.4981,
#                  29.4798, 29.5120, 29.4984, 29.6032, 29.6513, 29.7486),
#                 byrow=TRUE, ncol=6,
#                 dimnames = list(c(129:135, 139), 
#                                 c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
# 
# 
# expect_equal(round(Biobase::exprs(mvFilter(test, "allCond", th=as.integer(2))),4), Mout)
# expect_equal(round(Biobase::exprs(mvFilter(test, "allCond", th=2)),4), Mout)
# })
# 
# 
# 
# 
# 
# test_that("Missing values filter, 
#         at least 2 intensities ine one condition", {
# 
# Mout <- matrix(c(28.1944, 28.0756, 28.1724, 28.3253, 28.4246, 28.3782,
#                  24.4159, 24.4346, 24.4237, 24.3140, 24.5114, 24.5450,
#                  22.6008, 22.9028, NA, 22.8938, 23.0190, NA,
#                  27.7759, 27.6335, 27.6969, 27.9458, 28.0809, 28.0318,
#                  24.7701, 24.7433, 24.6523, 24.5939, 24.0654, 24.3660,
#                  19.6482, NA, 22.9006, 23.2119, 23.1454, 22.9676,
#                  26.7079, 26.6682, 26.7437, 27.4372, 27.4080, 27.4981,
#                  29.4798, 29.5120, 29.4984, 29.6032, 29.6513, 29.7486),
#                 byrow=TRUE, ncol=6,
#                 dimnames = list(c(129:135,139), 
#                                 c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3"))
# )
# 
# expect_equal(round(Biobase::exprs(mvFilter(test, "atLeastOneCond", th=as.integer(2))), 4), Mout)
# expect_equal(round(Biobase::exprs(mvFilter(test, "atLeastOneCond", th=2)), 4), Mout)
# })
