context("Differential analysis FDR")
# require(DAPARdata)
# data(Exp1_R25_prot)
# test <- Exp1_R25_prot[1:10]


# 
# test_that("compare FDR", {
# datalimma <- 
#     matrix(c(1.144844e-07, 1.055972e-07, 8.192425e-05, 
#                 2.450354e-06, 7.641383e-06, 5.534496e-08,
#                 -1.559135, -1.554906, -1.683824, -1.841546,
#                 -1.786713, -1.468805), 6,2)
# colnames(datalimma) <- c("P_Value","logFC")
# rownames(datalimma) <- c("2","10","11","12","13","14")
# datalimma <- as.data.frame(datalimma)
# 
# funcFdr <- diffAnaComputeFDR(datalimma)
# FDR <- 8.19242509813458e-05
# 
# expect_equal(funcFdr, FDR)
# })
# 
# 
# 
# test_that("Compute limma", {
# data(Exp1_R2_prot)
# obj <- Exp1_R2_prot[c(1:6,8:11)]
# limmaRes <- data.frame(P_Value=c(0.2319,0.3303,0.0000,0.1743,0.2484,0.3145,
#                                     0.0007,0.2569,0.1785,0.0002),
#                         logFC=c(0.4416,0.0554,-1.0852,0.4476,1.6296, 0.2944,
#                                 0.9114,0.1111,-0.5349,-1.0077),
#                         row.names = as.character(c(0:5, 7:10)))
# 
# expect_equal(round(wrapper.diffAnaLimma(obj, "10fmol", "5fmol"),4), limmaRes)
# })


# test_that("Compute Welch", {
#     data(Exp1_R2_prot)
#     obj <- Exp1_R2_prot[c(1:6,8:11)]
#     
# welchRes <- data.frame(P_Value=c(0.3114,0.1906,0.0000,0.2729,0.3618,0.3898,
#                                     0.0021,0.2582,0.2855,0.0033),
#                         logFC=c(0.4416,0.0554,-1.0852,0.4476,1.6296, 0.2944,
#                                 0.9114,0.1111,-0.5349,-1.0077),
#                         row.names = 1:10)
# 
# expect_equal(round(wrapper.diffAnaWelch(obj, "10fmol", "5fmol"),4), welchRes)
# })
