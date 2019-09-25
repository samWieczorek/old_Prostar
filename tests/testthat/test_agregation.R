 context("Agregation peptides to proteins")
 
 require(DAPARdata)
# 
# #########################################################
# test_that("Build Adjacency Matrix with sparse Matrix", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     
#     matUnique <- matrix(rep(0,72), 9,8, dimnames=list(c(0:8), c("1212", "253", "360", "375", "1050", "1995", "595", "856")))
#     matUnique[1,1] <- matUnique[2:3,2] <- matUnique[4,3] <-   matUnique[5,4] <-  matUnique[6,5] <-  matUnique[7,6] <-  matUnique[8,7] <- matUnique[9,8] <- 1
#     
#     
#     
#     matShared <-matrix(rep(0,100), 10,10, dimnames=list(c(0:9),NULL))
#     matShared[1,1] <-  matShared[2:3,2]<-  matShared[4,3] <-   matShared[5,4] <-  matShared[6,5] <-  matShared[7,6] <-  matShared[8,7] <-  matShared[9,8] <-  matShared[10,9] <-matShared[10,10] <-  1
#     
#     computedMatUnique  <- BuildAdjacencyMatrix(obj, 
#                                                "Protein.group.IDs", 
#                                                unique=TRUE)
#     computedMatShared  <- BuildAdjacencyMatrix(obj, 
#                                                "Protein.group.IDs", 
#                                                unique=FALSE)
#     colnames(computedMatShared) <- NULL
#     
#     expect_equal(as(matUnique, "dgCMatrix"), computedMatUnique[,colnames(computedMatUnique)])
#     expect_equal(as(matShared, "dgCMatrix"), computedMatShared)
# })
# 
# 
# 
# 
# 
# 
# #########################################################
# test_that("Sum of shared peptides using sparse Matrices", {
#     
#     require(DAPARdata)
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     
#     protID <- "Protein.group.IDs"
#     
#     computedMShared <- BuildAdjacencyMatrix(obj, protID, unique=FALSE)
#     sumShared <- matrix(c( 25.278, 24.996, 24.487, 25.502, 25.025, 24.691,
#                            48.949, 48.702, 48.546, 49.512, 49.208, 49.416,
#                            24.088, 23.866, 24.335, 24.532, 24.674, 24.766,
#                            24.607, 24.836, 24.414, 24.455, 24.608, 24.921,
#                            0.000, 0.000, 0.000, 0.000, 22.180, 21.932,
#                            24.684, 24.302, 24.255, 24.937, 24.614, 24.584,
#                            27.113, 27.180, 27.270, 27.380, 27.351, 27.383,
#                            21.839, 21.811, 23.116, 21.859, 22.157, 0.000,
#                            31.092, 31.104, 31.195, 31.208, 31.213, 31.299,
#                            31.092, 31.104, 31.195, 31.208, 31.213, 31.299),byrow=TRUE, 10,6, 
#                         dimnames=list(c("1212","253","360","375","1050","1995" ,"595", "856","115" ,"114"), 
#                                       c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptSharedUsed <- matrix(c( 1,1,1,1,1,1,
#                                 2,2,2,2,2,2,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1),byrow=TRUE, 10,6, 
#                              dimnames=list(c("1212","253","360","375","1050","1995" ,"595", "856","115" ,"114"), 
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     
#     
#     sumOfMatShared <- SumPeptides(computedMShared, Biobase::exprs(obj))
#     expect_equal(as(sumShared, "dgeMatrix"),  round(sumOfMatShared$matfin, digits = 3))
#     expect_equal(as(peptSharedUsed, "dgeMatrix"), sumOfMatShared$nbpep)
# })
# 
# 
# 
# 
# 
# #########################################################
# test_that("Sum of unique peptides with sparse Matrices", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     protID <- "Protein.group.IDs"
#     
#     computedMUnique <- BuildAdjacencyMatrix(obj, protID, unique=TRUE)
#     sumUnique <- matrix(c(25.278,24.996,24.487,25.502,25.025,24.691,
#                           48.949,48.702,48.546,49.512,49.208,49.416,
#                           24.088,23.866,24.335,24.532,24.674,24.766,
#                           24.607,24.836,24.414,24.455,24.608,24.921,
#                           0.000,0.000,0.000,0.000,22.180,21.932,
#                           24.684,24.302,24.255,24.937,24.614,24.584,
#                           27.113,27.180,27.270,27.380,27.351,27.383,
#                           21.839,21.811,23.116,21.859,22.157,0.000),byrow=TRUE, 8,6, 
#                         dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"), c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptUniqueUsed <- matrix(c( 1,1,1,1,1,1,
#                                 2,2,2,2,2,2,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1),byrow=TRUE, 8,6, 
#                              dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"),
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     
#     
#     sumOfMatUnique <- SumPeptides(computedMUnique, Biobase::exprs(obj))
#     expect_equal(as(sumUnique, "dgeMatrix"),round(sumOfMatUnique$matfin, digits=3))
#     expect_equal(as(peptUniqueUsed, "dgeMatrix"), sumOfMatUnique$nbpep)
# })
# 
# 
# 
# 
# 
# 
# #########################################################
# test_that("Mean of unique peptides with sparse Matrix", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     protID <- "Protein.group.IDs"
#     
#     computedMUnique <- BuildAdjacencyMatrix(obj, protID, unique=TRUE)
#     meanUnique <- matrix(c(25.278,24.996,24.487,25.502,25.025,24.691,
#                            24.474,24.351,24.273,24.756,24.604,24.708,
#                            24.088,23.866,24.335,24.532,24.674,24.766,
#                            24.607,24.836,24.414,24.455,24.608,24.921,
#                            0.000,0.000,0.000,0.000,22.180,21.932,
#                            24.684,24.302,24.255,24.937,24.614,24.584,
#                            27.113,27.180,27.270,27.380,27.351,27.383,
#                            21.839,21.811,23.116,21.859,22.157,0.000
#     ),byrow=TRUE, 8,6, 
#     dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"),
#                   c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptUniqueUsed <- matrix(c( 1,1,1,1,1,1,
#                                 2,2,2,2,2,2,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1),byrow=TRUE, 8,6, 
#                              dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"), 
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     
#     meanOfMatUnique <- MeanPeptides(computedMUnique, exprs(obj))
#     expect_equal(as(meanUnique, "dgeMatrix"), round(meanOfMatUnique$matfin, digits=3))
#     expect_equal(as(peptUniqueUsed, "dgeMatrix"),  meanOfMatUnique$nbpep)
# })
# 
# 
# 
# 
# 
# #########################################################
# test_that("Mean of SHARED peptides with sparse Matrix", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     protID <- "Protein.group.IDs"
#     
#     computedMShared <- BuildAdjacencyMatrix(obj, protID, unique=FALSE)
#     meanShared <- 
#         matrix(c(25.278,24.996,24.487,25.502,25.025,24.691,
#                  24.474,24.351,24.273,24.756,24.604,24.708,
#                  24.088,23.866,24.335,24.532,24.674,24.766,
#                  24.607,24.836,24.414,24.455,24.608,24.921,
#                  0.000,0.000,0.000,0.000,22.180,21.932,
#                  24.684,24.302,24.255,24.937,24.614,24.584,
#                  27.113,27.180,27.270,27.380,27.351,27.383,
#                  21.839,21.811,23.116,21.859,22.157,0.000,
#                  31.092,31.104,31.195,31.208,31.213,31.299,
#                  31.092,31.104,31.195,31.208,31.213,31.299),
#                byrow=TRUE, 10,6, 
#                dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856", "115", "114"),
#                              c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptSharedUsed <- matrix(c( 1,1,1,1,1,1,
#                                 2,2,2,2,2,2,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1),byrow=TRUE, 10,6, 
#                              dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856", "115", "114"),
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     meanOfMatShared <- MeanPeptides(computedMShared, Biobase::exprs(obj))
#     expect_equal(as(meanShared, "dgeMatrix"), round(meanOfMatShared$matfin, digits=3), tolerance=1e-5)
#     expect_equal(as(peptSharedUsed, "dgeMatrix"),meanOfMatShared$nbpep)
# })
# 
# 
# 
# 
# 
# 
# #########################################################
# test_that("Top 3 of SHARED peptides with sparse Matrix", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     n <- 3
#     protID <- "Protein.group.IDs"
#     
#     computedMShared <- BuildAdjacencyMatrix(obj, protID, unique=FALSE)
#     topnShared <- matrix(c( 25.278, 24.996, 24.487, 25.502, 25.025, 24.691,
#                             48.949, 48.702, 48.546, 49.512, 49.208, 49.416,
#                             24.088, 23.866, 24.335, 24.532, 24.674, 24.766,
#                             24.607, 24.836, 24.414, 24.455, 24.608, 24.921,
#                             0.000, 0.000, 0.000, 0.000, 22.180, 21.932,
#                             24.684, 24.302, 24.255, 24.937, 24.614, 24.584,
#                             27.113, 27.180, 27.270, 27.380, 27.351, 27.383,
#                             21.839, 21.811, 23.116, 21.859, 22.157, 0.000,
#                             31.092, 31.104, 31.195, 31.208, 31.213, 31.299,
#                             31.092, 31.104, 31.195, 31.208, 31.213, 31.299),
#                          byrow=TRUE, 10,6, 
#                          dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856", "115", "114"), 
#                                        c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptSharedUsed <- matrix(c( 1,1,1,1,1,1,
#                                 2,2,2,2,2,2,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1,
#                                 1,1,1,1,1,1),byrow=TRUE, 10,6, 
#                              dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856", "115", "114"),
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     
#     
#     topnOfMatShared <- TopnPeptides(computedMShared, Biobase::exprs(obj), n)
#     expect_equal(as(topnShared, "dgeMatrix"), topnOfMatShared$matfin, tolerance=1e-5)
#     expect_equal(as(peptSharedUsed, "dgeMatrix"),topnOfMatShared$nbpep)
# })
# 
# 
# 
# 
# 
# 
# 
# 
# #########################################################
# test_that("Top 3 of UNIQUE peptides with sparse matrices", {
#     
#     data(Exp1_R2_pept)
#     obj <- Exp1_R2_pept[1:10]
#     n <- 3
#     protID <- "Protein.group.IDs"
#     
#     computedMUnique <- BuildAdjacencyMatrix(obj, protID, unique=TRUE)
#     topnUnique <- matrix(c( 25.278, 24.996, 24.487, 25.502, 25.025, 24.691,
#                             48.949, 48.702, 48.546, 49.512, 49.208, 49.416,
#                             24.088, 23.866, 24.335, 24.532, 24.674, 24.766,
#                            24.607, 24.836, 24.414, 24.455, 24.608, 24.921,
#                            0.000, 0.000, 0.000 ,0.000, 22.180, 21.932,
#                            24.684, 24.302, 24.255, 24.937, 24.614, 24.584,
#                            27.113, 27.180, 27.270, 27.380, 27.351, 27.383,
#                            21.839 ,21.811, 23.116, 21.859, 22.157, 0.000),
#                          byrow=TRUE, 8,6, 
#                          dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"), 
#                                        c("Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3", "Intensity.E.R1", "Intensity.E.R2", "Intensity.E.R3")))
#     
#     peptUniqueUsed <- matrix(c(1,1,1,1,1,1,
#                              2,2,2,2,2,2,
#                              1,1,1,1,1,1,
#                              1,1,1,1,1,1,
#                              1,1,1,1,1,1,
#                              1,1,1,1,1,1,
#                              1,1,1,1,1,1,
#                              1,1,1,1,1,1),byrow=TRUE, 8,6, 
#                              dimnames=list(c("1212", "253", "360", "375", "1050", "1995", "595", "856"),
#                                            c("nb.pep.used.Intensity.D.R1", "nb.pep.used.Intensity.D.R2", "nb.pep.used.Intensity.D.R3", "nb.pep.used.Intensity.E.R1", "nb.pep.used.Intensity.E.R2", "nb.pep.used.Intensity.E.R3")))
#     
#     
#     topnOfMatUnique <- TopnPeptides(computedMUnique, Biobase::exprs(obj), n)
#     expect_equal(as(topnUnique, "dgeMatrix"), topnOfMatUnique$matfin , tolerance=1e-5)
#     expect_equal(as(peptUniqueUsed, "dgeMatrix"), topnOfMatUnique$nbpep)
# })
