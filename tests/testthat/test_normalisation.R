context("Test of normalization methods")
require(DAPARdata)
data(Exp1_R25_prot)
test <- Exp1_R25_prot[1:10]

# 
# test_that("Wrapper / Global Alignment, alignment on all quantiles", {
#     
#     
#     norm <- matrix(c(21.2630, 21.2630, 21.2630, 21.2630, 21.2630, 21.2630,
#                      32.4831, 32.4831, 32.4831, 32.4831, 32.4831, 32.4831,
#                      30.6154, 30.6154, 30.6154, 30.6154, 30.4524, 30.6154,
#                      25.5100, 25.5100, 25.5100, 25.8686, 25.6893, 25.8686,
#                      29.3113, 29.3113, 29.3113, 29.3113, 28.6668, 29.3113,
#                      23.0180, 22.4839, 23.0180, 22.4839, 22.5507, 22.4839,
#                      24.4794, 24.4794, 24.4794, 24.4794, NA, 24.4794,
#                      22.4839, 23.0180, 22.4839, 23.0180, 23.3834, 23.0180,
#                      25.8686, 25.8686, 25.8686, 26.7330, 26.4088, 26.7330,
#                      26.7330, 26.7330, 26.7330, 25.5100, 24.8658 ,25.5100),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     funcNorm <- wrapper.normalizeD2(test, "Global Alignment", "Alignment on all quantiles")
#     expect_equal(round(Biobase::exprs(funcNorm),4), norm)
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Global Alignment", "Alignment on all quantiles")
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# # 
# # test_that("Wrapper / Global Alignment, sum by columns", {
# #     
# #     
# #     norm <- matrix(c(-11.8555, -11.8433, -11.8449 ,-11.5694 ,-11.9095, -11.5044,
# #                      -0.6892, -0.6935, -0.7088, -0.3674, -0.3810, -0.3666,
# #                      -1.8484, -1.8581, -1.7915, -3.0836, -3.0601, -3.0688,
# #                      -7.5450, -7.0893, -7.0051, -8.0411, -7.3320, -7.4227,
# #                      -3.8876, -3.7775, -3.9966, -3.5466, -3.4853, -3.6381,
# #                      -9.2961, -10.6796, -9.9086, -10.3036, -11.1436, -10.7440,
# #                      -7.6777, -8.0457, -7.4535, -9.7443, NA, -9.6292,
# #                      -10.2751, -9.7293, -9.9708, -10.2759, -10.4409, -10.1901,
# #                      -6.6988, -6.9163, -6.8377, -6.5558, -6.5341, -6.4313,
# #                       -6.3824, -6.4527, -6.3675, -8.0481, -7.7976, -7.8190),10,6, byrow=TRUE)
# #     
# #     norm <- round(norm, 4)
# #     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
# #                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
# #     rownames(norm) <- c(0:9)
# #     
# #     funcNorm <- wrapper.normalizeD2(test, "Global Alignment", "sum by columns")
# #     expect_equal(round(Biobase::exprs(funcNorm),4), norm)
# #     
# #     labels <- Biobase::pData(test)[,"Condition"]
# #     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
# #                             "Global Alignment", "sum by columns")
# #     expect_equal(round(funcNorm,4), norm)
# # })
# # 
# 
# test_that("Wrapper Quantile Centering, overall, quantile=0.25", {
#     
#     
#     norm <- matrix(c(19.7994, 20.2284, 20.2133, 21.3369, 21.2948, 21.3089,
#                      30.9657, 31.3783, 31.3494, 32.5390, 32.8233, 32.4467,
#                      29.8065, 30.2137, 30.2667, 29.8228, 30.1441, 29.7445,
#                      24.1099, 24.9825, 25.0531, 24.8653, 25.8723, 25.3906,
#                      27.7673, 28.2942, 28.0616, 29.3597, 29.7190, 29.1752,
#                      22.3588, 21.3922, 22.1496, 22.6028, 22.0607, 22.0693,
#                      23.9772, 24.0261, 24.6047, 23.1621, NA, 23.1841,
#                      21.3797, 22.3425, 22.0874, 22.6305, 22.7634, 22.6231,
#                      24.9561, 25.1554, 25.2205, 26.3506, 26.6702, 26.3820,
#                      25.2725, 25.6191, 25.6907, 24.8583, 25.4067, 24.9942),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     funcNorm <- wrapper.normalizeD2(test, "Quantile Centering", "overall", quantile=0.25)
#     expect_equal(round(Biobase::exprs(funcNorm),4), norm)
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Quantile Centering", "overall", quantile=0.25)
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# 
# 
# 
# test_that("Wrapper Quantile Centering, within conditions, quantile=0.25", {
#     
#     
#     norm <- matrix(c(20.9087, 21.3378, 21.3227, 21.1286, 21.0865, 21.1006,
#                      32.0751, 32.4876, 32.4588, 32.3306, 32.6149, 32.2384,
#                      30.9159, 31.3231, 31.3761, 29.6145, 29.9358, 29.5362,
#                      25.2192, 26.0919, 26.1625, 24.6570, 25.6639, 25.1823,
#                      28.8767, 29.4036, 29.1710, 29.1514, 29.5107, 28.9669,
#                      23.4681, 22.5015, 23.2589, 22.3945, 21.8524, 21.8610,
#                      25.0865, 25.1355, 25.7141, 22.9537, NA, 22.9757,
#                      22.4891, 23.4518, 23.1968, 22.4221, 22.5550, 22.4148,
#                      26.0654, 26.2648, 26.3299, 26.1423, 26.4619, 26.1737,
#                      26.3818, 26.7285, 26.8000, 24.6500, 25.1984, 24.7859),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     funcNorm <- wrapper.normalizeD2(test, "Quantile Centering", "within conditions", quantile=0.25)
#     expect_equal(round(Biobase::exprs(funcNorm),4), norm)
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                            "Quantile Centering", "within conditions", quantile=0.25)
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# 
# test_that("Mean Centering, within conditions, scaling=TRUE", {
#     
#     
#     norm <- matrix(c(25.07700, 25.14353, 25.07271, 24.62039, 24.54830, 24.61753,
#                      28.19567, 28.17688, 28.18229, 27.64495, 27.45702, 27.62711,
#                      27.87192, 27.86006, 27.87997, 26.91158, 26.78105, 26.89694,
#                      26.28089, 26.43690, 26.42414, 25.57305, 25.70323, 25.72046,
#                      27.30239, 27.33786, 27.26423, 26.78656, 26.67379, 26.74310,
#                      25.79182, 25.46013, 25.61337, 24.96217, 24.74153, 24.82300,
#                      26.24383, 26.17670, 26.29893, 25.11317, NA, 25.12422,
#                      25.51839, 25.71866, 25.59602, 24.96964, 24.91883, 24.97266,
#                      26.51723, 26.48394, 26.47089, 25.97409, 25.90455, 25.98835,
#                      26.60560, 26.61008, 26.60218, 25.57116, 25.58576, 25.61336),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Mean Centering", "within conditions", scaling=TRUE)
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# 
# 
# 
# test_that("Mean Centering, within conditions, scaling=FALSE", {
#     
#     
#     norm <- matrix(c(21.3005, 21.4057, 21.2841, 21.3968, 20.8014, 21.3897,
#                      32.4669, 32.5555, 32.4202, 32.5988, 32.3299, 32.5275,
#                      31.3077, 31.3909, 31.3375, 29.8827, 29.6508, 29.8253,
#                      25.6110, 26.1597, 26.1239, 24.9252, 25.3789, 25.4714,
#                      29.2685, 29.4715, 29.1324, 29.4196, 29.2257, 29.2560,
#                      23.8599, 22.5694, 23.2203, 22.6627, 21.5673, 22.1501,
#                      25.4783, 25.2033, 25.6755, 23.2219, NA, 23.2649,
#                      22.8809, 23.5197, 23.1582, 22.6903, 22.2700, 22.7040,
#                      26.4573, 26.3327, 26.2913, 26.4105, 26.1768, 26.4628,
#                      26.7736, 26.7963, 26.7614, 24.9182, 24.9133, 25.0751),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Mean Centering", "within conditions", scaling=FALSE)
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# 
# 
# test_that("Mean Centering, overall, scaling=FALSE", {
#     
#     
#     norm <- matrix(c(20.9366, 21.0418, 20.9202, 21.7607, 21.1653, 21.7536,
#                      32.1030, 32.1916, 32.0563, 32.9627, 32.6938, 32.8914,
#                      30.9438, 31.0270, 30.9736, 30.2466, 30.0147, 30.1892,
#                      25.2471, 25.7958, 25.7600, 25.2891, 25.7428, 25.8353,
#                      28.9046, 29.1076, 28.7685, 29.7835, 29.5896, 29.6199,
#                      23.4960, 22.2055, 22.8564, 23.0266, 21.9312, 22.5140,
#                      25.1144, 24.8394, 25.3116, 23.5858, NA, 23.6288,
#                      22.5170, 23.1558, 22.7943, 23.0542, 22.6339, 23.0678,
#                      26.0934, 25.9688, 25.9274, 26.7744, 26.5407, 26.8267,
#                      26.4097, 26.4324, 26.3975, 25.2821, 25.2772, 25.4390),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Mean Centering", "overall", scaling=FALSE)
#     expect_equal(round(funcNorm,4), norm)
# })
# 
# 
# 
# 
# 
# test_that("Mean Centering, overall, scaling=TRUE", {
#     
#     
#     norm <- matrix(c(24.7131, 24.7796, 24.7088, 24.9843, 24.9122, 24.9814,
#                      27.8318, 27.8130, 27.8184, 28.0089, 27.8209, 27.9910,
#                      27.5080, 27.4962, 27.5161, 27.2755, 27.1450, 27.2608,
#                      25.9170, 26.0730, 26.0602, 25.9369, 26.0671, 26.0844,
#                      26.9385, 26.9740, 26.9003, 27.1505, 27.0377, 27.1070,
#                      25.4279, 25.0962, 25.2495, 25.3261, 25.1054, 25.1869,
#                      25.8799, 25.8128, 25.9350, 25.4771,      NA, 25.4881,
#                      25.1545, 25.3548, 25.2321, 25.3335, 25.2827, 25.3366,
#                      26.1533, 26.1200, 26.1070, 26.3380, 26.2685, 26.3523,
#                      26.2417, 26.2462, 26.2383, 25.9351, 25.9497, 25.9773),10,6, byrow=TRUE)
#     
#     norm <- round(norm, 4)
#     colnames(norm) <- c("Intensity.C.R1", "Intensity.C.R2", "Intensity.C.R3", 
#                         "Intensity.D.R1", "Intensity.D.R2", "Intensity.D.R3")
#     rownames(norm) <- c(0:9)
#     
#     
#     labels <- Biobase::pData(test)[,"Condition"]
#     funcNorm <- normalizeD2(Biobase::exprs(test), labels, 
#                             "Mean Centering", "overall", scaling=TRUE)
#     expect_equal(round(funcNorm,4), norm)
# })