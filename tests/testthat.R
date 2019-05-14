Sys.setenv("R_TESTS" = "")

library(testthat)
#library(shinytest)
library(DAPAR)
library(DAPARdata)

test_check("Prostar")
