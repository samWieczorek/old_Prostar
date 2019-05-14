context("Prostar")

# file_loc <- getwd()
# if (length(grep(".Rcheck", file_loc)) == 1){
#     path2App <- "../../Prostar/ProstarApp"
# }else if (length(grep("Prostar-tests", file_loc)) == 1){
#     path2App <- "../../ProstarApp"
#     } else {
#     path2App <- "../../inst/ProstarApp"
# }

# test_that("Opening Prostar", {
#     app <- ShinyDriver$new(path2App)
#     title <- app$getTitle()
#     expect_equal(title, "Prostar")
# })


test_that("Opening Prostar 2", {
    expect_null(Prostar())
})
