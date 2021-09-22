data(api, package = "survey")

test_that("Importing survey specification format", {
    s <- import_survey('apiclus2.svydesign', apiclus2)

})
