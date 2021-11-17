require(survey)
data(api)

test_that("Survey design file written correctly", {
    data <- apiclus2
    des <- survey::svydesign(ids=~dnum+snum, fpc = ~fpc1+fpc2, data = data)

    f <- tempfile(fileext = ".svydesign")
    on.exit(unlink(f))

    write_spec(des, f)

    expect_equal(
        readLines(f),
        c("ids = \"dnum + snum\"", "fpc = \"fpc1 + fpc2\"", "type = \"survey\"")
    )

    expect_equal(
        import_survey(f, data)$design,
        des,
        ignore_attr = TRUE
    )
})

test_that("Survey design for stratified, unclustered data", {
    data <- apistrat
    des <- survey::svydesign(ids = ~1, strata = ~stype,
        fpc = ~fpc, weights = ~pw, data = data)

    f <- tempfile(fileext = ".svydesign")
    on.exit(unlink(f))

    write_spec(des, f)

    expect_equal(
        readLines(f),
        c("strata = \"stype\"", "fpc = \"fpc\"", "weights = \"pw\"", "type = \"survey\"")
    )

    expect_equal(
        import_survey(f, data)$design,
        des,
        ignore_attr = TRUE
    )
})

test_that("Errors returned if object not supported", {
    expect_error(
        write_spec(iris)
    )
})
