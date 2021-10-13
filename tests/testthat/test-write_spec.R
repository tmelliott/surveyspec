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
