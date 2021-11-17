require(survey)
data(api)

test_that("Survey design file parsed correctly", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    svytoml <-
'strata = "stype"
weights = "pw"
fpc = "fpc"
'
    writeLines(svytoml, svyfile)

    s <- import_survey(svyfile)
    expect_s3_class(s, "inzsvyspec")
    expect_null(s$design)

    s2 <- import_survey(svyfile, apistrat)
    expect_s3_class(s2, "inzsvyspec")
    expect_s3_class(s2$design, "survey.design")

    expect_equal(
        {data <- apistrat; make_survey(data, s)},
        s2,
        ignore_formula_env = TRUE
    )

    expect_output(print(s), "empty")
    expect_output(print(s2), "Stratified Independent Sampling design")
})

test_that("Survey design file parsed correctly", {
    svyfile <- tempfile("apiclus2", fileext = ".svydesign")
    svytoml <-
'ids = "dnum + snum"
weights = "pw"
fpc = "fpc1 + fpc2"
'
    writeLines(svytoml, svyfile)

    s <- import_survey(svyfile)
    expect_s3_class(s, "inzsvyspec")
    expect_null(s$design)

    s2 <- import_survey(svyfile, apiclus2)
    expect_s3_class(s2, "inzsvyspec")
    expect_s3_class(s2$design, "survey.design")

    expect_equal(
        {data <- apiclus2; make_survey(data, s)},
        s2,
        ignore_formula_env = TRUE
    )

    expect_output(print(s), "empty")
    expect_output(print(s2), "2 - level Cluster Sampling design")

    svyfile <- tempfile("apiclus2", fileext = ".svydesign")
    svytoml <-
'clusters = "dnum + snum"
weights = "pw"
fpc = "fpc1 + fpc2"
'
    writeLines(svytoml, svyfile)
    s3 <- import_survey(svyfile, apiclus2)
    expect_equal(s2, s3, ignore_formula_env = TRUE)
})

test_that("Survey spec with data imported OK", {
    datfile <- tempfile("apiclus2", fileext = ".rds")
    on.exit(unlink(datfile))

    svyfile <- tempfile("apiclus2", fileext = ".svydesign")
    on.exit(unlink(svyfile), add = TRUE)

    svytoml <- sprintf(
'ids = "dnum + snum"
weights = "pw"
fpc = "fpc1 + fpc2"
data = "%s"
', basename(datfile))

    saveRDS(apiclus2, file = datfile)
    writeLines(svytoml, svyfile)

    s <- import_survey(svyfile, read_fun = readRDS)
    expect_equal(s$data, apiclus2)
    expect_s3_class(s$design, "survey.design")
})

test_that("Replicate weight designs", {
    skip_if_offline()
    skip_on_cran()

    chis_url <- "https://inzight.nz/testdata/chis.csv"
    skip_if_not(RCurl::url.exists(chis_url))

    data <- try(iNZightTools::smart_read(chis_url), silent = TRUE)
    skip_if(inherits(data, "try-error"))

    svyfile <- tempfile("chis", fileext = ".svydesign")
    svytoml <-
'repweights = "rakedw[1-9]"
weights = "rakedw0"
type = "other"
scale = "~2/1 - 1"
rscales = 1
'
    writeLines(svytoml, svyfile)

    dchis <- svrepdesign(
        weights = ~rakedw0,
        repweights = "rakedw[1-9]",
        type = "other",
        scale = 1,
        rscales = 1,
        data = data
    )

    s <- import_survey(svyfile, data)
    expect_s3_class(s, "inzsvyspec")

    expect_s3_class(s$design, "svyrep.design")
    expect_equal(
        make_survey(data, s)$design,
        dchis,
        ignore_attr = TRUE
    )
})

test_that("Replicate design has same defaults as survey::svrepdesign()", {
    skip_if_offline()
    skip_on_cran()

    chis_url <- "https://inzight.nz/testdata/chis.csv"
    skip_if_not(RCurl::url.exists(chis_url))

    data <- try(iNZightTools::smart_read(chis_url), silent = TRUE)
    skip_if(inherits(data, "try-error"))

    svyfile <- tempfile("chis", fileext = ".svydesign")
    svytoml <-
'repweights = "rakedw[1-9]"
weights = "rakedw0"
'
    writeLines(svytoml, svyfile)

    dchis <- svrepdesign(
        weights = ~rakedw0,
        repweights = "rakedw[1-9]",
        data = data
    )

    s <- import_survey(svyfile, data)
    expect_s3_class(s, "inzsvyspec")

    expect_s3_class(s$design, "svyrep.design")
    expect_equal(
        make_survey(data, s)$design,
        dchis,
        ignore_attr = TRUE
    )
})

test_that("Poststratification", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    svyTOML <- 'strata = "stype"
weights = "pw"
fpc = "fpc"

[calibrate.stype]
E = 4421
H = 755
M = 1018

[calibrate."sch.wide"]
"No" = 1072
"Yes" = 5122
'
    writeLines(svyTOML, svyfile)

    s <- import_survey(svyfile, apistrat)
    expect_s3_class(s, "inzsvyspec")
    expect_s3_class(s$design, "survey.design")

    expect_output(print(s), "survey::calibrate")
})

test_that("Survey designs can be parsed as survey spec", {
    dclus2 <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)
    dsvy <- as_survey_spec(dclus2)
    expect_s3_class(dsvy, "inzsvyspec")
    expect_equal(dsvy$design, dclus2)
    expect_equal(dsvy$data, apiclus2)
    expect_equal(
        dsvy$spec,
        list(
            ids = "dnum + snum",
            probs = NULL,
            strata = NULL,
            fpc = "fpc1 + fpc2",
            nest = NULL,
            weights = "pw",
            type = "survey"
        )
    )
})

test_that("Survey spec can be converted to survey design (srvyr)", {
    dclus2 <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)
    dsvy <- as_survey_spec(dclus2)
    expect_equal(as_survey(dsvy), srvyr::as_survey(dclus2))
})
