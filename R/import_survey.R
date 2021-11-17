#' Import survey information from a file
#'
#' The survey information should be in TOML format, with fields
#' corresponding to survey design components. For example,
#' ```
#' strata = strata_var
#' clusters = cluster_var
#' weights = wt_var
#' ```
#'
#' For replicate weight designs, vectors (if necessary) are declared with
#' square brackets, like so:
#' ```
#' repweights = ['w01', 'w02', 'w03', 'w04', ..., 'w20']
#' ```
#' although this would be better expressed using a regular expression,
#' ```
#' repweights = '^w[0-2]'
#' ```
#' which matches all variables starting with a `w` followed by digits between 0 and 2 (inclusive).
#'
#' Additionally, the information can contain a `file` specification
#' indicating the path to the data, which will be imported (if it exists
#' in the same directory as `file`) using `read_fun`, if specified;
#' or alternatively `file` can be a URL to a data file that will be downloaded
#' and read, again using `read_fun`.
#'
#' If the data is not specified to `import_survey`, then `make_survey` can be used to manually
#' construct an `inzsvyspec` object with the design attached. This might be useful if you have
#' multiple datasets with the same design, for example.
#'
#' `import_survey` calls `make_survey` when data is provided.
#'
#' @section Specification format:
#'
#' The survey design specification file used by 'surveyspec' should be in
#' [TOML](https://github.com/toml-lang/toml) format.
#' This allows for a very human-readable syntax,
#' ```
#' arg = "value"
#' ```
#' and additionally some additional complexity where needed (for example when specifying
#' calibration information).
#'
#' For stratified and clustering samples, each argument to `survey::svydesign` can be
#' given on its own line. So for a stratified sample using the `apistrat` data from the
#' 'survey' package, the following specification would suffice:
#' ```
#' strata = "stype"
#' weights = "pw"
#' fpc = "fpc"
#' ```
#' For a cluster sample, we instead can provide either `clusters` or `ids` (the former
#' makes it more obvious to beginners, while the latter is consistent with `svydesign()`).
#' For example, specifying the design for the `apiclus2` data:
#' ```
#' clusters = "dnum + snum"
#' fpc = "fpc1 + fpc2"
#' ```
#'
#' Alternatively, survey data may be distributed with replicate weights.
#' To specify this information to `import_survey()`, the same concept is used
#' but the arguments supplied should be based off those used in `survey::svrepdesign()`.
#' For example (taken from `?svrepdesign`):
#' ```
#' weights = "pw"
#' repweights = "wt[1-9]+"
#' type = "JK1"
#' scale = "~(1-15/757)*14/15"
#' combined = FALSE
#' ```
#' Note here that you can specify an expression for `scale`, but need to use
#' this syntax, "~expr", for it to be parsed correctly.
#'
#' Finally, survey design calibration can be performed by including this information
#' using TOML list syntax. For example, to calibrate the 'apistrat' data,
#' ```
#' strata = "stype"
#' weights = "pw"
#' fpc = "fpc"
#'
#' [calibrate.stype]
#' E = 4421
#' H = 755
#' M = 1018
#'
#' [calibrate."sch.wide"]
#' "No" = 1072
#' "Yes" = 5122
#' ```
#' Note importantly the use of quotes around variable names which include a period (.),
#' here `sch.wide`. Currently, only calibrating by a factor is possible.
#'
#' @param file the file containing survey information (see Details)
#' @param data optional, if supplied the survey object will be created with the supplied data.
#'        Can be either a data.frame-like object, or a path to a data set which
#'        will be imported using `iNZightTools::smart_read`.
#' @param read_fun function required to load the data specified in `file`
#' @param ... additional arguments to `read_fun`
#' @return a `inzsvyspec` object containing the design parameters and, if data supplied,
#'         the created survey object. The object is a list containing at least a 'spec'
#'         component, and if `data` is supplied then also 'data' and 'design' components.
#' @describeIn import_survey Import survey information from a file
#' @export
#' @md
#' @examples
#' library(survey)
#' data(api)
#' dstrat <- svydesign(ids = ~1, strata = ~stype, weights = ~pw,
#'  fpc = ~fpc, data = apistrat)
#'
#' f <- tempfile(fileext = ".svydesign")
#' write_spec(dstrat, f)
#'
#' cat(readLines(f), sep = "\n")
#'
#' (spec <- import_survey(f))
#' (svy <- make_survey(apistrat, spec))
#'
#' # or all in one:
#' (svy <- import_survey(f, data = apistrat))
import_survey <- function(file, data, read_fun, ...) {
    # spec <- as.data.frame(read.dcf(file), stringsAsFactors = FALSE)
    spec <- RcppTOML::parseTOML(file)

    no_scale_types <- c("BRR", "ACS", "successive-difference", "JK2")
    svyspec <- structure(
        list(
            spec = list(
                ids =
                    if ("clusters" %in% names(spec) && !is.null(spec$clusters)) spec$clusters
                    else if ("ids" %in% names(spec) && !is.null(spec$ids)) spec$ids
                    else 1,
                probs = spec$probs,
                strata = spec$strata,
                fpc = spec$fpc,
                nest = spec$nest,
                weights = spec$weights,
                repweights = spec$repweights,
                type = spec$type,
                scale =
                    if (is.null(spec$type) || spec$type %in% c(no_scale_types)) {
                        NULL
                    } else if (!is.null(spec$scale) && is.character(spec$scale) &&
                                grepl("^~", spec$scale)) {
                        eval(parse(text = gsub("^~", "", spec$scale)))
                    } else {
                        spec$scale
                    },
                rscales =
                    if (is.null(spec$type) || spec$type %in% c(no_scale_types)) {
                        NULL
                    } else {
                        as.numeric(spec$rscales)
                    },
                ## this will become conditional on what fields are specified
                survey_type = ifelse("repweights" %in% names(spec), "replicate", "survey"),
                calibrate = spec$calibrate
            )
        ),
        class = "inzsvyspec"
    )

    if (!is.null(spec$data)) {
        if (grepl("^https?://", spec$data)) {
            data <- spec$data
        } else {
            data <- file.path(dirname(file), spec$data)
            if (!file.exists(data)) data <- NULL
        }
    }

    if (!missing(data) && !is.null(data) && is.character(data)) {
        if (file.exists(data) || grepl("^https?://", data)) {
            if (missing(read_fun)) {
                if (requireNamespace("iNZightTools", quietly = TRUE))
                    read_fun <- iNZightTools::smart_read
                else {
                    read_fun <- NULL
                    warning(
                        sprintf("Please specify a function to read `%s`", data)
                    )
                }
            }
            if (!is.null(read_fun)) data <- read_fun(data, ...)
        }
    }

    if (missing(data) || !is.data.frame(data)) return(svyspec)

    make_survey(data, svyspec)
}
