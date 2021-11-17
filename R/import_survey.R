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
#' @param file the file containing survey information (see Details)
#' @param data optional, if supplied the survey object will be created with the supplied data.
#'        Can be either a data.frame-like object, or a path to a data set which
#'        will be imported using `iNZightTools::smart_read`.
#' @param read_fun function required to load the data specified in `file`
#' @param ... additional arguments to `read_fun`
#' @return a `inzsvyspec` object containing the design parameters and, if data supplied,
#'         the created survey object
#' @author Tom Elliott
#' @describeIn import_survey Import survey information from a file
#' @export
#' @md
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
                reptype = spec$reptype,
                scale =
                    if (is.null(spec$reptype) || spec$reptype %in% c(no_scale_types)) {
                        NULL
                    } else {
                        spec$scale
                    },
                rscales =
                    if (is.null(spec$reptype) || spec$reptype %in% c(no_scale_types)) {
                        NULL
                    } else {
                        as.numeric(spec$rscales)
                    },
                ## this will become conditional on what fields are specified
                type = ifelse("repweights" %in% names(spec), "replicate", "survey"),
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
