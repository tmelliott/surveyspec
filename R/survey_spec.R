#' Parse survey to survey spec
#'
#' @param x an object which can be converted to a survey spec (e.g., survey.design)
#' @param ... additional arguments, used for methods
#' @return an `inzsvydesign` object, see [import_survey]
#' @md
#' @export
as_survey_spec <- function(x, ...) UseMethod("as_survey_spec")

#' @describeIn as_survey_spec Method for survey.design objects
#' @param des a survey design object, needed for calibrated/poststratified surveys
#' @export
as_survey_spec.survey.design <- function(x, des, ...) {
    if (!is.null(x$postStrat)) {
        if (missing(des)) stop("Please provide original design object with `des` argument.")
        spec <- as_survey_spec(des)$spec
        svy_cal <- parse_calibration(x$call)
        spec$calibrate <- svy_cal$calibrate
        spec$calfun <- svy_cal$calfun

        spec <- list(
            spec = spec,
            data = x$variables,
            design = x
        )
        class(spec) <- "inzsvyspec"
        return(spec)
    }
    get_arg <- function(x, arg) {
        x <- x$call
        orNULL(x[[arg]], as.character(x[[arg]])[2])
    }
    spec <- list(
        spec = list(
            ids = get_arg(x, 2),
            probs = get_arg(x, "probs"),
            strata = get_arg(x, "strata"),
            fpc = get_arg(x, "fpc"),
            nest = get_arg(x, "nest"),
            weights = get_arg(x, "weights"),
            type = "survey"
        ),
        data = x$variables,
        design = x
    )
    class(spec) <- "inzsvyspec"
    spec
}

#' as_survey method
#'
#' @importFrom srvyr as_survey
#' @name as_survey
#' @rdname as_survey.inzsvyspec
#' @export
NULL

#' Coerce to survey design
#'
#' Coerce an object to a survey design by extracting the survey object
#'
#' @param .data an `inzsvyspec` object
#' @param ... additional arguments, ignored
#' @return a survey design object of class 'tbl_svy' (from 'srvyr')
#' @export
#' @md
as_survey.inzsvyspec <- function(.data, ...) {
    srvyr::as_survey(.data$design)
}

#' Print iNZight Survey Spec
#'
#' @param x a `inzsvyspec` object
#' @param ... additional arguments, ignored
#' @return Called for the side-effect of printing to the console.
#' @md
#' @export
print.inzsvyspec <- function(x, ...) {
    cat("Survey design specification:\n")
    s <- x$spec
    lapply(names(s),
        function(y) {
            if (is.null(s[[y]])) return()
            if (y == "calibrate") {
                cat(sprintf(" * %s: %s\n",
                    y,
                    paste(names(s[[y]]), collapse = " + ")
                ))
            } else {
                cat(sprintf(" * %s: %s\n", y, s[[y]]))
            }
        }
    )

    cat("\nDesign object: ")
    if (is.null(x$design)) {
        cat("empty\n")
    } else {
        cat("\n")
        print(x$design)
    }
}

parse_calibration <- function(x) {
    cal_list <- as.list(x)
    fmla <- if (!is.null(cal_list$formula)) cal_list$formula else cal_list[[3]]
    popn <- if (!is.null(cal_list$population)) cal_list$population else cal_list[[4]]

    if (is.call(popn)) popn <- eval(popn)

    # need to check if fmla is a list or single formula
    cal <- list(
        formula = paste(as.character(fmla)[-1], sep = " + "),
        population = as.numeric(popn)
    )

    # should return a list of calibration informations in 'spec' format
    list(
        calibrate = list(cal),
        calfun = if (is.null(x$calfun)) "linear" else x$calfun
    )
}
