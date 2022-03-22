#' Write a survey specification file
#'
#' Writes a survey specification file for the given survey design object.
#'
#' @param x a survey object
#' @param file the path of a file to write to
#' @param data_path the location of the data, relative. Only specify if distributing both, and relative paths will remain consistent.
#' @param ... additional arguments for methods
#' @return Called for side-effect of writing to a file.
#' @export
write_spec <- function(x, file, data_path, ...) {
    UseMethod("write_spec", x)
}

#' @rdname write_spec
#' @export
write_spec.default <- function(x, file, data_path, ...) {
    stop("Cannot extract survey design from x")
}

#' @rdname write_spec
#' @export
write_spec.inzsvyspec <- function(x, file, data_path, ...) {
    spec <- x$spec[!sapply(x$spec, is.null)]
    if (spec$ids == 1) spec$ids <- NULL

    spec <- paste0(names(spec), " = \"", as.character(spec), "\"", collapse = "\n")
    writeLines(spec, con = file)
}

#' @rdname write_spec
#' @param des a survey design object, needed for calibrated/poststratified surveys
#' @export
write_spec.survey.design <- function(x, file, data_path, des, ...) {
    write_spec(as_survey_spec(x, des), file, data_path)
}
