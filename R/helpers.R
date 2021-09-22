replaceVars <- function (exp, ...) {
    sub_list <- list(...)
    exp_str <- as.character(exp)
    for (i in seq_along(sub_list)) {
        exp_str <- gsub(names(sub_list)[i], sub_list[i], exp_str,
            fixed = TRUE)
    }
    exp <- as.formula(paste(exp_str, collapse = " "))
    exp
}

as_call <- function (x) {
    if (inherits(x, "formula")) {
        stopifnot(length(x) == 2)
        x[[2]]
    }
    else if (is.atomic(x) || is.name(x) || is.call(x)) {
        x
    }
    else {
        stop("Unknown input")
    }
}

interpolate <- function (code, ..., comment = character(), `_env` = parent.frame(2)) {
    if (length(list(...)) > 0) {
        args <- lapply(list(...), as_call)
        expr <- methods::substituteDirect(as_call(code), args)
    }
    else {
        expr <- as_call(code)
    }
    res <- eval(expr, `_env`)
    if (length(comment) > 0)
        comment <- paste("##", comment)
    attr(res, "code") <- c(comment, capture.output(expr))
    res
}
