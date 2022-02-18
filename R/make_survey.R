#' @param .data a data.frame
#' @param spec a `inzsvyspec` object
#' @describeIn import_survey Construct a survey object from a data set and an `inzsvyspec` object
#' @export
#' @md
make_survey <- function(.data, spec) {
    mc <- match.call()
    dataname <- mc$.data

    type <- spec$spec$survey_type
    exp <- switch(type,
        "replicate" = ~survey::svrepdesign(terms, data = .data),
        "survey" = ~survey::svydesign(terms, data = .data)
    )

    s <- spec$spec
    s$survey_type <- NULL
    fmla_args <- c("ids", "probs", "strata", "fpc", "weights")
    str_args <- c("type")

    if (type == "replicate") {
        s <- s[names(s) %in% c("weights", "repweights", "type", "scale", "rscales")]
        if (is.character(s$repweights) && length(s$repweights) > 1L)
            s$repweights <- paste(s$repweights, collapse = " + ")
        # is repweights a formula or string?
        split <- trimws(strsplit(s$repweights, "+", fixed = TRUE)[[1]])
        if (all(split %in% names(.data))) {
            # a formula
            fmla_args <- c(fmla_args, "repweights")
        } else {
            # string/something else ...
            str_args <- c(str_args, "repweights")
        }
        if (is.null(s$rscales)) {
            s$rscales <- NULL
        } else {
            if (all(diff(s$rscales) == 0)) s$rscales <- s$rscales[1]
            s$rscales <- paste(utils::capture.output(dput(s$rscales)), collapse = "")
        }
    }

    if (type == "survey") {
        s <- s[names(s) %in% c("ids", "probs", "strata", "fpc", "nest", "weights")]
    }

    terms <- do.call(
        paste,
        c(
            lapply(names(s)[!sapply(s, is.null)],
                function(x) {
                    sprintf("%s = %s%s%s",
                        x,
                        ifelse(x %in% fmla_args,
                            "~",
                            ifelse(x %in% str_args, "\"", "")
                        ),
                        s[[x]],
                        ifelse(x %in% str_args, "\"", "")
                    )
                }
            ),
            list(sep = ", ")
        )
    )
    exp <- replaceVars(exp, terms = terms)

    if (!is.null(spec$spec$calibrate)) {
        cal <- spec$spec$calibrate

        switch(spec$spec$calfun,
            "linear" = {
                # put cal into a more useful format
                vnames <- names(cal)
                pop.totals <- do.call(c,
                    lapply(seq_along(vnames),
                        function(i) {
                            x <- cal[[i]]
                            z <- paste0(vnames[[i]], names(x))
                            z[1] <- "(Intercept)"
                            x <- as.numeric(x)
                            x[1] <- sum(x)
                            names(x) <- z
                            if (i > 1L) x <- x[-1]
                            x
                        }
                    )
                )

                cal_exp <- ~survey::calibrate(.design, ~.vars, .totals)
                cal_exp <- replaceVars(cal_exp,
                    .vars = paste(vnames, collapse = " + ")
                )
                ifun <- function(e) {

                }
            },
            "raking" = {
                # make wo lists: one of formulas, one of data
                fmla <- lapply(names(cal),
                    function(x) sprintf("~%s", x))
                popn <- lapply(cal,
                    function(x) {
                        sprintf("as.table(rbind(%s))",
                            paste(
                                sprintf("c(%s)",
                                    sapply(x, paste, collapse = ", ")),
                                collapse = ", "
                            )
                        )
                    }
                )
                popn <- eval(parse(text = sprintf("list(%s)", paste(popn, collapse = ", "))))
                # figure out dimnames ...
                # --- this only works for categorical variables though
                dims <- lapply(names(cal),
                    function(x) {
                        v <- strsplit(x, split = "+", fixed = TRUE)[[1]]
                        vl <- lapply(v, function(z) levels(.data[[z]]))
                        names(vl) <- v
                        vl
                    }
                )

                for (i in seq_along(dims)) dimnames(popn[[i]]) <- dims[[i]]

                cal_exp <- ~survey::calibrate(.design,
                    formula = .FMLA,
                    population = .POPN,
                    calfun = "raking"
                )
                cal_exp <- replaceVars(cal_exp,
                    .FMLA = sprintf("list(%s)", paste(fmla, collapse = ", "))
                    # .POPN = popn
                )
            },
            stop(sprintf("calfun '%s' not supported", spec$spec$calfun))
        )

    }

    spec$data <- .data
    spec$design <- interpolate(exp, .data = dataname)

    if (!is.null(spec$spec$calibrate)) {
        # calibrate design:
        design_obj <- spec$design
        spec$design <- switch(spec$spec$calfun,
            "linear" = (function() {
                    interpolate(cal_exp,
                        .totals = pop.totals,
                        .design = ~design_obj
                    )
                })(),
            "raking" = {
                (function() {
                    interpolate(cal_exp,
                        .POPN = popn,
                        .design = ~design_obj
                    )
                })()
            }
        )
    }
    spec
}

as_call <- function (x) {
    if (inherits(x, "formula")) {
        stopifnot(length(x) == 2)
        x[[2]]
    }
    else if (is.list(x)) {
        x
    }
    else if (is.atomic(x) || is.name(x) || is.call(x)) {
        x
    }
    else {
        stop("Unknown input")
    }
}
