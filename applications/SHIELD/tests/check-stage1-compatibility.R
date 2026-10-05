#!/usr/bin/env Rscript

# Score the registered stage-1 likelihood on a few fixed parameter vectors.
# Uses the recorded offline bootstrap, but never creates or runs an MCMC cache.

shield.stage1.require.finite <- function(values, label) {
    if (!is.numeric(values) || !length(values) || any(!is.finite(values))) {
        bad <- if (is.numeric(values)) names(values)[!is.finite(values)] else NULL
        stop(label, " must contain finite numeric values",
             if (length(bad)) paste0(": ", paste(bad, collapse = ", ")) else "",
             call. = FALSE)
    }
    invisible(values)
}

shield.stage1.parameter.cases <- function(medians) {
    shield.stage1.require.finite(medians, "Prior medians")
    required <- c("global.transmission.rate.msm", "global.transmission.rate.het")
    if (is.null(names(medians)) || anyDuplicated(names(medians)) ||
        !all(required %in% names(medians))) {
        stop("Expected uniquely named SHIELD parameters including transmission rates",
             call. = FALSE)
    }
    lower <- higher <- medians
    lower[required] <- medians[required] * 0.9
    higher[required] <- medians[required] * 1.1
    list(prior_medians = medians, transmission_minus_10pct = lower,
         transmission_plus_10pct = higher)
}

shield.stage1.instructions <- function(info, location) {
    if (location %in% names(info$special.case.likelihood.instructions)) {
        info$special.case.likelihood.instructions[[location]]
    } else info$likelihood.instructions
}

shield.stage1.read.cases <- function(path, medians) {
    # Read only the trusted numeric fixture produced by this diagnostic. RDS
    # preserves the exact doubles; a JSON round-trip can round parameter values.
    cases <- readRDS(path)
    if (!is.list(cases) || !length(cases) || is.null(names(cases)) ||
        anyNA(names(cases)) || any(!nzchar(names(cases))) || anyDuplicated(names(cases))) {
        stop("Parameter cases must be a nonempty named object", call. = FALSE)
    }
    lapply(cases, function(values) {
        if (!is.numeric(values) || is.null(names(values)) || anyDuplicated(names(values)) ||
            !setequal(names(values), names(medians)) ||
            any(!is.finite(values))) {
            stop("Parameter case must contain every model parameter exactly once, finite and numeric",
                 call. = FALSE)
        }
        values[names(medians)]
    })
}

shield.stage1.trajectories <- function(simulation, years = 2010:2030) {
    dimensions <- c("year", "age", "race", "sex")
    result <- lapply(c("population", "incidence", "diagnosis.total", "diagnosis.ps"),
                     function(outcome) {
        values <- simulation$get(
            outcomes = outcome, keep.dimensions = dimensions,
            dimension.values = list(year = as.character(years)),
            drop.single.sim.dimension = TRUE, summary.type = "individual.simulation",
            replace.inf.values.with.zero = FALSE, na.rm = FALSE)
        shield.stage1.require.finite(values, paste("Trajectory", outcome))
        labels <- dimnames(values)
        if (is.null(labels) || !setequal(names(labels), dimensions) ||
            any(vapply(labels, function(x) is.null(x) || !length(x) || anyNA(x) ||
                       any(!nzchar(x)) || anyDuplicated(x) > 0L, logical(1)))) {
            stop("Missing or inconsistent trajectory dimensions: ", outcome, call. = FALSE)
        }
        values <- aperm(values, match(dimensions, names(labels)))
        if (!identical(dimnames(values)$year, as.character(years))) {
            stop("Missing trajectory years: ", outcome, call. = FALSE)
        }
        list(dimensions = lapply(dimnames(values), as.list),
             values = as.list(sprintf("%.17g", as.vector(values))))
    })
    names(result) <- c("population", "incidence", "diagnosis.total", "diagnosis.ps")
    result
}

shield.stage1.score <- function(likelihood, simulation) {
    # Match calibration's optimized scoring, while also checking the result via
    # the ordinary, consistency-checking path. Neither path changes the formula.
    pieces <- likelihood$compute.piecewise(
        simulation, log = TRUE, use.optimized.get = TRUE, check.consistency = FALSE)
    shield.stage1.require.finite(pieces, "Likelihood components")
    if (is.null(names(pieces)) || anyNA(names(pieces)) || any(!nzchar(names(pieces)))) {
        stop("Likelihood components must have names", call. = FALSE)
    }
    total <- likelihood$compute(
        simulation, log = TRUE, use.optimized.get = TRUE, check.consistency = FALSE)
    checked <- likelihood$compute(
        simulation, log = TRUE, use.optimized.get = FALSE, check.consistency = TRUE)
    for (value in list(total, checked)) {
        shield.stage1.require.finite(value, "Total log likelihood")
        if (length(value) != 1L) stop("Expected one total log likelihood", call. = FALSE)
    }
    if (!isTRUE(all.equal(unname(total), unname(sum(pieces)), tolerance = 1e-10)) ||
        !isTRUE(all.equal(unname(total), unname(checked), tolerance = 1e-10))) {
        stop("Likelihood total, component sum, and checked evaluation disagree",
             call. = FALSE)
    }
    list(total = unname(total), checked_total = unname(checked),
         total_exact = sprintf("%.17g", total),
         components = lapply(seq_along(pieces), function(i) {
             list(index = i, name = names(pieces)[[i]], value = unname(pieces[[i]]),
                  value_exact = sprintf("%.17g", pieces[[i]]))
         }))
}

shield.stage1.main <- function(args = commandArgs(trailingOnly = TRUE)) {
    if (length(args) < 1L || length(args) > 3L) {
        stop("Usage: Rscript --vanilla check-stage1-compatibility.R REPORT_DIRECTORY [LOCATION] [CALIBRATION_CODE]",
             call. = FALSE)
    }
    report.dir <- path.expand(args[[1L]])
    if (file.exists(report.dir)) stop("Report directory already exists: ", report.dir)
    if (!dir.exists(dirname(report.dir))) stop("Report parent directory must exist")
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required")
    if (!dir.create(report.dir)) stop("Could not create report directory")
    report.dir <- normalizePath(report.dir, mustWork = TRUE)
    location <- if (length(args) >= 2L) args[[2L]] else "C.12580"
    code <- if (length(args) >= 3L) args[[3L]] else "calib.10.1.stage1"
    report <- list(schema_version = 1L, status = "running", location = location,
                   calibration_code = code, stage = "preflight", samples = list(),
                   scope = "Likelihood compatibility only; no MCMC or predecessor handoff",
                   started_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
    set.stage <- function(stage) {
        report$stage <<- stage
        cat("Stage-1 compatibility: ", stage, "\n", sep = "")
        flush.console()
    }
    git <- function(path, ...) {
        value <- system2("git", c("-C", shQuote(path), ...), stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(value, "status"))) stop("Could not inspect source checkout")
        value
    }
    failure <- tryCatch({
        if (!identical(tolower(Sys.getenv("SHIELD_RECORDED_RUN")), "true")) {
            stop("This check requires SHIELD_RECORDED_RUN=true (offline, no repository updates)")
        }
        analyses <- normalizePath(Sys.getenv("JHEEM_ANALYSES_PATH"), mustWork = TRUE)
        source(file.path(analyses, "applications/SHIELD/R/shield_recorded_runtime.R"),
               local = globalenv())
        config <- shield.recorded.config()
        shield.recorded.assert.names(location, code)
        if (!identical(config$run_mode, "fresh")) stop("Use SHIELD_RUN_MODE=fresh")
        if (length(list.files(config$root_dir, all.files = TRUE, no.. = TRUE))) {
            stop("JHEEM_ROOT_DIR must be an empty, isolated directory")
        }
        if (!identical(git(analyses, "rev-parse", "HEAD")[[1L]], config$analyses_ref) ||
            !identical(git(config$jheem2_path, "rev-parse", "HEAD")[[1L]], config$jheem2_ref)) {
            stop("Declared source revisions do not match the checkouts")
        }
        report$selection <- config
        report$analyses_worktree_status <- git(analyses, "status", "--porcelain")
        report$engine_worktree_status <- git(config$jheem2_path, "status", "--porcelain")
        if (length(report$engine_worktree_status)) stop("Engine checkout must be clean")
        report$loading_mode <- if (config$jheem2_mode == "source") {
            "recorded source mode (pkgload::load_all); not the ordinary source script"
        } else "installed package"
        source.files <- git(analyses, "ls-files", "--", "applications/SHIELD", "commoncode")
        report$source_files <- lapply(source.files, function(path) {
            list(path = path, sha256 = shield.recorded.sha256(file.path(analyses, path)))
        })
        report$check_script_sha256 <- shield.recorded.sha256(file.path(
            analyses, "applications/SHIELD/tests/check-stage1-compatibility.R"))
        report$support_packages <- lapply(c("locations", "bayesian.simulations", "distributions"),
                                          function(package) {
            description <- utils::packageDescription(package)
            list(package = package, version = as.character(utils::packageVersion(package)),
                 path = find.package(package), installed_remote_sha = description$RemoteSha)
        })
        old.wd <- setwd(analyses)
        on.exit(setwd(old.wd), add = TRUE)
        # Do not use the calibration launcher: it clears/creates cache state and
        # starts MCMC. Source the actual specification and registry directly.
        assign("JHEEM.ANALYSES.PATH", analyses, envir = globalenv())
        assign("SHIELD.DIR", file.path(analyses, "applications/SHIELD"), envir = globalenv())
        set.stage("load specification and exact offline managers")
        source(file.path(SHIELD.DIR, "shield_specification.R"), local = globalenv())
        if (identical(get0("SHIELD.COMPARISON.ENGINE.LOADING", envir = globalenv()),
                      "native-source")) {
            report$loading_mode <- "hand-sourced engine with diagnostic offline bootstrap"
        }
        report$managers <- list(census = get.data.manager.resolution(CENSUS.MANAGER),
                               syphilis = get.data.manager.resolution(SURVEILLANCE.MANAGER))
        for (manager in report$managers) {
            if (is.null(manager$sha256) || is.null(manager$resolved_tag)) {
                stop("Expected verified manager identities")
            }
        }
        set.stage("load actual calibration registry")
        source(file.path(SHIELD.DIR, "shield_calib_register.R"), local = globalenv())
        info <- shield.recorded.jheem2.function("get.calibration.info")(code)
        report$preceding_calibration_codes <- info$preceding.calibration.codes
        report$end_year <- info$end.year
        instructions <- shield.stage1.instructions(info, location)
        if (!identical(instructions, lik.inst.stage1)) {
            stop("Selected calibration does not use the current stage-1 likelihood")
        }
        set.seed(config$random_seed)
        report$rng_kind <- RNGkind()
        set.stage("instantiate registered stage-1 likelihood")
        likelihood <- instructions$instantiate.likelihood(
            version = "shield", location = location, data.manager = info$data.manager)
        report$likelihood_names <- names(likelihood$sub.likelihoods)
        if (!any(grepl("prop.male.ps.diag.among.msm", report$likelihood_names, fixed = TRUE))) {
            stop("Stage-1 likelihood does not include the expected MSM diagnosis term")
        }
        set.stage("build engine")
        engine <- create.jheem.engine("shield", location, end.year = info$end.year,
                                      max.run.time.seconds = 60)
        medians <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
        parameter.file <- Sys.getenv("SHIELD_COMPARISON_PARAMETERS")
        cases <- if (nzchar(parameter.file)) shield.stage1.read.cases(parameter.file, medians)
                 else shield.stage1.parameter.cases(medians)
        save.parameters <- Sys.getenv("SHIELD_SAVE_PARAMETERS")
        if (nzchar(save.parameters)) {
            if (nzchar(parameter.file) || file.exists(save.parameters) ||
                !dir.exists(dirname(save.parameters))) {
                stop("Choose a new parameter fixture path, without an input fixture")
            }
            saveRDS(cases, save.parameters, version = 3)
            parameter.file <- save.parameters
        }
        report$parameter_source <- if (nzchar(parameter.file)) {
            list(path = normalizePath(parameter.file, mustWork = TRUE),
                 sha256 = shield.recorded.sha256(parameter.file))
        } else "prior medians and diagnostic transmission variations"
        trajectories <- Sys.getenv("SHIELD_COMPARE_TRAJECTORIES", unset = "false")
        if (!trajectories %in% c("true", "false")) stop("SHIELD_COMPARE_TRAJECTORIES must be true or false")
        for (name in names(cases)) {
            set.stage(paste("simulate and score", name))
            started <- proc.time()[["elapsed"]]
            simulation <- engine$run(cases[[name]])
            result <- shield.stage1.score(likelihood, simulation)
            if (trajectories == "true") result$trajectories <- shield.stage1.trajectories(simulation)
            report$samples[[name]] <- c(list(parameters = as.list(cases[[name]])), result,
                                       list(elapsed_seconds = proc.time()[["elapsed"]] - started))
        }
        report$status <- "passed"
        NULL
    }, error = function(e) {
        report$status <<- "failed"
        report$error <<- conditionMessage(e)
        e
    })
    report$finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    report$session_info <- capture.output(sessionInfo())
    jsonlite::write_json(report, file.path(report.dir, "report.json"),
                        auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
    summary <- c(paste("SHIELD stage-1 compatibility:", toupper(report$status)),
                 paste("Location:", location), paste("Calibration:", code),
                 "No MCMC run, stage-0 output reuse, or scientific formula change.",
                 paste("Last check:", report$stage))
    if (!is.null(report$managers)) {
        summary <- c(summary, vapply(report$managers, function(x) {
            paste(x$manager, x$resolved_tag, x$sha256)
        }, character(1)))
    }
    for (name in names(report$samples)) {
        result <- report$samples[[name]]
        summary <- c(summary, sprintf("%s: %d finite components; total log likelihood %.10g",
                                       name, length(result$components), result$total))
    }
    if (!is.null(report$error)) summary <- c(summary, paste("Error:", report$error))
    writeLines(summary, file.path(report.dir, "summary.txt"))
    cat(paste(summary, collapse = "\n"), "\nReport: ", report.dir, "\n", sep = "")
    if (!is.null(failure)) stop(conditionMessage(failure), call. = FALSE)
    invisible(report)
}

if (sys.nframe() == 0L) shield.stage1.main()
