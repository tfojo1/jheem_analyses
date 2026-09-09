# ============================================================================
# compare_calc_results() -- are two results arrays the same?
# ============================================================================
#
# Why not just identical(a, b)?
#
#   1. identical() compares ATTRIBUTES too. The current
#      calculate_custom_outcomes() attaches attr(x, "baseline.year"), so an
#      array built before that returns FALSE even when every number matches.
#   2. identical() is bit equality. Two runs that differ in the last bit of a
#      floating point number -- a different R version, a different BLAS, a
#      cumsum() accumulated in a different order -- are "not identical" but
#      are the same answer.
#   3. If the two arrays have different outcomes or a different dimension
#      ORDER, identical() just says FALSE without telling you which.
#
# This reports all of that, then compares the numbers on the outcomes the two
# arrays share.
#
# Usage:
#   source("compare_results.R")
#   compare_calc_results(total_calc_results, total_calc_results1)
#
# Returns (invisibly) a data frame with one row per shared outcome.
# ============================================================================

compare_calc_results <- function(a, b,
                                 name.a = deparse(substitute(a)),
                                 name.b = deparse(substitute(b)),
                                 tol    = 1e-8,     # relative tolerance
                                 show.n = 8) {
    
    hdr <- function(x) cat("\n", x, "\n", strrep("-", nchar(x)), "\n", sep = "")
    
    # ---- 1. dimensions -----------------------------------------------------
    hdr("1. DIMENSIONS")
    da <- names(dimnames(a)); db <- names(dimnames(b))
    cat(sprintf("  %-28s %s\n", name.a,
                paste0(paste(da, collapse = " x "), "  =  ",
                       paste(dim(a), collapse = " x "))))
    cat(sprintf("  %-28s %s\n", name.b,
                paste0(paste(db, collapse = " x "), "  =  ",
                       paste(dim(b), collapse = " x "))))
    
    if (!setequal(da, db)) {
        cat("\n  *** different dimension NAMES -- these are not comparable.\n")
        cat("      only in ", name.a, ": ", paste(setdiff(da, db), collapse = ", "), "\n", sep = "")
        cat("      only in ", name.b, ": ", paste(setdiff(db, da), collapse = ", "), "\n", sep = "")
        return(invisible(NULL))
    }
    if (!identical(da, db)) {
        cat("\n  NOTE: same dimensions, different ORDER. Reordering ", name.b,
            " to match ", name.a, ".\n", sep = "")
        b <- aperm(b, da)
    } else cat("\n  same names, same order.\n")
    
    # ---- 2. levels within each dimension -----------------------------------
    hdr("2. LEVELS")
    aligned <- TRUE
    for (d in da) {
        la <- dimnames(a)[[d]]; lb <- dimnames(b)[[d]]
        only.a <- setdiff(la, lb); only.b <- setdiff(lb, la)
        if (length(only.a) == 0 && length(only.b) == 0) {
            cat(sprintf("  %-14s %3d levels, identical%s\n", d, length(la),
                        if (identical(la, lb)) "" else "  (different ORDER -- realigned)"))
        } else {
            aligned <- FALSE
            cat(sprintf("  %-14s only in %s: %s\n", d, name.a,
                        paste(head(only.a, show.n), collapse = ", ")))
            cat(sprintf("  %-14s only in %s: %s\n", "", name.b,
                        paste(head(only.b, show.n), collapse = ", ")))
        }
    }
    if (!aligned)
        cat("\n  -> comparing on the INTERSECTION only; the rows above are excluded.\n")
    
    # keep a's ordering, restricted to what both arrays have
    common <- lapply(da, function(d) intersect(dimnames(a)[[d]], dimnames(b)[[d]]))
    names(common) <- da
    if (any(lengths(common) == 0)) {
        cat("\n  *** no overlap in at least one dimension -- nothing to compare.\n")
        return(invisible(NULL))
    }
    A <- do.call(`[`, c(list(a), common, list(drop = FALSE)))
    B <- do.call(`[`, c(list(b), common, list(drop = FALSE)))
    
    # ---- 3. attributes ------------------------------------------------------
    hdr("3. ATTRIBUTES (beyond dim / dimnames)")
    extra <- function(x) attributes(x)[setdiff(names(attributes(x)), c("dim", "dimnames"))]
    ea <- extra(a); eb <- extra(b)
    if (length(ea) == 0 && length(eb) == 0) cat("  none on either.\n")
    else for (nm in union(names(ea), names(eb)))
        cat(sprintf("  %-16s %s: %-22s %s: %s\n", nm,
                    name.a, if (is.null(ea[[nm]])) "<absent>" else paste(ea[[nm]], collapse = ","),
                    name.b, if (is.null(eb[[nm]])) "<absent>" else paste(eb[[nm]], collapse = ",")))
    
    # ---- 4. the numbers, outcome by outcome --------------------------------
    hdr("4. VALUES, BY OUTCOME")
    o.pos <- match("outcome", da)
    slice <- function(X, o) {
        idx <- rep(list(TRUE), length(dim(X))); idx[[o.pos]] <- o
        do.call(`[`, c(list(X), idx, list(drop = TRUE)))
    }
    
    res <- do.call(rbind, lapply(common$outcome, function(o) {
        x <- slice(A, o); y <- slice(B, o)
        na.match  <- identical(is.na(x),  is.na(y))        # missing in the same cells
        nan.match <- identical(is.nan(x), is.nan(y))       # ...and missing the same WAY
        d   <- abs(x - y)
        rel <- d / pmax(abs(x), .Machine$double.eps)
        finite <- is.finite(d)
        max.abs <- if (any(finite)) max(d[finite])   else 0
        max.rel <- if (any(finite)) max(rel[finite]) else 0
        worst <- if (any(finite) && max.abs > 0) {
            w  <- which(finite)[which.max(d[finite])]
            ai <- arrayInd(w, dim(x))
            paste(mapply(function(k, i) dimnames(x)[[k]][i],
                         seq_along(ai), as.vector(ai)), collapse = " / ")
        } else ""
        data.frame(outcome = o,
                   identical = identical(x, y),
                   na.cells.match = na.match,
                   nan.cells.match = nan.match,
                   max.abs.diff = max.abs,
                   max.rel.diff = max.rel,
                   worst.cell = worst,
                   stringsAsFactors = FALSE)
    }))
    
    res$verdict <- ifelse(res$identical, "identical",
                          ifelse(!res$na.cells.match,  "MISSING-CELL PATTERN DIFFERS",
                                 ifelse(res$max.rel.diff > tol, "DIFFERENT",
                                        ifelse(!res$nan.cells.match, "equal, but NaN vs NA",
                                               "equal within tol"))))
    
    bad <- res$verdict %in% c("DIFFERENT", "MISSING-CELL PATTERN DIFFERS")
    print(format(res[, c("outcome", "verdict", "max.abs.diff", "max.rel.diff")],
                 digits = 3), row.names = FALSE)
    
    if (any(bad)) {
        hdr("WORST CELLS FOR THE OUTCOMES THAT DIFFER")
        print(res[bad, c("outcome", "max.abs.diff", "max.rel.diff", "worst.cell")],
              row.names = FALSE)
    }
    
    # ---- 5. verdict ---------------------------------------------------------
    hdr("VERDICT")
    n.id  <- sum(res$verdict == "identical")
    n.tol <- sum(res$verdict == "equal within tol")
    n.nan <- sum(res$verdict == "equal, but NaN vs NA")
    cat(sprintf("  %d outcomes compared: %d bit-identical, %d equal within %g, %d DIFFERENT\n",
                nrow(res), n.id, n.tol, tol, sum(bad)))
    if (n.nan > 0)
        cat(sprintf("  %d equal numerically but with NaN in one array where the other has NA\n", n.nan))
    if (!aligned)
        cat("  (plus levels present in only one array -- see section 2)\n")
    if (sum(bad) == 0 && aligned && n.tol == 0 && n.nan == 0)
        cat("  -> the two arrays hold the same numbers.\n")
    
    invisible(res)
}

if (1==2){
    # compare old and new results to make sure they match
    compare_calc_results(total_calc_results,total_calc_results1)
    compare_calc_results(sex_calc_results,sex_calc_results1)
}