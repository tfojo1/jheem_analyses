## ============================================================================
## WHAT THIS FILE COVERS
##   R/shield_ontology_mappings.R - the ~25 registered translations between the
##   category systems of external data sources (BRFSS, CDC, census, WONDER,
##   Emory) and the model's own age / race / sex strata.
##
## WHY IT MATTERS
##   Every number that enters the model crosses one of these mappings. They are
##   pure bookkeeping, which is exactly why they are dangerous: a mapping that
##   sends two source categories to the same target double-counts, and one that
##   sends a source category nowhere silently discards it. In both cases the
##   model runs, the plots look normal, and the calibration fits a target that
##   is quietly wrong.
##
##   There is no way to notice this from a simulation. It has to be checked at
##   the mapping itself.
## ============================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

## registered_mappings ----
## Parse the mapping names and their from/to category lists straight out of the
## source, so the test describes the file as written rather than a copy of it.
registered_mappings <- function() {
    f <- file.path(SHIELD.DIR, "R/shield_ontology_mappings.R")
    lines <- readLines(f, warn = FALSE)
    code <- lines[!grepl("^\\s*#", lines)]
    m <- regmatches(code, regexpr("register\\.ontology\\.mapping\\(\\s*['\"][^'\"]+['\"]", code))
    unique(gsub("^register\\.ontology\\.mapping\\(\\s*['\"]|['\"]$", "", m))
}

test_that("the ontology mapping file defines mappings", {
    names <- registered_mappings()
    expect_gt(length(names), 10)
    expect_equal(anyDuplicated(names), 0,
                 info = paste("a mapping name is registered twice:",
                              paste(unique(names[duplicated(names)]), collapse = ", ")))
})

test_that("every registered mapping is retrievable by name", {
    ## register.ontology.mapping() works by side effect. If registration ever
    ## stopped happening - a renamed function, a changed signature - sourcing
    ## the file would still succeed and every lookup would fall back to whatever
    ## default jheem2 supplies.
    skip_if(!exists("get.ontology.mapping.by.name", mode = "function"),
            "jheem2 does not expose get.ontology.mapping.by.name()")

    names <- registered_mappings()
    missing <- character(0)
    for (nm in names) {
        got <- tryCatch(get.ontology.mapping.by.name(nm), error = function(e) NULL)
        if (is.null(got)) missing <- c(missing, nm)
    }

    expect_equal(missing, character(0),
                 info = paste("mappings named in the file but not registered:",
                              paste(missing, collapse = ", ")))
})

## shield_categories ----
## The model's own categories, which every "to shield" mapping must land inside.
shield_categories <- function() {
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)
    list(race = sm$dim.names$race, sex = sm$dim.names$sex, age = sm$dim.names$age)
}

test_that("mappings into the model land only on categories the model has", {
    ## A mapping whose target is "white" or "male" - categories the SHIELD
    ## specification does not have - produces an array the model cannot index,
    ## or worse, one that silently drops on join.
    skip_if(is.null(shield.test.specification()), "no specification")
    skip_if(!exists("get.ontology.mapping.by.name", mode = "function"),
            "jheem2 does not expose get.ontology.mapping.by.name()")

    cats <- shield_categories()
    names <- grep("to\\.shield|to\\.SHIELD", registered_mappings(), value = TRUE)
    skip_if(length(names) == 0)

    problems <- character(0)
    for (nm in names) {
        mapping <- tryCatch(get.ontology.mapping.by.name(nm), error = function(e) NULL)
        if (is.null(mapping)) next

        to.dims <- tryCatch(mapping$to.dimensions, error = function(e) NULL)
        if (is.null(to.dims)) next

        for (d in intersect(to.dims, names(cats))) {
            targets <- tryCatch(unique(mapping$mappings[, ncol(mapping$mappings)]),
                                error = function(e) NULL)
            if (is.null(targets)) next
            stray <- setdiff(targets, cats[[d]])
            ## only complain when the mapping is unambiguously for this dimension
            if (length(to.dims) == 1 && length(stray)) {
                problems <- c(problems, paste0(
                    nm, " (", d, ") maps to categories the model does not have: ",
                    paste(stray, collapse = ", ")))
            }
        }
    }

    expect_equal(problems, character(0),
                 info = paste0("ontology mappings landing outside the model's ",
                               "own categories:\n  ",
                               paste(problems, collapse = "\n  ")))
})

test_that("race mappings cover every model race", {
    ## The complement of the check above: not "does it land somewhere valid" but
    ## "does every model category get fed". A model race that no mapping ever
    ## produces is a stratum that receives no data at all.
    skip_if(is.null(shield.test.specification()), "no specification")
    skip_if(!exists("get.ontology.mapping.by.name", mode = "function"),
            "jheem2 does not expose get.ontology.mapping.by.name()")

    cats <- shield_categories()
    names <- grep("to\\.shield\\.race|to\\.SHIELD\\.race", registered_mappings(),
                  value = TRUE)
    skip_if(length(names) == 0)

    covered <- character(0)
    for (nm in names) {
        mapping <- tryCatch(get.ontology.mapping.by.name(nm), error = function(e) NULL)
        if (is.null(mapping)) next
        targets <- tryCatch(unique(mapping$mappings[, ncol(mapping$mappings)]),
                            error = function(e) character(0))
        covered <- union(covered, targets)
    }
    skip_if(length(covered) == 0, "could not read any mapping targets")

    expect_equal(setdiff(cats$race, covered), character(0),
                 info = paste("model races that no 'to shield' mapping produces:",
                              paste(setdiff(cats$race, covered), collapse = ", ")))
})

test_that("no mapping sends one source category to two different targets", {
    ## A one-to-many row makes the mapping ambiguous: the same source count can
    ## be routed two ways, and which one wins depends on ordering.
    skip_if(!exists("get.ontology.mapping.by.name", mode = "function"),
            "jheem2 does not expose get.ontology.mapping.by.name()")

    problems <- character(0)
    for (nm in registered_mappings()) {
        mapping <- tryCatch(get.ontology.mapping.by.name(nm), error = function(e) NULL)
        if (is.null(mapping)) next
        tbl <- tryCatch(mapping$mappings, error = function(e) NULL)
        if (is.null(tbl) || !is.matrix(tbl) || ncol(tbl) < 2) next

        from <- apply(tbl[, seq_len(ncol(tbl) - 1), drop = FALSE], 1, paste, collapse = "|")
        to <- tbl[, ncol(tbl)]
        split.targets <- tapply(to, from, function(x) length(unique(x)))
        ambiguous <- names(split.targets)[split.targets > 1]

        if (length(ambiguous)) {
            problems <- c(problems, paste0(
                nm, ": ", paste(ambiguous, collapse = ", "),
                " map to more than one target"))
        }
    }

    expect_equal(problems, character(0),
                 info = paste0("ambiguous ontology mappings:\n  ",
                               paste(problems, collapse = "\n  ")))
})

test_that("the specification's own ontology is self-consistent", {
    ## The model's categories must not contain duplicates or blanks - both of
    ## which would make every mapping into them ill-defined.
    skip_if(is.null(shield.test.specification()), "no specification")
    cats <- shield_categories()

    for (d in names(cats)) {
        expect_equal(anyDuplicated(cats[[d]]), 0,
                     info = paste("duplicate category in the model's", d))
        expect_true(all(nzchar(cats[[d]])),
                    info = paste("empty category name in the model's", d))
    }
})
