# ****************************************************************************************************
# SURVEILLANCE DISCONTINUITY CHECK ----
# ****************************************************************************************************
# Screens the syphilis surveillance manager for suspicious discontinuities (case-definition changes,
# reporting artifacts, data-entry errors; e.g., NYC late/unknown-duration diagnoses jumping from a
# few hundred to a few thousand around 2000) and writes a CSV of series-years for manual review.
#
# RULES
#   (1) Year-over-year fold jump : |log(y_t / y_prev)| >= log(YOY.FOLD.THRESHOLD)
#   (2) Robust level shift       : median level in [t-W, t-1] vs [t, t+W-1] differs >= SHIFT.FOLD.THRESHOLD
#                                  AND the shift is >= SHIFT.Z.THRESHOLD robust SDs (MAD) of within-segment noise
#   Counts/rates are compared on the log scale; proportions on the logit scale (fold = odds ratio).
#   Small-number floors (MIN.COUNT, MIN.PROP.DIFF) suppress noise in sparse series.
#
# SCOPE
#   metric = "estimate"; every outcome x source x ontology; stratifications year__location (totals)
#   and year__location__<one dim> (one-way strata); locations matching LOCATION.PATTERN (MSAs).
#
# MEMORY
#   Reads arrays straight from manager$data -- never calls $pull(), so no ontology mapping or
#   aggregation is triggered. Arrays are subset to the target locations BEFORE reshaping, so
#   county-level arrays never get expanded. If SURVEILLANCE.MANAGER is already in the session it is
#   reused; otherwise MANAGER.FILE is loaded once and freed after extraction.
#
# READ-ONLY: does not modify the manager or any model file. Only writes OUTPUT.FILE.
#
# Run from the jheem_analyses root (same convention as analysis/shield_plot_core.R).
# Suggested first run: OUTCOMES.TO.CHECK = "unknown.duration.or.late.syphilis.diagnoses",
#                      LOCATIONS.TO.CHECK = "C.35620"  -> should flag NYC ~2000.
# ****************************************************************************************************

library(dplyr)
library(purrr)
library(tibble)
library(readr)

## ---- 1. Settings ------------------------------------------------------------------------------------

SHIELD.DIR         <- "../jheem_analyses/applications/SHIELD"
MANAGER.FILE       <- NULL     # only used if SURVEILLANCE.MANAGER is not loaded; e.g. "~/Downloads/syphilis_manager.rdata"
OUTPUT.FILE        <- file.path(SHIELD.DIR, "analysis", "data_qc",
                                paste0("surveillance_flags_", format(Sys.Date(), "%Y%m%d"), ".csv"))

METRIC             <- "estimate"
OUTCOMES.TO.CHECK  <- NULL     # NULL = all outcomes
LOCATION.PATTERN   <- "^C\\."  # MSAs; "^(C\\.|US$)" to also screen national
LOCATIONS.TO.CHECK <- NULL     # NULL = all locations matching LOCATION.PATTERN
MAX.EXTRA.DIMS     <- 1        # 0 = totals only; 1 = totals + one-way strata

# Rule 1: year-over-year fold jump
YOY.FOLD.THRESHOLD   <- 3      # log(3) = 1.099
# Rule 2: robust level shift
SHIFT.WINDOW         <- 5      # years on each side of a candidate break
SHIFT.MIN.POINTS     <- 3      # minimum observed years required on each side
SHIFT.FOLD.THRESHOLD <- 2      # log(2) = 0.693
SHIFT.Z.THRESHOLD    <- 4      # shift / robust noise SD
SHIFT.NOISE.FLOOR    <- 0.05   # lower bound on noise SD (log/logit units) so flat series don't give z = Inf

# Small-number floors
MIN.COUNT     <- 20            # count series: larger of the two compared levels must be >= this
MIN.PROP.DIFF <- 0.05          # proportion series: absolute difference must be >= this
LOG.OFFSET    <- 0.5           # added before log() so zeros are finite
PROP.CLAMP    <- 1e-4          # proportions clamped to [eps, 1 - eps] before logit

SERIES.KEYS <- c("outcome", "source", "ontology", "stratification", "stratum", "location")

## ---- 2. Get the manager -----------------------------------------------------------------------------

if (exists("SURVEILLANCE.MANAGER", envir = .GlobalEnv)) {
    cat("Using SURVEILLANCE.MANAGER already in session\n")
    manager <- get("SURVEILLANCE.MANAGER", envir = .GlobalEnv)
    manager.loaded.here <- FALSE
} else {
    if (is.null(MANAGER.FILE)) stop("SURVEILLANCE.MANAGER not loaded and MANAGER.FILE not set")
    cat("Loading", MANAGER.FILE, "(freed after extraction)\n")
    load.env <- new.env()
    obj.names <- load(MANAGER.FILE, envir = load.env)
    if (length(obj.names) != 1) stop("Expected one object in MANAGER.FILE, found: ", paste(obj.names, collapse = ", "))
    manager <- load.env[[obj.names]]
    rm(load.env)
    manager.loaded.here <- TRUE
}
if (!is.list(manager$data)) stop("manager$data not found; load the manager via shield_source_code.R and rerun")

## ---- 3. Helpers -------------------------------------------------------------------------------------

get.outcome.scale <- function(manager, outcome) {
    scale <- tryCatch(manager$outcome.info[[outcome]]$metadata$scale, error = function(e) NULL)
    if (is.null(scale) || length(scale) != 1) {
        scale <- if (grepl("^prop|proportion", outcome)) "proportion" else "non.negative.number"
    }
    scale
}

# One manager array -> long tibble (target locations only)
extract.one.array <- function(arr, outcome, source, ontology, scale) {
    dn <- dimnames(arr)
    dims <- names(dn)
    if (is.null(dims) || !all(c("year", "location") %in% dims)) return(NULL)
    extra.dims <- setdiff(dims, c("year", "location"))
    if (length(extra.dims) > MAX.EXTRA.DIMS) return(NULL)

    keep.loc <- grepl(LOCATION.PATTERN, dn$location)
    if (!is.null(LOCATIONS.TO.CHECK)) keep.loc <- keep.loc & dn$location %in% LOCATIONS.TO.CHECK
    if (!any(keep.loc)) return(NULL)

    # subset locations before reshaping (keeps county arrays from expanding)
    idx <- rep(list(TRUE), length(dims))
    idx[[which(dims == "location")]] <- which(keep.loc)
    arr <- do.call(`[`, c(list(arr), idx, list(drop = FALSE)))

    df <- as_tibble(as.data.frame.table(arr, responseName = "value", stringsAsFactors = FALSE)) %>%
        filter(!is.na(value))
    if (nrow(df) == 0) return(NULL)

    if (length(extra.dims) == 0) {
        df <- df %>% mutate(stratification = "total", stratum = "total")
    } else {
        df <- df %>% mutate(stratification = extra.dims, stratum = .data[[extra.dims]])
    }

    df %>%
        mutate(outcome = outcome, source = source, ontology = ontology, scale = scale,
               year = suppressWarnings(as.numeric(year))) %>%
        select(all_of(SERIES.KEYS), scale, year, value)
}

transform.values <- function(value, scale) {
    if_else(scale == "proportion",
            qlogis(pmin(pmax(value, PROP.CLAMP), 1 - PROP.CLAMP)),
            log(pmax(value, 0) + LOG.OFFSET))
}

passes.floor <- function(level.a, level.b, scale) {
    case_when(scale == "proportion"          ~ abs(level.a - level.b) >= MIN.PROP.DIFF,
              scale == "non.negative.number" ~ pmax(level.a, level.b) >= MIN.COUNT,
              TRUE                           ~ TRUE)   # rates/ratios: no floor
}

# Rule 2 on one series (years sorted)
detect.level.shift <- function(year, z, value, scale) {
    out <- vector("list", length(year))
    for (i in seq_along(year)) {
        t <- year[i]
        before <- year >= t - SHIFT.WINDOW & year <= t - 1
        after  <- year >= t & year <= t + SHIFT.WINDOW - 1
        if (sum(before) < SHIFT.MIN.POINTS || sum(after) < SHIFT.MIN.POINTS) next

        med.b <- median(z[before]); med.a <- median(z[after])
        resid <- c(z[before] - med.b, z[after] - med.a)
        noise <- max(mad(resid), SHIFT.NOISE.FLOOR)
        shift <- med.a - med.b

        out[[i]] <- tibble(year = t, value_before = median(value[before]), value_after = median(value[after]),
                           log_shift = shift, robust_z = shift / noise)
    }
    res <- bind_rows(out)
    if (nrow(res) == 0) return(res)

    res %>%
        filter(abs(log_shift) >= log(SHIFT.FOLD.THRESHOLD),
               abs(robust_z)  >= SHIFT.Z.THRESHOLD,
               passes.floor(value_before, value_after, scale[1])) %>%
        # one step flags several adjacent break years; keep the strongest per contiguous run
        arrange(year) %>%
        mutate(run = cumsum(year - lag(year, default = -Inf) > 1)) %>%
        group_by(run) %>%
        slice_max(abs(robust_z), n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(-run)
}

## ---- 4. Extract target series -----------------------------------------------------------------------

outcomes <- if (is.null(OUTCOMES.TO.CHECK)) names(manager$data) else intersect(OUTCOMES.TO.CHECK, names(manager$data))
cat("Extracting", length(outcomes), "outcomes\n")

pieces <- list()
for (outcome in outcomes) {
    metric.data <- manager$data[[outcome]][[METRIC]]
    if (is.null(metric.data)) next
    scale <- get.outcome.scale(manager, outcome)
    for (source in names(metric.data)) {
        for (ontology in names(metric.data[[source]])) {
            for (strat.key in names(metric.data[[source]][[ontology]])) {
                pieces[[length(pieces) + 1]] <- extract.one.array(metric.data[[source]][[ontology]][[strat.key]],
                                                                  outcome, source, ontology, scale)
            }
        }
    }
}
series.long <- bind_rows(pieces)
rm(pieces)

n.bad.year <- sum(is.na(series.long$year))
if (n.bad.year > 0) cat("Dropping", n.bad.year, "rows with non-numeric year labels\n")
series.long <- series.long %>% filter(!is.na(year))

if (manager.loaded.here) { rm(manager); invisible(gc()) }

cat("Series:", nrow(distinct(series.long, across(all_of(SERIES.KEYS)))), "| rows:", nrow(series.long), "\n")

## ---- 5. Rule 1: year-over-year fold jump ------------------------------------------------------------

flags.yoy <- series.long %>%
    group_by(across(all_of(SERIES.KEYS))) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(n_years      = n(),
           z            = transform.values(value, scale),
           dz           = z - lag(z),
           prev_dz      = lag(dz),
           next_dz      = lead(dz),
           prev_year    = lag(year),
           value_before = lag(value)) %>%
    ungroup() %>%
    filter(!is.na(dz),
           abs(dz) >= log(YOY.FOLD.THRESHOLD),
           passes.floor(value_before, value, scale)) %>%
    mutate(rule        = "yoy_fold_jump",
           value_after = value,
           gap_years   = year - prev_year,
           log_shift   = dz,
           robust_z    = NA_real_,
           pattern     = case_when(!is.na(next_dz) & sign(next_dz) == -sign(dz) & abs(next_dz) >= 0.5 * abs(dz) ~ "possible transient outlier",
                                   !is.na(prev_dz) & sign(prev_dz) == -sign(dz) & abs(prev_dz) >= 0.5 * abs(dz) ~ "return from outlier",
                                   TRUE ~ "possible step change"))

## ---- 6. Rule 2: robust level shift ------------------------------------------------------------------

flags.shift <- series.long %>%
    group_by(across(all_of(SERIES.KEYS)), scale) %>%
    arrange(year, .by_group = TRUE) %>%
    # note: grouping columns (incl. scale) are in .y, not .x
    group_modify(~ detect.level.shift(.x$year, transform.values(.x$value, .y$scale), .x$value, .y$scale) %>%
                     mutate(n_years = nrow(.x))) %>%
    ungroup() %>%
    mutate(rule = "robust_level_shift", prev_year = NA_real_, gap_years = NA_real_, pattern = "sustained level shift")

## ---- 7. Combine, annotate, write --------------------------------------------------------------------

out.cols <- c("rule", SERIES.KEYS, "scale", "year", "prev_year", "gap_years", "value_before", "value_after",
              "fold_change", "log_shift", "robust_z", "pattern", "n_years")

flags <- bind_rows(flags.yoy, flags.shift) %>%
    mutate(fold_change = exp(log_shift)) %>%          # odds ratio for proportions
    select(any_of(out.cols))

# Flagged by both rules within +/- 1 year of each other (strongest candidates for definition changes)
make.key <- function(df, year.col) do.call(paste, c(as.list(df[SERIES.KEYS]), list(df[[year.col]]), sep = "|"))

both <- inner_join(flags %>% filter(rule == "yoy_fold_jump")      %>% select(all_of(SERIES.KEYS), year_yoy = year),
                   flags %>% filter(rule == "robust_level_shift") %>% select(all_of(SERIES.KEYS), year_shift = year),
                   by = SERIES.KEYS, relationship = "many-to-many") %>%
    filter(abs(year_yoy - year_shift) <= 1)

both.keys <- c(make.key(both, "year_yoy"), make.key(both, "year_shift"))
flags$both_rules <- make.key(flags, "year") %in% both.keys

# Optional readable location names (silently skipped if the locations package is unavailable)
flags$location_name <- tryCatch(unname(locations::get.location.name(flags$location)),
                                error = function(e) NA_character_)

# Review order: both rules first, then totals before strata, then largest change
flags <- flags %>%
    arrange(desc(both_rules), stratification != "total", desc(abs(log_shift))) %>%
    relocate(both_rules, .after = rule) %>%
    relocate(location_name, .after = location)

write_csv(flags, OUTPUT.FILE)

## ---- 8. Console summary -----------------------------------------------------------------------------

cat("\nFlags written to", OUTPUT.FILE, "\n")
cat("Total flags:", nrow(flags), "| series flagged:", nrow(distinct(flags, across(all_of(SERIES.KEYS)))),
    "| flagged by both rules:", sum(flags$both_rules), "\n\n")
print(flags %>% count(outcome, rule) %>% tidyr::pivot_wider(names_from = rule, values_from = n, values_fill = 0),
      n = Inf)
