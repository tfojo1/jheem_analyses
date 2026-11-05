# =============================================================================
# tga_migration.R
#
# Compute immigration & emigration for an AREA (a set of whole counties, e.g. a
# Ryan White Transitional Grant Area) from the Census ACS county-to-county
# migration flow files.
#
# WHY THIS EXISTS: a TGA is not a Census MSA, so there is no pre-aggregated
# metro-to-metro flow product for it (unlike CBSAs, handled in immigration_new.R).
# We therefore build area flows up from the county-to-county data.
#
# METHOD (boundary-crossing filter):
#   - Immigration to area A = sum of county-to-county flows INTO any county in A
#       from any origin NOT in A. Origins outside the US ("from abroad") are
#       included, matching the census.immigration convention for immigration.
#   - Emigration from area A = sum of flows OUT of any county in A to any
#       destination NOT in A. Domestic only -- ACS does not observe moves abroad.
#   - Intra-area moves are excluded automatically by the filter (no subtraction).
#
# SCOPE: total population only (the county-to-county files carry no age/sex/race).
# Given a census manager, adult (13+) immigration and emigration are estimated
# from the totals with the census age distribution of the area's counties.
#
# OUTPUT: area.migration.long.form() returns a long-form data frame
# (outcome, year, location, value). Values are 5-year window totals: the flow
# files give annual movers averaged over the 5-year ACS window, so, like the MSA
# data in immigration_new.R (since November 2026), they are multiplied by 5.
# immigration/emigration suit the `census.immigration` ontology and
# adult.immigration/adult.emigration the `census.immigration.adults` ontology,
# both under the `census.population` source in the HIV surveillance manager.
#
# REQUIRES: readxl; the `locations` package (>= 0.4.0) for area -> county lookup.
# =============================================================================

library(readxl)

# Needs the `locations` package installed (used fully-namespaced below, so it
# does not need to be attached). 0.4.0 is the version that added the TGA type;
# fail early with a clear message rather than later on a failed lookup.
if (!requireNamespace("locations", quietly = TRUE) ||
    packageVersion("locations") < "0.4.0")
  stop("tga_migration.R needs the `locations` package (>= 0.4.0, which adds the ",
       "TGA type). Update it and restart R.")

# Raw-data location on the team NAS. Override `data.dir` to read from elsewhere.
DATA.DIR.COUNTY.TO.COUNTY <- Sys.getenv(
  "COUNTY_TO_COUNTY_DIR", "Q:/data_raw/movement/county.to.county")

# Column positions in the county-to-county flow files (data starts on row 5).
#   1-2  destination (current residence) state / county code
#   3-4  origin (residence 1 year ago) state / county code
#   15   destination marginal: Movers from Different County, Same State (Estimate)
#   17   destination marginal: Movers from Different State (Estimate)
#   19   destination marginal: Movers from Abroad (Estimate)
#   37   the bilateral county-to-county flow (Estimate)
.FLOW.COL <- c(dest_state = 1, dest_county = 2, orig_state = 3, orig_county = 4,
               movers_diff_county_same_state = 15, movers_diff_state = 17,
               movers_from_abroad = 19, flow = 37)

# Build a 5-digit county FIPS from the file's 3-digit state + 3-digit county
# codes (e.g. "006" + "001" -> "06001"). Foreign/abroad origins (non-numeric
# state code) return NA, which correctly never matches an area county.
.make.fips <- function(state.code, county.code) {
  st <- suppressWarnings(as.integer(state.code))
  ifelse(is.na(st), NA_character_, paste0(sprintf("%02d", st), county.code))
}

# Locate the workbook for a window ("2016.2020") and direction ("inflow"/"outflow"),
# tolerating the .xlsx (newer) / .xls (older) extension split.
.find.flow.file <- function(data.dir, window, direction) {
  candidates <- file.path(data.dir, paste0(window, ".", direction, c(".xlsx", ".xls")))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit))
    stop(sprintf("Cannot find %s file for window '%s' in %s", direction, window, data.dir))
  hit[1]
}

# Read one state sheet into a tidy frame of just the columns we use.
.read.flow.sheet <- function(file, sheet) {
  d <- as.data.frame(read_excel(file, sheet = sheet, col_names = FALSE,
                                skip = 4, col_types = "text", .name_repair = "minimal"))
  num <- function(i) suppressWarnings(as.numeric(d[[i]]))
  data.frame(
    dest_fips = .make.fips(d[[.FLOW.COL[["dest_state"]]]], d[[.FLOW.COL[["dest_county"]]]]),
    orig_fips = .make.fips(d[[.FLOW.COL[["orig_state"]]]], d[[.FLOW.COL[["orig_county"]]]]),
    movers_diff_county_same_state = num(.FLOW.COL[["movers_diff_county_same_state"]]),
    movers_diff_state             = num(.FLOW.COL[["movers_diff_state"]]),
    movers_from_abroad            = num(.FLOW.COL[["movers_from_abroad"]]),
    flow                          = num(.FLOW.COL[["flow"]]),
    stringsAsFactors = FALSE
  )
}

# Full state name(s) (= Census sheet names) for a set of county FIPS. The
# locations package returns the state abbreviation as the value and the full
# state name as the name attribute -- the latter matches the workbook sheets.
.sheets.for.counties <- function(area.counties) {
  unique(unlist(lapply(area.counties, function(cty) {
    names(locations::get.containing.locations(cty, "STATE"))
  })))
}

# Reconciliation guard: for each area county, the sum of itemized inbound
# county-to-county flows (incl. abroad) should match that county's marginal
# totals. A large shortfall means Census suppressed small flows, so the filtered
# sum would undercount -- worth knowing for small/rural areas.
.reconcile.inflow <- function(inflow, area.counties) {
  do.call(rbind, lapply(area.counties, function(cty) {
    rows <- inflow[!is.na(inflow$dest_fips) & inflow$dest_fips == cty, ]
    if (!nrow(rows)) return(NULL)
    keep <- is.na(rows$orig_fips) | rows$orig_fips != cty   # exclude within-same-county
    itemized <- sum(rows$flow[keep], na.rm = TRUE)
    marginal <- rows$movers_diff_county_same_state[1] +
                rows$movers_diff_state[1] + rows$movers_from_abroad[1]
    data.frame(county = cty, itemized = itemized, marginal = marginal,
               pct_diff = round(100 * (itemized - marginal) / marginal, 2))
  }))
}

# The flow files give annual movers averaged over a 5-year ACS window; the
# manager holds window totals.
.WINDOW.YEARS <- 5

# Share of the area's population aged 1+ that is aged 13+, by year, from census
# single-year ages. The flow files cover movers aged 1+ and carry no ages, so
# adult movers are estimated with this share. Same method as the Oakland
# in-session patch (applications/EHE/calibration_runs/oakland_add_missing_data.R).
.adult.share.by.year <- function(area.counties, census.manager, years = 2005:2020) {
  pop <- census.manager$pull(outcome = "population",
                             dimension.values = list(location = area.counties,
                                                     year = as.character(years)),
                             keep.dimensions = c("year", "age"),
                             from.ontology.names = "single.year.age.census")
  if (is.null(pop) || !all(c("year", "age") %in% names(dimnames(pop))))
    stop("No single-year-age census population for counties ",
         paste(area.counties, collapse = ", "))
  extra.dims <- setdiff(names(dimnames(pop)), c("year", "age"))
  if (length(extra.dims)) {   # e.g. a source dimension: take the first along each
    first.only <- setNames(as.list(rep(1, length(extra.dims))), extra.dims)
    pop <- apply(array.access(pop, first.only, drop = FALSE), c("year", "age"), sum)
  }
  lower <- parse.age.strata.names(dimnames(pop)$age)$lower
  share <- rowSums(pop[, lower >= 13, drop = FALSE], na.rm = TRUE) /
           rowSums(pop[, lower >= 1, drop = FALSE], na.rm = TRUE)
  share[is.finite(share) & share > 0]
}

# The adult share for a window ("2011-2015"): the mean over the window's years
# that have census data, or over all years if none do.
.adult.share.for.window <- function(share.by.year, year.label) {
  bounds <- as.numeric(strsplit(year.label, "-")[[1]])
  years <- intersect(as.character(bounds[1]:bounds[2]), names(share.by.year))
  if (length(years)) mean(share.by.year[years]) else mean(share.by.year)
}

# --- core ------------------------------------------------------------------

# Compute immigration/emigration for an explicit set of county FIPS.
compute.area.migration <- function(area.counties,
                                   window,
                                   data.dir = DATA.DIR.COUNTY.TO.COUNTY,
                                   verbose = TRUE) {
  area.counties <- toupper(area.counties)
  sheets <- .sheets.for.counties(area.counties)

  inflow.file  <- .find.flow.file(data.dir, window, "inflow")
  outflow.file <- .find.flow.file(data.dir, window, "outflow")

  inflow  <- do.call(rbind, lapply(sheets, function(s) .read.flow.sheet(inflow.file, s)))
  outflow <- do.call(rbind, lapply(sheets, function(s) .read.flow.sheet(outflow.file, s)))

  dest.in.area <- inflow$dest_fips  %in% area.counties
  orig.in.area <- inflow$orig_fips  %in% area.counties      # NA origins -> FALSE (abroad)
  immigration  <- sum(inflow$flow[dest.in.area & !orig.in.area], na.rm = TRUE)
  internal     <- sum(inflow$flow[dest.in.area &  orig.in.area], na.rm = TRUE)

  o.orig.in.area <- outflow$orig_fips %in% area.counties
  o.dest.in.area <- outflow$dest_fips %in% area.counties
  emigration     <- sum(outflow$flow[o.orig.in.area & !o.dest.in.area], na.rm = TRUE)

  reconciliation <- .reconcile.inflow(inflow, area.counties)

  if (verbose) {
    cat(sprintf("Area counties: %s\n", paste(area.counties, collapse = ", ")))
    cat(sprintf("Window %s (sheets: %s)\n", window, paste(sheets, collapse = ", ")))
    cat(sprintf("  immigration (incl. abroad): %d\n", immigration))
    cat(sprintf("  emigration  (domestic):     %d\n", emigration))
    cat(sprintf("  internal moves (excluded):  %d\n", internal))
    cat("  reconciliation (itemized vs marginal inbound, per county):\n")
    print(reconciliation, row.names = FALSE)
    bad <- reconciliation[abs(reconciliation$pct_diff) > 1, , drop = FALSE]
    if (nrow(bad))
      warning("Reconciliation off by >1% for county(ies): ",
              paste(bad$county, collapse = ", "),
              " -- possible flow suppression; filtered sum may undercount.")
  }

  list(immigration = immigration, emigration = emigration,
       internal = internal, reconciliation = reconciliation, sheets = sheets)
}

# Take an area location CODE (e.g. a TGA), resolve its member counties via the
# locations package, and return the manager-ready long-form data frame: 5-year
# immigration and emigration totals, and, given a census manager, their adult
# (13+) estimates.
area.migration.long.form <- function(area.code,
                                     window,
                                     year.label,
                                     data.dir = DATA.DIR.COUNTY.TO.COUNTY,
                                     census.manager = NULL,
                                     verbose = TRUE) {
  area.counties <- unname(locations::get.contained.locations(area.code, "COUNTY"))
  if (!length(area.counties) || all(is.na(area.counties)))
    stop("No counties found for area '", area.code,
         "'. Is it registered in the `locations` package?")

  res <- compute.area.migration(area.counties, window, data.dir, verbose)
  totals <- c(immigration = res$immigration, emigration = res$emigration) * .WINDOW.YEARS

  long.form <- data.frame(
    outcome  = names(totals),
    year     = year.label,
    location = area.code,
    value    = unname(totals),
    stringsAsFactors = FALSE
  )
  if (!is.null(census.manager)) {
    share <- .adult.share.for.window(.adult.share.by.year(area.counties, census.manager),
                                     year.label)
    long.form <- rbind(long.form, data.frame(
      outcome  = paste0("adult.", names(totals)),
      year     = year.label,
      location = area.code,
      value    = round(unname(totals) * share),
      stringsAsFactors = FALSE
    ))
  }
  long.form
}
