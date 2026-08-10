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
#
# OUTPUT: area.migration.long.form() returns a long-form data frame
# (outcome, year, location, value) suitable for the `census.immigration`
# ontology / `census.population` source in the HIV surveillance manager.
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
DATA.DIR.COUNTY.TO.COUNTY <- "Q:/data_raw/movement/county.to.county"

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
# locations package, and return the manager-ready long-form data frame.
area.migration.long.form <- function(area.code,
                                     window,
                                     year.label,
                                     data.dir = DATA.DIR.COUNTY.TO.COUNTY,
                                     verbose = TRUE) {
  area.counties <- unname(locations::get.contained.locations(area.code, "COUNTY"))
  if (!length(area.counties) || all(is.na(area.counties)))
    stop("No counties found for area '", area.code,
         "'. Is it registered in the `locations` package?")

  res <- compute.area.migration(area.counties, window, data.dir, verbose)

  data.frame(
    outcome  = c("immigration", "emigration"),
    year     = year.label,
    location = area.code,
    value    = c(res$immigration, res$emigration),
    stringsAsFactors = FALSE
  )
}
