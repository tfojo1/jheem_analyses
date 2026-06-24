# ---------------------------------------------------------------------------
# Register a custom "TGA" (Transitional Grant Area) location type and create
# one TGA, Oakland, made up of two counties: Alameda + Contra Costa.
#
# BEFORE RUNNING:
#   1. In RStudio: Session -> Restart R  (Cmd+Shift+F10). This is important:
#      reinstalling the package does NOT replace the copy already loaded in a
#      running session. If a traceback ever mentions "LOCATIONS_impl.R" or
#      "Location$new", you are still on the OLD version -- restart and reinstall.
#   2. Reinstall the latest 'locations' package from the repo.
#   3. Run this script in the fresh session.
#
# NOTE: these registrations live only for the current R session. They are not
# saved into the package, so run this script each session (e.g. source it at
# the top of your analysis).
# ---------------------------------------------------------------------------

library(locations)
library(rlang)

# Sanity check: confirm your reinstall actually took. You want >= 0.3.1.
# If this shows 0.1.0, you are on an old version -- restart R and reinstall.
stopifnot(packageVersion("locations") >= "0.3.1")

# Type names are UPPERCASE by convention ("TGA", "COUNTY"), matching the
# built-in types (STATE, COUNTY, CBSA, ...). (Note: in versions before 0.3.1,
# register.relationship.between.types required uppercase; uppercase is safe on
# every version, so we use it throughout.)

# 1. Register the new TYPE (guarded so re-running doesn't error).
if (!("TGA" %in% get.location.types())) {
  register.types(
    type            = "TGA",
    prefix          = "TGA.",
    prefix.longform = "Transitional Grant Area"
  )
}

# 2. Declare that a TGA completely contains counties (guarded).
if (!location.type.comprises("TGA", "COUNTY")) {
  register.relationship.between.types("TGA", "COUNTY", TRUE)
}

# 3. Register the single TGA location: code, then display name (guarded).
if (!is.location.valid("TGA.OAKLAND")) {
  register.locations(type = "TGA", locations = "TGA.OAKLAND", location.names = "Oakland")
}

# 4. Attach the two counties as completely-enclosed sub-locations.
#    (Safe to re-run.)
register.sub.and.super.locations(
  sub.locations                 = c("06001", "06013"),   # Alameda, Contra Costa
  super.locations               = c("TGA.OAKLAND", "TGA.OAKLAND"),
  super.completely.encloses.sub = TRUE
)

# --- Verify ---
cat("Counties in Oakland TGA:\n")
print(get.contained.locations("TGA.OAKLAND", sub.type = "county"))
cat("TGA comprises COUNTY:", location.type.comprises("TGA", "COUNTY"), "\n")


## TGA engine test and plotting 
source("applications/EHE/ehe_specification.R")
location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.09

engine.oakland = create.jheem.engine('ehe', 'TGA.OAKLAND', end.year=2035, max.run.time.seconds = 10)
sim.oakland = engine.oakland$run(parameters = params)

engine.sf = create.jheem.engine('ehe', 'C.41860', end.year=2035, max.run.time.seconds = 10)
sim.sf = engine.sf$run(parameters = params)

sim - sim.oakland

simplot(sim,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("population"),
        style.manager = source.style.manager, # use when looking at totals
        dimension.values = list(year = 2000:2030))

simplot(sim,
        #facet.by = "age", # age, race; 1-way
        outcomes = c("immigration","emigration"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(sim,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# must ask for years 1981-2001 
simplot(sim,
        outcomes = c("aids.deaths"),
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001))

simplot(sim,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("total.hiv.tests.per.population"), 
        style.manager = location.style.manager,plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 
