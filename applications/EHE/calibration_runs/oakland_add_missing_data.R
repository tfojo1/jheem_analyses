# =============================================================================
# OAKLAND TGA: IN-SESSION DATA PATCHES
# Adds outcomes that TGA.OAKLAND needs for calibration but that are not (yet)
# in the shared surveillance manager. Modifies SURVEILLANCE.MANAGER IN MEMORY
# ONLY -- nothing is saved to the cached/shared .rdata files.
#
# Each block here should eventually move into
# data_processing/hiv.surveillance.manager/add.oakland.tga.hiv.data.R
# (data team), at which point it can be deleted from this file.
#
# Requires locations >= 0.4.1 (put.msa.data.as.new.source.NEW uses include.partial)
# =============================================================================

if (packageVersion("locations") < "0.4.1")
    stop("oakland_add_missing_data.R needs locations >= 0.4.1 (you have ", packageVersion("locations"),
         "). Update with devtools::install_github('tfojo1/locations') and restart R.")

source('../jheem_analyses/data_processing/aggregate_county_to_msa_new.R')

OAKLAND.TGA = 'TGA.OAKLAND'

#-- General mortality ('deaths'): county -> TGA ----
# Same call as the county -> MSA deaths aggregation in
# data_processing/hiv.surveillance.manager/hiv.surveillance.processing.for.merging/updated.data.aggregation.jheem.R
# but with to.locations = TGA.OAKLAND and geographic.type.to = 'TGA'
# (as in add.oakland.tga.hiv.data.R)
# Weight counties by county-level adult.population (census.population). Use whichever
# ontologies actually hold county year__location data for the Oakland counties --
# these have moved between surveillance manager versions.
OAKLAND.COUNTIES = unname(locations::get.contained.locations(OAKLAND.TGA, 'COUNTY'))
adult.pop.by.ont = SURVEILLANCE.MANAGER$data$adult.population$estimate$census.population
rel.onts = names(adult.pop.by.ont)[sapply(names(adult.pop.by.ont), function(ont) {
    arr = adult.pop.by.ont[[ont]][['year__location']]
    !is.null(arr) && all(OAKLAND.COUNTIES %in% dimnames(arr)$location) &&
        any(apply(!is.na(arr[, OAKLAND.COUNTIES, drop = F]), 1, all))
})]
if (length(rel.onts) == 0)
    stop("Oakland data patch: no county-level adult.population (source 'census.population', stratification 'year__location') ",
         "for ", paste(OAKLAND.COUNTIES, collapse = ', '), ". Ontologies present: ",
         paste(names(adult.pop.by.ont), collapse = ', '))
print(paste0("Oakland data patch: weighting county deaths with adult.population from ontology(ies): ",
             paste(rel.onts, collapse = ', ')))

for (rel.ont in rel.onts)
{
    put.msa.data.as.new.source.NEW(outcome = 'deaths',
                                   from.source.name = 'census.deaths',
                                   to.source.name = 'census.deaths.aggregated',
                                   to.locations = OAKLAND.TGA,
                                   geographic.type.from = 'COUNTY',
                                   geographic.type.to = 'TGA',
                                   details.for.new.data = 'estimated from county data',
                                   data.manager = SURVEILLANCE.MANAGER,
                                   required.coverage = 0.95,
                                   outcome.for.relative.contribution = 'adult.population',
                                   source.for.relative.contribution = 'census.population',
                                   ontology.for.relative.contribution = rel.ont)
}

# The first year of each Census estimates vintage (2000, 2010, 2020) covers only
# April 1 - June 30 (~1/4 year), and section2_census.postcensal.deaths.by.county.R
# keeps exactly those partial years. Remove them for Oakland.
# (Data-team fix: drop each vintage's FIRST year instead of the older vintage's overlap year.)
# (The ontology holding the aggregated deaths follows the county deaths' ontology --
#  'census' in older surveillance managers, 'census.adults' from v2026.10.29 -- so find it.)
oak.death.onts = names(SURVEILLANCE.MANAGER$data$deaths$estimate$census.deaths.aggregated)
oak.death.onts = oak.death.onts[sapply(oak.death.onts, function(ont) {
    arr = SURVEILLANCE.MANAGER$data$deaths$estimate$census.deaths.aggregated[[ont]][['year__location']]
    !is.null(arr) && OAKLAND.TGA %in% dimnames(arr)$location
})]
for (death.ont in oak.death.onts)
{
    partial.years = intersect(c('2000', '2010', '2020'),
                              dimnames(SURVEILLANCE.MANAGER$data$deaths$estimate$census.deaths.aggregated[[death.ont]][['year__location']])$year)
    if (length(partial.years) > 0)
        SURVEILLANCE.MANAGER$remove_data(outcome = 'deaths',
                                         source = 'census.deaths.aggregated',
                                         ontology.name = death.ont,
                                         dimension.values = list(location = OAKLAND.TGA,
                                                                 year = partial.years),
                                         details.for.removal = 'partial-year (Apr-Jun) Census estimate removed',
                                         url.for.removal = 'www.census.gov')
}

oakland.deaths = SURVEILLANCE.MANAGER$pull(outcome = 'deaths', dimension.values = list(location = OAKLAND.TGA))
if (is.null(oakland.deaths))
    stop("Oakland data patch: aggregation ran but no 'deaths' data exist for ", OAKLAND.TGA)
oakland.death.years = dimnames(oakland.deaths)$year[apply(!is.na(oakland.deaths), which(names(dimnames(oakland.deaths)) == 'year'), any)]
print(paste0("Oakland data patch: 'deaths' (ontology ", paste(oak.death.onts, collapse = ', '), ") for ", OAKLAND.TGA, " in ", length(oakland.death.years),
             " years (", paste(range(oakland.death.years), collapse = '-'), "); 2000/2010/2020 present: ",
             paste(intersect(c('2000','2010','2020'), oakland.death.years), collapse = ', ')))


#-- Migration ('immigration'/'emigration'): rescale for the likelihood ----
# The ACS county-to-county flows (add.oakland.tga.migration.values.R) are ANNUAL
# movers (moved in the past year), averaged over a 5-year ACS window, but the
# likelihood compares a "2011-2015"-style range to the SIM'S 5-YEAR TOTAL.
# They also cover ages 1+, while the sim covers model ages (13+).
# So: adjusted = annual value * 5 * (share of the 1+ population aged 13+).
# The adjusted values REPLACE Oakland's values (in memory only) under the original
# source/ontology, because the spec ALSO reads these data: get.immigration.rates()
# and get.emigration.rates() pull them and divide by 5 ("5-year aggregate"). With
# the replacement, both the starting rates (-> annual adult rate) and the
# likelihoods (5-year totals vs. the sim's 5-year sum) are on the right scale.

# Share of the 1+ population aged 13+, by year, from census single-year ages
oak.age.pop = CENSUS.MANAGER$pull(outcome = 'population',
                                  dimension.values = list(location = OAKLAND.COUNTIES,
                                                          year = as.character(2005:2020)),
                                  keep.dimensions = c('year', 'age'),
                                  from.ontology.names = 'single.year.age.census')
if (is.null(oak.age.pop))
    stop("Oakland data patch: no single-year-age census population for the Oakland counties")
if (is.null(names(dimnames(oak.age.pop))) || !all(c('year', 'age') %in% names(dimnames(oak.age.pop))))
    stop("Oakland data patch: unexpected shape for census population pull; dims = ",
         paste(dim(oak.age.pop), collapse = ' x '), ", dimnames = ",
         paste(names(dimnames(oak.age.pop)), collapse = ', '))
if (length(dim(oak.age.pop)) > 2) # e.g. a source dimension: take the first along any extra dims
{
    extra.dims = setdiff(names(dimnames(oak.age.pop)), c('year', 'age'))
    first.only = lapply(extra.dims, function(d) 1)
    names(first.only) = extra.dims
    oak.age.pop = array.access(oak.age.pop, first.only, drop = F)
    oak.age.pop = apply(oak.age.pop, c('year', 'age'), sum)
}
oak.age.lower = parse.age.strata.names(dimnames(oak.age.pop)$age)$lower
oak.adult.share.by.year = rowSums(oak.age.pop[, oak.age.lower >= 13, drop = F], na.rm = T) /
                          rowSums(oak.age.pop[, oak.age.lower >= 1, drop = F], na.rm = T)
oak.adult.share.by.year = oak.adult.share.by.year[is.finite(oak.adult.share.by.year) & oak.adult.share.by.year > 0]

get.oakland.adult.share = function(year.range)
{
    bounds = as.numeric(strsplit(year.range, '-')[[1]])
    yrs = as.character(bounds[1]:bounds[2])
    yrs = intersect(yrs, names(oak.adult.share.by.year))
    if (length(yrs) == 0)
        mean(oak.adult.share.by.year)
    else
        mean(oak.adult.share.by.year[yrs])
}

for (migration.outcome in c('immigration', 'emigration'))
{
    raw = SURVEILLANCE.MANAGER$data[[migration.outcome]]$estimate$census.population$census.immigration$year__location
    if (is.null(raw) || !(OAKLAND.TGA %in% dimnames(raw)$location))
        stop("Oakland data patch: no total '", migration.outcome, "' for ", OAKLAND.TGA)
    raw = raw[, OAKLAND.TGA]
    raw = raw[!is.na(raw)]
    adult.share = sapply(names(raw), get.oakland.adult.share)
    
    # Safe to re-run in the same R session: if the manager already holds the
    # adjusted values from an earlier run, rescale from the stored originals
    # instead of multiplying again
    if (!exists('OAKLAND.ORIGINAL.MIGRATION'))
        OAKLAND.ORIGINAL.MIGRATION = list()
    prev.orig = OAKLAND.ORIGINAL.MIGRATION[[migration.outcome]]
    already.adjusted = !is.null(prev.orig) && setequal(names(prev.orig), names(raw)) &&
        all(abs(raw - round(prev.orig[names(raw)] * 5 * adult.share)) < 1)
    if (already.adjusted)
    {
        print(paste0("Oakland data patch: '", migration.outcome, "' already rescaled in this session - leaving as is"))
        next
    }
    OAKLAND.ORIGINAL.MIGRATION[[migration.outcome]] = raw
    
    adjusted = data.frame(outcome = migration.outcome,
                          year = names(raw),
                          location = OAKLAND.TGA,
                          value = round(raw * 5 * adult.share),
                          stringsAsFactors = F)
    
    SURVEILLANCE.MANAGER$put.long.form(data = adjusted,
                                       ontology.name = 'census.immigration',
                                       source = 'census.population',
                                       dimension.values = list(),
                                       url = 'https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html',
                                       details = 'Oakland TGA county-to-county flows: annual movers x 5 (5-year total) x share of 1+ population aged 13+')
    
    print(paste0("Oakland data patch: '", migration.outcome, "' rescaled (x5, adult share ",
                 paste(range(round(adult.share, 3)), collapse = '-'), ") for ", length(raw), " windows"))
}


#-- Heroin & cocaine (NSDUH): substate regions -> TGA ----
# The TGA is exactly two NSDUH substate regions -- Region 7R (Contra Costa) and
# Region 8R (Alameda), one county each -- so jheem2 refuses the nested-proportion
# likelihood and wants the data aggregated to the TGA. Oakland's value is the
# average of the regions' proportions weighted by each region's adult population
# (county adult.population, averaged over the years in each NSDUH year range).
# NOTE: age-stratified values use the same TOTAL adult-population weights.

oak.nsduh.regions = unique(unname(unlist(locations::get.containing.locations(OAKLAND.COUNTIES, 'NSDUH'))))
oak.region.counties = lapply(oak.nsduh.regions, function(r) unname(locations::get.contained.locations(r, 'COUNTY')))
names(oak.region.counties) = oak.nsduh.regions
if (length(oak.nsduh.regions) == 0 ||
    !setequal(unlist(oak.region.counties), OAKLAND.COUNTIES))
    stop("Oakland data patch: NSDUH regions (", paste(oak.nsduh.regions, collapse = ', '),
         ") do not exactly cover the Oakland TGA counties")

oak.county.adult.pop = adult.pop.by.ont[[rel.onts[1]]][['year__location']][, OAKLAND.COUNTIES, drop = F]
get.oak.region.weights = function(year.label)
{
    bounds = as.numeric(strsplit(year.label, '-')[[1]])
    yrs = as.character(bounds[1]:bounds[length(bounds)])
    yrs = intersect(yrs, dimnames(oak.county.adult.pop)$year)
    if (length(yrs) == 0) # fall back to the closest available year
    {
        avail = as.numeric(dimnames(oak.county.adult.pop)$year)
        yrs = as.character(avail[which.min(abs(avail - mean(bounds)))])
    }
    county.pop = colMeans(oak.county.adult.pop[yrs, , drop = F], na.rm = T)
    sapply(oak.region.counties, function(cties) sum(county.pop[cties]))
}

for (nsduh.outcome in c('heroin', 'cocaine'))
{
    by.strat = SURVEILLANCE.MANAGER$data[[nsduh.outcome]]$estimate$nsduh$nsduh
    if (is.null(by.strat))
        stop("Oakland data patch: no '", nsduh.outcome, "' data (source 'nsduh', ontology 'nsduh')")
    n.put = 0
    for (strat in names(by.strat))
    {
        arr = by.strat[[strat]]
        if (!all(oak.nsduh.regions %in% dimnames(arr)$location))
            next
        arr = array.access(arr, location = oak.nsduh.regions, drop = F)
        df = as.data.frame.table(arr, stringsAsFactors = F, responseName = 'value')
        # weighted average across the regions within each year (x other strata)
        df$weight = mapply(function(yr, loc) get.oak.region.weights(yr)[[loc]], df$year, df$location)
        other.cols = setdiff(names(df), c('location', 'value', 'weight'))
        df = df[!is.na(df$value), ]
        key = do.call(paste, c(df[other.cols], sep = '\r'))
        complete = tapply(df$location, key, function(l) setequal(l, oak.nsduh.regions))
        df = df[key %in% names(complete)[complete], ]
        if (nrow(df) == 0)
            next
        key = do.call(paste, c(df[other.cols], sep = '\r'))
        agg = do.call(rbind, lapply(split(df, key), function(d) {
            out = d[1, other.cols, drop = F]
            out$value = sum(d$value * d$weight) / sum(d$weight)
            out
        }))
        agg$location = OAKLAND.TGA
        agg$outcome = nsduh.outcome
        SURVEILLANCE.MANAGER$put.long.form(data = agg,
                                           ontology.name = 'nsduh',
                                           source = 'nsduh',
                                           dimension.values = list(),
                                           url = 'https://pdas.samhsa.gov/saes/substate',
                                           details = 'NSDUH Substate Estimates, adult-population-weighted average of Regions 7R + 8R for Oakland TGA')
        n.put = n.put + 1
    }
    if (n.put == 0)
        stop("Oakland data patch: could not build '", nsduh.outcome, "' for ", OAKLAND.TGA)
    print(paste0("Oakland data patch: '", nsduh.outcome, "' aggregated from ",
                 paste(oak.nsduh.regions, collapse = ' + '), " (", n.put, " stratifications)"))
}


#-- STI year-on-year ratios ('gonorrhea.ratio', 'ps.syphilis.ratio') ----
# sti_ratio_calculation.R builds these from county-aggregated case counts in section 4
# of the surveillance manager build -- BEFORE add.oakland.tga.hiv.data.R adds Oakland's
# gonorrhea/ps.syphilis at the end -- so Oakland has counts but no ratios.
# Same calculation here: value(year) / value(year - 1), total and by sex, race, age.
# (Data-team fix: run sti_ratio_calculation.R after the Oakland data are added, or
#  include TGA.OAKLAND in it.)
for (sti in c('gonorrhea', 'ps.syphilis'))
{
    by.strat = SURVEILLANCE.MANAGER$data[[sti]]$estimate$cdc.aggregated.county$cdc.sti
    if (is.null(by.strat))
        stop("Oakland data patch: no '", sti, "' (source 'cdc.aggregated.county', ontology 'cdc.sti')")
    n.put = 0
    for (strat in intersect(names(by.strat), paste0('year__location', c('', '__sex', '__race', '__age'))))
    {
        arr = by.strat[[strat]]
        if (!(OAKLAND.TGA %in% dimnames(arr)$location))
            next
        df = as.data.frame.table(array.access(arr, location = OAKLAND.TGA, drop = F),
                                 stringsAsFactors = F, responseName = 'value')
        df = df[!is.na(df$value), ]
        df$prev.year = as.character(as.numeric(df$year) - 1)
        other.cols = setdiff(names(df), c('year', 'value', 'prev.year'))
        prev = df[, c(other.cols, 'year', 'value')]
        names(prev)[names(prev) == 'year'] = 'prev.year'
        names(prev)[names(prev) == 'value'] = 'prev.value'
        ratio = merge(df, prev, by = c(other.cols, 'prev.year'))
        ratio = ratio[ratio$prev.value > 0, ]
        if (nrow(ratio) == 0)
            next
        ratio$value = ratio$value / ratio$prev.value
        ratio$outcome = paste0(sti, '.ratio')
        ratio = ratio[, c('outcome', 'year', other.cols, 'value')]
        SURVEILLANCE.MANAGER$put.long.form(data = ratio,
                                           ontology.name = 'cdc.sti',
                                           source = 'cdc.sti',
                                           dimension.values = list(),
                                           url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
                                           details = 'Year-over-year ratio computed in-session for Oakland TGA from county-aggregated cases')
        n.put = n.put + 1
    }
    if (n.put == 0)
        stop("Oakland data patch: could not build '", sti, ".ratio' for ", OAKLAND.TGA)
    print(paste0("Oakland data patch: '", sti, ".ratio' computed (", n.put, " stratifications)"))
}
