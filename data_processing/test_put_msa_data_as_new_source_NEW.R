# Tests for put.msa.data.as.new.source.NEW: that county data are aggregated to the MSA correctly, and that the
# MSA data get the metadata (url, details, other fields) of exactly the county points that contributed.
#
# Requires jheem2 with metadata records, loaded with its internals (the function uses apply.robust), eg
#   pkgload::load_all('path/to/jheem2')
# Set FUNCTION.FILE to the file that defines put.msa.data.as.new.source.NEW before sourcing this one, or edit the default below.
#
# locations::get.contained.locations is replaced with a fake for the duration of the tests, so that the counties in each MSA
# (and so the coverage fractions) are fixed and don't depend on the version of the locations package. It is restored at the end.

if (!exists('FUNCTION.FILE'))
    FUNCTION.FILE = '../jheem_analyses/data_processing/put_msa_data_as_new_source_NEW.R'
source(FUNCTION.FILE)

if (!exists('apply.robust'))
    stop("apply.robust not found: load jheem2 with pkgload::load_all() so its internal functions are available")

#-- Helpers --#
check = function(description, actual, expected)
{
    if (!isTRUE(all.equal(actual, expected, check.attributes = F)) || !identical(is.na(actual), is.na(expected))) {
        cat("FAILED:", description, "\n  expected: "); dput(expected)
        cat("  actual:   "); dput(actual)
        stop("Test failed: ", description)
    }
    cat("  ok:", description, "\n")
}

check.error = function(description, expr, pattern)
{
    message = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(message) || !grepl(pattern, message))
        stop("Test failed: ", description, "\n  expected an error matching '", pattern, "', got: ",
             if (is.null(message)) "no error" else message)
    cat("  ok:", description, "\n")
}

#-- Fake geography --#
# MSA 1 (Baltimore) has three counties; with the populations below they make up 70%, 25% and 5% of the MSA
# MSA 2 (Houston) has two counties
MSA.1 = 'C.12580'
MSA.2 = 'C.26420'
A = '24005'; B = '24510'; C = '24003'
D = '48201'; E = '48157'
FAKE.COUNTIES = list(C.12580 = c(A, B, C), C.26420 = c(D, E))

# Note from Andrew: Since apparently the "locations" package changes sometimes,
# this replaces it with a fake version defined above. When the tryCatch loop
# finishes for any reason, the "finally" argument tells it to run the "set.get."
# function defined here to put the official locations package back in.
locations.ns = asNamespace('locations')
real.get.contained.locations = locations.ns$get.contained.locations
fake.get.contained.locations = function(locations, sub.type, ...) unname(unlist(FAKE.COUNTIES[locations]))
set.get.contained.locations = function(fun)
{
    unlockBinding('get.contained.locations', locations.ns)
    assign('get.contained.locations', fun, envir = locations.ns)
    lockBinding('get.contained.locations', locations.ns)
}

#-- Data manager setup --#
YEARS = c('2020', '2021')
DETAILS.FOR.NEW.DATA = 'Aggregated from county data'

make.test.data.manager = function(author.required = F)
{
    dm = create.data.manager('msa.aggregation.test', description = 'a data manager to test county-to-MSA aggregation')

    count.metadata = function(name) create.outcome.metadata(scale = 'non.negative.number', display.name = name, axis.name = name,
                                                            units = 'people', description = name)
    dm$register.outcome('population', metadata = count.metadata('Population'))
    dm$register.outcome('diagnoses', metadata = count.metadata('Diagnoses'))
    dm$register.outcome('diagnosed.prevalence', metadata = count.metadata('Diagnosed Prevalence'))
    dm$register.outcome('suppression', denominator.outcome = 'diagnosed.prevalence',
                        metadata = create.outcome.metadata(scale = 'proportion', display.name = 'Suppression', axis.name = 'Suppression',
                                                           units = '%', description = 'Proportion virally suppressed'))

    dm$register.parent.source('cdc', full.name = 'CDC', short.name = 'cdc')
    dm$register.parent.source('census', full.name = 'Census', short.name = 'census')
    dm$register.source('cdc.county', parent.source = 'cdc', full.name = 'CDC county data', short.name = 'cdc.county')
    dm$register.source('cdc.msa', parent.source = 'cdc', full.name = 'CDC data aggregated to MSA', short.name = 'cdc.msa')
    dm$register.source('census.population', parent.source = 'census', full.name = 'Census population', short.name = 'census.pop')

    # year before location, as in the real ontologies, so the stratification is 'year__location' (which the function expects)
    dm$register.ontology('test.ont', ont = ontology(year = NULL, location = NULL, sex = c('male', 'female')))
    dm$register.metadata.field('author', required = author.required)

    # Population, for relative contributions
    population = c(700, 250, 50, 600, 400)
    names(population) = c(A, B, C, D, E)
    for (loc in names(population))
        dm$put(array(population[[loc]], c(1, 2), list(location = loc, year = YEARS)),
               outcome = 'population', source = 'census.population', ontology.name = 'test.ont',
               url = 'http://census.gov', details = 'Census estimates', metadata = list(author = 'Census'))
    dm
}

# Puts one county's values (a named vector by year; NA years are left out) with their own url and author
put.county = function(dm, outcome, county, values.by.year, url, author = NULL, sex = NULL)
{
    values.by.year = values.by.year[!is.na(values.by.year)]
    dn = list(location = county, year = names(values.by.year))
    if (!is.null(sex)) dn$sex = sex
    dm$put(array(values.by.year, sapply(dn, length), dn), outcome = outcome, source = 'cdc.county', ontology.name = 'test.ont',
           url = url, details = 'County method', metadata = if (!is.null(author)) list(author = author))
}

aggregate.to.msa = function(dm, outcome, to.locations = MSA.1, ...)
{
    capture.output(  # the function prints a line for every put
        put.msa.data.as.new.source.NEW(data.manager = dm,
                                       outcome = outcome,
                                       from.source.name = 'cdc.county',
                                       to.source.name = 'cdc.msa',
                                       to.locations = to.locations,
                                       geographic.type.from = 'COUNTY',
                                       geographic.type.to = 'CBSA',
                                       details.for.new.data = DETAILS.FOR.NEW.DATA,
                                       outcome.for.relative.contribution = 'population',
                                       source.for.relative.contribution = 'census.population',
                                       ontology.for.relative.contribution = 'test.ont',
                                       ...))
    invisible(dm)
}

# The MSA's data by year (and by any other kept dimensions), with the requested metadata attributes
pull.msa = function(dm, outcome, location = MSA.1, keep.dimensions = 'year')
{
    rv = dm$pull(outcome, keep.dimensions = keep.dimensions, dimension.values = list(location = location),
                 sources = 'cdc.msa', append.attributes = c('url', 'details', 'author'))
    rv
}

# Stored values for one MSA stratum, or NULL if nothing was put there
msa.stratum = function(dm, outcome, stratification = 'year__location')
    dm$data[[outcome]][['estimate']][['cdc.msa']][['test.ont']][[stratification]]

set.get.contained.locations(fake.get.contained.locations)
tryCatch({

    #-- 1. Counts with full coverage: summed, with every county's url and author --#
    cat("1. Counts with full coverage\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 10, '2021' = 11), url = 'http://a.gov', author = 'Smith')
    put.county(dm, 'diagnoses', B, c('2020' = 20, '2021' = 21), url = c('http://b.gov', 'http://shared.gov'), author = 'Jones')
    put.county(dm, 'diagnoses', C, c('2020' = 30, '2021' = 31), url = 'http://shared.gov', author = 'Smith')
    aggregate.to.msa(dm, 'diagnoses')

    msa = pull.msa(dm, 'diagnoses')
    check("counts are summed", as.vector(msa[YEARS, 'cdc.msa']), c(60, 63))
    check("url is the union of every county's urls, without duplicates", attr(msa, 'url')['2020', 'cdc.msa'],
          'http://a.gov | http://b.gov | http://shared.gov')
    check("details are details.for.new.data, not the counties' details", attr(msa, 'details')['2020', 'cdc.msa'], DETAILS.FOR.NEW.DATA)
    check("other metadata fields are carried over and merged", attr(msa, 'author')['2020', 'cdc.msa'], 'Jones | Smith')
    stored.ids = dm$metadata[['diagnoses']][['estimate']][['cdc.msa']][['test.ont']][['year__location']]
    check("the MSA put makes exactly one metadata record", length(unique(as.vector(stored.ids))), 1L)
    check("the census population's metadata is not mixed in", grepl('census', attr(msa, 'url')['2020', 'cdc.msa']), FALSE)

    #-- 2. Coverage: a year without enough coverage is not put, and counties behind only that year add no metadata --#
    cat("2. Coverage condition\n")
    dm = make.test.data.manager()
    # 2020: A and B (95% of population) -> aggregated. 2021: B and C (30%) -> not enough coverage
    put.county(dm, 'diagnoses', A, c('2020' = 10, '2021' = NA), url = 'http://a.gov')
    put.county(dm, 'diagnoses', B, c('2020' = 20, '2021' = 21), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = NA, '2021' = 31), url = 'http://c-only-2021.gov')
    aggregate.to.msa(dm, 'diagnoses')

    msa = pull.msa(dm, 'diagnoses')
    check("a year with enough coverage sums the counties that have data", msa['2020', 'cdc.msa'], 30)
    check("a year without enough coverage is NA", if ('2021' %in% dimnames(msa)$year) msa['2021', 'cdc.msa'] else NA_real_, NA_real_)
    check("a county that contributed only to a dropped year adds no url", attr(msa, 'url')['2020', 'cdc.msa'], 'http://a.gov | http://b.gov')

    #-- 3. Coverage: nothing put when no year has enough coverage --#
    cat("3. No year with enough coverage\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', B, c('2020' = 20, '2021' = 21), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = 30, '2021' = 31), url = 'http://c.gov')
    aggregate.to.msa(dm, 'diagnoses')
    stratum = msa.stratum(dm, 'diagnoses')
    check("no MSA values are put", is.null(stratum) || all(is.na(stratum)), TRUE)

    #-- 4. skip.coverage.condition with 'years' (this used to error) --#
    cat("4. skip.coverage.condition with years\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 10, '2021' = NA), url = 'http://a.gov')
    put.county(dm, 'diagnoses', B, c('2020' = 20, '2021' = 21), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = 30, '2021' = 31), url = 'http://c.gov')
    aggregate.to.msa(dm, 'diagnoses', skip.coverage.condition = T, years = '2021')

    msa = pull.msa(dm, 'diagnoses')
    check("only the requested year is put", dimnames(msa)$year, '2021')
    check("without the coverage condition, the available counties are summed", msa['2021', 'cdc.msa'], 52)
    check("url comes from the counties with 2021 data only", attr(msa, 'url')['2021', 'cdc.msa'], 'http://b.gov | http://c.gov')

    #-- 5. 'years' with the coverage condition --#
    cat("5. years with the coverage condition\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 10, '2021' = 11), url = 'http://a.gov')
    put.county(dm, 'diagnoses', B, c('2020' = 20, '2021' = 21), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = 30, '2021' = 31), url = 'http://c.gov')
    aggregate.to.msa(dm, 'diagnoses', years = '2020')
    msa = pull.msa(dm, 'diagnoses')
    check("only the requested year is put", dimnames(msa)$year, '2020')
    check("the requested year is summed", msa['2020', 'cdc.msa'], 60)

    #-- 6. Proportions: weighted by the denominator; counties without a denominator add neither data nor metadata --#
    cat("6. Proportions\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnosed.prevalence', A, c('2020' = 100), url = 'http://prev-a.gov')
    put.county(dm, 'diagnosed.prevalence', B, c('2020' = 300), url = 'http://prev-b.gov')
    # C has no denominator
    put.county(dm, 'suppression', A, c('2020' = 0.5), url = 'http://supp-a.gov', author = 'Smith')
    put.county(dm, 'suppression', B, c('2020' = 0.8), url = 'http://supp-b.gov', author = 'Jones')
    put.county(dm, 'suppression', C, c('2020' = 0.2), url = 'http://supp-c.gov', author = 'Lee')
    aggregate.to.msa(dm, 'suppression')

    msa = pull.msa(dm, 'suppression')
    check("proportion is the denominator-weighted average of the counties with both", msa['2020', 'cdc.msa'], (0.5*100 + 0.8*300) / 400)
    check("url comes from the proportion data of counties with a denominator", attr(msa, 'url')['2020', 'cdc.msa'],
          'http://supp-a.gov | http://supp-b.gov')
    check("author likewise", attr(msa, 'author')['2020', 'cdc.msa'], 'Jones | Smith')
    check("details are details.for.new.data", attr(msa, 'details')['2020', 'cdc.msa'], DETAILS.FOR.NEW.DATA)

    #-- 7. Proportions: not enough counties with a denominator --#
    cat("7. Proportions without enough denominator coverage\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnosed.prevalence', B, c('2020' = 300), url = 'http://prev-b.gov')  # B is only 25%
    put.county(dm, 'suppression', A, c('2020' = 0.5), url = 'http://supp-a.gov')
    put.county(dm, 'suppression', B, c('2020' = 0.8), url = 'http://supp-b.gov')
    aggregate.to.msa(dm, 'suppression')
    stratum = msa.stratum(dm, 'suppression')
    check("no MSA proportion is put", is.null(stratum) || all(is.na(stratum)), TRUE)

    #-- 8. Stratified data: each stratum is aggregated, and dropped strata add no metadata --#
    cat("8. Stratified data\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 1), url = 'http://a-male.gov', sex = 'male')
    put.county(dm, 'diagnoses', B, c('2020' = 2), url = 'http://b-male.gov', sex = 'male')
    # Female: only B and C (30%), so the female stratum is dropped
    put.county(dm, 'diagnoses', B, c('2020' = 3), url = 'http://b-female.gov', sex = 'female')
    put.county(dm, 'diagnoses', C, c('2020' = 4), url = 'http://c-female.gov', sex = 'female')
    aggregate.to.msa(dm, 'diagnoses')

    stored = msa.stratum(dm, 'diagnoses', 'year__location__sex')
    check("male stratum is summed", stored['2020', MSA.1, 'male'], 3)
    check("female stratum, without enough coverage, is NA", stored['2020', MSA.1, 'female'], NA_real_)
    msa = pull.msa(dm, 'diagnoses', keep.dimensions = c('year', 'sex'))
    check("male url comes from the male data", attr(msa, 'url')['2020', 'male', 'cdc.msa'], 'http://a-male.gov | http://b-male.gov')

    #-- 9. Several MSAs: each gets only its own counties' data and metadata --#
    cat("9. Several MSAs\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 10), url = 'http://a.gov')
    put.county(dm, 'diagnoses', B, c('2020' = 20), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = 30), url = 'http://c.gov')
    put.county(dm, 'diagnoses', D, c('2020' = 40), url = 'http://d.gov')
    put.county(dm, 'diagnoses', E, c('2020' = 50), url = 'http://e.gov')
    aggregate.to.msa(dm, 'diagnoses', to.locations = c(MSA.1, MSA.2))

    msa.1 = pull.msa(dm, 'diagnoses', location = MSA.1)
    msa.2 = pull.msa(dm, 'diagnoses', location = MSA.2)
    check("MSA 1 sum", msa.1['2020', 'cdc.msa'], 60)
    check("MSA 2 sum", msa.2['2020', 'cdc.msa'], 90)
    check("MSA 1 url", attr(msa.1, 'url')['2020', 'cdc.msa'], 'http://a.gov | http://b.gov | http://c.gov')
    check("MSA 2 url", attr(msa.2, 'url')['2020', 'cdc.msa'], 'http://d.gov | http://e.gov')

    #-- 10. A required metadata field is carried over from the counties, so the put succeeds --#
    cat("10. Required metadata field\n")
    dm = make.test.data.manager(author.required = T)
    put.county(dm, 'diagnoses', A, c('2020' = 10), url = 'http://a.gov', author = 'Smith')
    put.county(dm, 'diagnoses', B, c('2020' = 20), url = 'http://b.gov', author = 'Jones')
    aggregate.to.msa(dm, 'diagnoses')
    msa = pull.msa(dm, 'diagnoses')
    check("aggregated with the required field", msa['2020', 'cdc.msa'], 30)
    check("required field merged", attr(msa, 'author')['2020', 'cdc.msa'], 'Jones | Smith')

    #-- 11. Removed county points (NA put over earlier data) add no metadata --#
    cat("11. Removed county points\n")
    dm = make.test.data.manager()
    put.county(dm, 'diagnoses', A, c('2020' = 10), url = 'http://a.gov')
    put.county(dm, 'diagnoses', B, c('2020' = 20), url = 'http://b.gov')
    put.county(dm, 'diagnoses', C, c('2020' = 30), url = 'http://c.gov')
    dm$put(array(NA_real_, c(1, 1), list(location = C, year = '2020')), outcome = 'diagnoses', source = 'cdc.county',
           ontology.name = 'test.ont', url = 'http://removal.gov', details = 'removed', allow.na.to.overwrite = T)
    aggregate.to.msa(dm, 'diagnoses')
    msa = pull.msa(dm, 'diagnoses')
    check("the removed county is not summed", msa['2020', 'cdc.msa'], 30)
    check("neither the removed county's url nor the removal's url is used", attr(msa, 'url')['2020', 'cdc.msa'],
          'http://a.gov | http://b.gov')

    cat("All put.msa.data.as.new.source.NEW tests passed\n")

}, finally = set.get.contained.locations(real.get.contained.locations))
