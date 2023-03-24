
##-- READING IN THE MAPPINGS --##

create.locale.mappings <- function(dir='cleaned_data/mappings')
{
    rv = list()

    #read fips
    fips = read.csv(paste0(dir, '/fips_codes.csv'), stringsAsFactors = F)

    #states
    state.fips = fips[fips$State.Code..FIPS.>0 & fips$County.Code..FIPS.==0 & fips$Place.Code..FIPS.==0 & fips$Consolidtated.City.Code..FIPS.==0,]
    states = read.csv(paste0(dir, '/us_state_abbreviations.csv'), stringsAsFactors = F, header = F)
    state.abbrev = states[,2]
    names(state.abbrev) = states[,1]

    rv$states = data.frame(name=state.fips$Area.Name..including.legal.statistical.area.description.,
                           state_fips=state.fips$State.Code..FIPS.,
                           stringsAsFactors = F)
    rv$states$abbreviation = state.abbrev[rv$states$name]
    dimnames(rv$states)[[1]] = as.character(rv$states$state_fips)

    #counties
    county.fips = fips[fips$County.Code..FIPS.>0 & fips$County.Subdivision.Code..FIPS.==0,]
    rv$counties = data.frame(name=county.fips$Area.Name..including.legal.statistical.area.description.,
                     state_fips=county.fips$State.Code..FIPS.,
                     county_fips=county.fips$County.Code..FIPS.,
                     stringsAsFactors = F)
    rv$counties$combined_fips = combined.fips(rv$counties$state_fips, rv$counties$county_fips)
    rv$counties$state_name = rv$states[as.character(rv$counties$state_fips), 'name']
    rv$counties$state_abbreviation = rv$states[as.character(rv$counties$state_fips), 'abbreviation']
    dimnames(rv$counties)[[1]] = rv$counties$combined_fips

    rural.urban = read.csv(paste0(dir,'/Rural_Urban_Codes.csv'))
    rural.urban$FIPS = fix.census.fips(rural.urban$FIPS)
    rv$rucc.levels = trimws(sapply(1:max(rural.urban$RUCC_2013), function(num){as.character(rural.urban$Description[rural.urban$RUCC_2013==num][1])}))
    rucc.codes = rural.urban$RUCC_2013
    names(rucc.codes) = format.combined.fips(rural.urban$FIPS)
    rv$counties$RUCC_2013 = rucc.codes[rv$counties$combined_fips]

    rv$counties = add.lat.long(rv$counties,
                               code.field = 'combined_fips',
                               dir=dir,
                               location.filename='2021_Gaz_counties_national.txt')
    
    #msas
    cbsas = read.csv(paste0(dir, '/cbsas.csv'), stringsAsFactors = F)
    cbsas.for.msas = cbsas[cbsas$Metropolitan.Micropolitan.Statistical.Area=='Metropolitan Statistical Area',]

    rv$msas = data.frame(cbsa=cbsas.for.msas$CBSA.Code,
                   name=cbsas.for.msas$CBSA.Title,
                   county_fips=cbsas.for.msas$FIPS.County.Code,
                   state_fips=cbsas.for.msas$FIPS.State.Code,
                   stringsAsFactors = F,
                   division_code=cbsas.for.msas$Metropolitan.Division.Code,
                   division_name=cbsas.for.msas$Metropolitan.Division.Title)
    rv$msas$combined_fips = combined.fips(rv$msas$state_fips, rv$msas$county_fips)
    dimnames(rv$msas)[[1]] = rv$msas$combined_fips
    
    rv$msas = add.lat.long(df=rv$msas,
                           code.field = 'cbsa',
                           dir=dir,
                           location.filename='2021_Gaz_cbsa_national.txt')

    #msa to state
    msas = unique(rv$msas[,c('cbsa','name')])
    rv$unique.msas = msas
    states = gsub('.*, ', '', msas[,2])
    states = strsplit(states, '-')
    rv$msas.to.state = data.frame(cbsa=unlist(sapply(1:length(states), function(i){rep(msas[i,1], length(states[[i]]))})),
                                  msa_name = unlist(sapply(1:length(states), function(i){rep(msas[i,2], length(states[[i]]))})),
                                  state_abbreviation=unlist(states),
                                  stringsAsFactors = F)
    rv$msas.to.state$state_fips = sapply(rv$msas.to.state$state_abbreviation, function(abbrev){rv$states$state_fips[rv$states$abbreviation==abbrev][1]})
    rv$msas.to.state$state_name = rv$states[as.character(rv$msas.to.state$state_fips), 'name']

    #zip codes
    zip_codes = read.csv(paste0(dir, '/zip_codes.csv'))
    rv$zip_codes = zip_codes[,-1]
    names(rv$zip_codes)[6] = 'combined_fips'
    rv$zip_codes$combined_fips = format.combined.fips(rv$zip_codes$combined_fips)
    rv$zip_codes$zip = sprintf("%05d", rv$zip_codes$zip)
    rv$zip_codes$zip3 = substr(rv$zip_codes$zip, 1, 3)
    dimnames(rv$zip_codes)[[1]] = rv$zip_codes$zip

    #peel out anything that doesn't map to a county
    invalid.county = is.na(county.names(rv$zip_codes$combined_fips, rv))
    rv$zip_codes = rv$zip_codes[!invalid.county,]

    #nsduh substate
    
    print('Reading in Substate-Region-to-County Mapping')
    keep.cols = c('sbst16n','sbst16','sbsta16n', 'sbstag16', 'county','state')
    mapping.44 = read.sas7bdat('../data2/IDU/substate_county141516.sas7bdat')
    mapping.44 = mapping.44[,keep.cols]
    mapping.7 = read.sas7bdat('../data2/IDU/substate_tract141516.sas7bdat')
    mapping.7 = mapping.7[,keep.cols]
    mapping.7 = unique(mapping.7)
    
    nsduh.substate.regions = rbind(mapping.44, mapping.7)
    nsduh.substate.regions$combined.fips = combined.fips(nsduh.substate.regions$state, nsduh.substate.regions$county)
    nsduh.substate.regions$state.abbreviation = state.fips.to.abbreviation(nsduh.substate.regions$state)
    
    nsduh.substate.regions$region.code = nsduh.substate.regions$sbst16
    nsduh.substate.regions$region.name = nsduh.substate.regions$sbst16n
    
    nsduh.substate.regions$region.name = as.character(nsduh.substate.regions$region.name)
    
    nsduh.substate.regions$substate.code = combined.fips(nsduh.substate.regions$state, nsduh.substate.regions$region.code)
    
    
    rv$nsduh.substate.regions = nsduh.substate.regions
    
    #return the value
    rv
}

add.lat.long <- function(df, 
                         code.field = 'cbsa',
                         dir='cleaned_data/mappings',
                         location.filename='2021_Gaz_cbsa_national.txt')
{
    loc.df = read.table(file.path(dir, location.filename), sep='\t', header = T, quote="\"")
    lat.long = sapply(df[,code.field], function(code){
        mask = loc.df$GEOID==code
        if (any(mask))
        {
            c(latitude = loc.df$INTPTLAT[mask][1],
              longitude = loc.df$INTPTLONG[mask][1])
        }
        else
            c(latitude=NaN,
              longitude=NaN)
    })
    
    df$latitude = lat.long[1,]
    df$longitude = lat.long[2,]
    
    df
}

#fixes Wade Hampton Census Area -> Kusilvak Census Area
# and Shannon County -> Oglala Lakota County
fix.census.fips <- function(fips)
{
    fips = format.combined.fips(fips)
    fips[fips=='2270'] = 2158
    fips[fips=='46113'] = 46102

    fips
}

combined.fips <- function(state.fips, county.fips)
{
    paste0(sprintf("%01d", as.numeric(state.fips)),
           sprintf("%03d", as.numeric(county.fips)))
}

format.combined.fips <- function(codes)
{
    sprintf("%04d", as.numeric(codes))
}

format.zip3 <- function(zip3s)
{
    sprintf("%03d", as.numeric(zip3s))
}

##-----------------------------------------##
##-- GENERAL GETTERS FOR ALL RESOLUTIONS --##
##-----------------------------------------##

get.location.latitude.longitude <- function(location.codes,
                                   mappings=DEFAULT.LOCALE.MAPPING)
{
    location.codes = as.character(location.codes)
    lat.long = sapply(location.codes, function(code){
        
        state.mask = mappings$states$state_fips==code
        if (any(state.mask))
        {
            c(latitude=mappings$states$latitude[state.mask][1],
              longitude=mappings$states$longitude[state.mask][1])
        }
        else
        {
            msa.mask = mappings$msas$cbsa==code
            if (any(msa.mask))
            {
                c(latitude=mappings$msas$latitude[msa.mask][1],
                  longitude=mappings$msas$longitude[msa.mask][1])
            }
            else
            {
                county.mask = mappings$counties$combined_fips==code
                if (any(county.mask))
                {
                    c(latitude=mappings$counties$latitude[county.mask][1],
                      longitude=mappings$counties$longitude[county.mask][1]) 
                }
                else
                {
                    c(latitude=NaN,
                      longitude=NaN)
                }
            }
        }
    })
    
    rv = list(
        latitude=lat.long[1,],
        longitude=lat.long[2,]
    )
    names(rv$latitude) = location.codes
    names(rv$longitude) = location.codes
    
    rv
}

get.location.name <- function(location.codes,
                              mappings=DEFAULT.LOCALE.MAPPING,
                              use.state.abbreviations=F,
                              return.code.if.missing.name=T)
{
    location.codes = as.character(location.codes)
    
    if (use.state.abbreviations)
    {
        rv = state.fips.to.abbreviation(location.codes, mappings=mappings)
        rv[is.na(rv)] = state.name.to.abbreviation(location.codes[is.na(rv)], mappings=mappings)
    }
    else
    {
        rv = state.fips.to.name(location.codes, mappings=mappings)
        rv[is.na(rv)] = state.fips.to.name(state.abbreviation.to.fips(location.codes[is.na(rv)], mappings=mappings))
    }
    
    rv[is.na(rv)] = unlist(msa.names(location.codes[is.na(rv)], mappings=mappings))
    
    rv[is.na(rv)] = county.names(location.codes[is.na(rv)], mappings=mappings)
    
    if (return.code.if.missing.name)
        rv[is.na(rv)] = location.codes[is.na(rv)]
    
    rv
}

##---------------------------------##
##-- ACTUALLY DOING THE MAPPINGS --##
##---------------------------------##


##-- NAMES --##

#A character vector with one name per combined.fips
county.names <- function(combined.fips,
                         mappings=DEFAULT.LOCALE.MAPPING,
                         include.state.abbreviation=T,
                         include.state.name=F)
{
    combined.fips = format.combined.fips(combined.fips)

    rv = mappings$counties[combined.fips, 'name']
    if (include.state.abbreviation)
        rv[!is.na(rv)] = paste0(rv[!is.na(rv)], ', ', mappings$counties[combined.fips[!is.na(rv)], 'state_abbreviation'])
    else if (include.state.name)
        rv[!is.na(rv)] = paste0(rv[!is.na(rv)], ', ', mappings$counties[combined.fips[!is.na(rv)], 'state_name'])

    rv
}


msa.names <- function(cbsas.or.division.codes,
                      mappings=DEFAULT.LOCALE.MAPPING,
                      use.division.names=T)
{
    cbsas.or.division.codes = as.character(cbsas.or.division.codes)
    rv = sapply(cbsas.or.division.codes, function(cbsa){
        mappings$msas$name[mappings$msas$cbsa==cbsa][1]
    })

    if (use.division.names)
        rv[is.na(rv)] = msa.division.names(cbsas.or.division.codes[is.na(rv)], mappings)
    else
        rv[is.na(rv)] = sapply(cbsas.or.division.codes[is.na(rv)], function(code){
            mappings$msas$name[!is.na(mappings$msas$division_code) & mappings$msas$division_code==code][1]
        })

    rv
}

msa.for.division <- function(division.codes,
                             mappings=DEFAULT.LOCALE.MAPPING)
{
    division.codes = as.character(division.codes)
    sapply(as.character(division.codes), function(code){
        mappings$msas$cbsa[!is.na(mappings$msas$division_code) & mappings$msas$division_code==code][1]
    })
}

msa.division.names <- function(division.codes,
                              mappings=DEFAULT.LOCALE.MAPPING)
{
    division.codes = as.character(division.codes)
    sapply(as.character(division.codes), function(code){
        mappings$msas$division_name[!is.na(mappings$msas$division_code) & mappings$msas$division_code==code][1]
    })
}


county.fips <- function(names, mappings=DEFAULT.LOCALE.MAPPING, states=NULL)
{
    sub.from = c(' county', ' municipio', 'st.', 'saint')
    sub.to = c('', '', 'saint', 'st.')

    specific.matches.from = list(c('washington dc', 'washington d.c.', 'washington, dc', 'washington, d.c.'))
    specific.matches.to = c(11001)

    if (is.null(states)) #pull states from the county name
    {
        names.for.error = names
        splits = strsplit(tolower(names), ',')
        parsed.names = trimws(sapply(splits, function(ss){ss[1]}))
        states = trimws(sapply(splits, function(ss){ss[2]}))
    }
    else
    {
        names.for.error = paste0(names, ', ', states)
        states = tolower(states)
        parsed.names = trimws(tolower(names))
    }

    county.names = tolower(mappings$counties$name)
    state.abbrevs = tolower(mappings$counties$state_abbreviation)
    state.names = tolower(mappings$counties$state_name)

    sapply(1:length(names), function(i){

        min.matches = Inf
        max.matches = 0

        for (k in 0:length(sub.from))
        {
            if (k > 0)
            {
                to.match.county = gsub(sub.from[k], sub.to[k], county.names)
                to.match.name = gsub(sub.from[k], sub.to[k], parsed.names[i])
            }
            else
            {
                to.match.county = county.names
                to.match.name = parsed.names[i]
            }

            if (is.na(states[i]))
                mask = to.match.county == to.match.name
            else
                mask = to.match.county == parsed.names[i] & (state.abbrevs==states[i] | state.names==states[i])

            num.matches = sum(mask)
            if (num.matches == 1)
                return (mappings$counties$combined_fips[mask])

            min.matches = min(min.matches, num.matches)
            max.matches = max(max.matches, num.matches)
        }

        for (k in 1:length(specific.matches.from))
        {
            if (any(sapply(specific.matches.from[[k]], function(x){grepl(x,parsed.names[i])})))
                return (specific.matches.to[k])
        }

        if (max.matches == 0)
            stop(paste0("There are no matches for '", names.for.error[i], "'"))
        else
            stop(paste0("With ", length(sub.from)+1, " parsings, there are at least ", min.matches, " matches for '", names.for.error[i], "'"))
    })
}

##-- COUNTY to MSA --##

#returns a character vector of combined fips codes
# representing the set of counties that is in ANY of the
# MSAs represented by the given cbsa codes
counties.for.msa <- function(cbsas,
                             mappings=DEFAULT.LOCALE.MAPPING)
{
    cbsas = as.character(cbsas)
    mask = sapply(mappings$msas$cbsa, function(code){any(code==cbsas)})
    mappings$msas$combined_fips[mask]
}

counties.for.msa.or.division <- function(cbsas.or.division.code,
                                         mappings=DEFAULT.LOCALE.MAPPING)
{
    cbsas.or.division.code = as.character(cbsas.or.division.code)
    mask = sapply(mappings$msas$cbsa, function(code){any(code==cbsas.or.division.code)}) |
        sapply(mappings$msas$division_code, function(code){!is.na(code) && any(code==cbsas.or.division.code)})
    mappings$msas$combined_fips[mask]
}

#returns a character vector of CBSA codes
# same length as the combined.fips parameter
msa.for.county <- function(combined.fips,
                           mappings=DEFAULT.LOCALE.MAPPING)
{
    combined.fips = format.combined.fips(combined.fips)
    mappings$msas[combined.fips, 'cbsa']
}

#If the fips falls within a division of an MSA, return the division code
#If not, return the CBSA for the MSA
#If does not fall in an MSA, return NA
msa.or.division.for.county <- function(combined.fips,
                                       mappings=DEFAULT.LOCALE.MAPPING)
{
    combined.fips = format.combined.fips(combined.fips)
    rv = mappings$msas[combined.fips, 'division_code']
    rv[is.na(rv)] = msa.for.county(combined.fips[is.na(rv)], mappings)
    rv
}

#returns a character vector of combined fips codes
# representing the set of counties that is in ANY of the
# MSAs represented by the given states
#states can be state fips, abbreviation, or name
counties.for.state <- function(states,
                               mappings=DEFAULT.LOCALE.MAPPING)
{
    states = tolower(states)
    mask = sapply(as.character(mappings$counties$state_fips), function(fips){
        any(states==fips)
    }) |
    sapply(tolower(mappings$counties$state_name), function(st){
        any(states==st)
    }) |
    sapply(tolower(mappings$counties$state_abbreviation), function(st){
        any(states==st)
    })

    mappings$counties$combined_fips[mask]
}

#returns a character vector of state fip codes
# same length as the combined.fips parameter
state.for.county <- function(combined.fips,
                             mappings=DEFAULT.LOCALE.MAPPING,
                             return.state.abbreviation=T)
{
    combined.fips = format.combined.fips(combined.fips)
    if (return.state.abbreviation)
        mappings$counties[combined.fips, 'state_abbreviation']
    else
        mappings$counties[combined.fips, 'state_fips']
}

#states can be any mixture of state fips, state names, or state abbreviations
convert.to.state.abbreviation <- function(states,
                                          mappings=DEFAULT.LOCALE.MAPPING)
{
    abbreviation.mask = sapply(states, function(st){any(!is.na(st) & st==mappings$states$abbreviation)})
    fips.mask = sapply(states, function(st){any(!is.na(st) & st==mappings$states$state_fips)})

    rv = character(length(states))
    rv = rep(NA, length(rv))

    if (any(abbreviation.mask))
        rv[abbreviation.mask] = as.character(states[abbreviation.mask])
    if (any(fips.mask))
        rv[fips.mask] = mappings$states[as.character(states[fips.mask]),'abbreviation']
    if (any(!abbreviation.mask & !fips.mask))
        rv[!abbreviation.mask & !fips.mask] = state.name.to.abbreviation(states[!abbreviation.mask & !fips.mask], mappings)

    rv
}

county.urbanization <- function(combined.fips,
                                mappings=DEFAULT.LOCALE.MAPPING,
                                return.as.text=T)
{
    combined.fips = format.combined.fips(combined.fips)
    rv = mappings$counties[combined.fips, 'RUCC_2013']
    if (return.as.text)
        mappings$rucc.levels[rv]
    else
        rv
}

URBANIZATION.TO.LARGE.SMALL.NON.METRO = c('large','large','small','non','non','non','non','non','non')

##-- MSAs BY NAME (with flex) --##

#If no exact match,
# matches if the first city and any state in the given names match to a known MSA name
cbsa.for.msa.name <- function(msa.names, allow.wrong.state=F, mappings=DEFAULT.LOCALE.MAPPING)
{
    msa.names = gsub('/', '-', msa.names)

    rv = sapply(msa.names, function(one.name){
        mask = tolower(one.name) == tolower(mappings$unique.msas$name)
        if (any(mask))
            mappings$unique.msas$cbsa[mask][1]
        else
            NA
    })

    missing = is.na(rv)
    if (any(missing))
    {
        missing.first.city = get.msa.name.first.city(msa.names[missing])
        missing.states = get.msa.name.states(msa.names[missing], split=T)
        msa.states = get.msa.name.states(mappings$unique.msas$name, split=T)
        missing.states.raw = get.msa.name.states(msa.names[missing], split=T, return.raw.names = T)
        msa.states.raw = get.msa.name.states(mappings$unique.msas$name, split=T, return.raw.names = T)

        rv[missing] = sapply(1:sum(missing), function(i){
            mask = grepl(missing.first.city[i], mappings$unique.msas$name, ignore.case = T) &
                sapply(msa.states, function(states.for.msa){length(intersect(states.for.msa, missing.states[[i]]))>0})
            if (any(mask))
                mappings$unique.msas$cbsa[mask][1]
            else
            {
                mask = grepl(missing.first.city[i], mappings$unique.msas$name, ignore.case = T) &
                        grepl(paste0('^', missing.states.raw[i]), msa.states.raw, ignore.case = T)

                if (!any(mask) && !is.na(missing.first.city[i]) && grepl('city', missing.first.city[i], ignore.case = T))
                {
                    city = trimws(gsub('city', '', missing.first.city[i], ignore.case = T))
                    mask = grepl(city, mappings$unique.msas$name, ignore.case = T) &
                        grepl(paste0('^', missing.states.raw[i]), msa.states.raw, ignore.case = T)
                }

                if (any(mask))
                    mappings$unique.msas$cbsa[mask][1]
                else if (allow.wrong.state)
                {
                    mask = !is.na(missing.first.city[i]) & grepl(missing.first.city[i], mappings$unique.msas$name, ignore.case = T)

                    if (any(mask))
                        mappings$unique.msas$cbsa[mask][1]
                    else if (!is.na(missing.first.city[i]) && grepl('city', missing.first.city[i], ignore.case = T))
                    {
                        city = trimws(gsub('city', '', missing.first.city[i], ignore.case = T))
                        mask = grepl(city, mappings$unique.msas$name, ignore.case = T)
                        if (any(mask))
                            mappings$unique.msas$cbsa[mask][1]
                        else
                            NA
                    }
                    else
                        NA
                }
                else
                    NA
            }
        })

        missing = is.na(rv)
        if (any(missing))
        {
            missing.first.city = get.msa.name.first.city(msa.names[missing])
            missing.first.city = gsub(' city', '', missing.first.city, ignore.case = T)
            missing.states = get.msa.name.states(msa.names[missing], split=T)
            msa.states = get.msa.name.states(mappings$unique.msas$name, split=T)

            rv[missing] = sapply(1:sum(missing), function(i){
                mask = grepl(missing.first.city[i], mappings$unique.msas$name) &
                    sapply(msa.states, function(states.for.msa){length(intersect(states.for.msa, missing.states[[i]]))>0})
                if (any(mask))
                    mappings$unique.msas$cbsa[mask][1]
                else
                    NA
            })
        }

        #specific checks
        missing = is.na(rv)
        if (any(missing))
        {
            st.louis.mask = grepl('saint louis', msa.names[missing], ignore.case = T)
            rv[missing][st.louis.mask] = cbsa.for.msa.name('St. Louis, MO-IL')
        }
    }


    rv
}

get.msa.name.first.city <- function(msa.names)
{
    matches.1 = grepl(MSA.REGEX.1, msa.names)
    matches.2 = !matches.1 & grepl(MSA.REGEX.2, msa.names)

    str.matches = rep(NA, length(msa.names))
    str.matches[matches.1] = gsub(MSA.REGEX.1, '\\1', msa.names[matches.1])
    str.matches[matches.2] = gsub(MSA.REGEX.2, '\\1', msa.names[matches.2])

#    rv[matches.1] = gsub('(,? +|, *)[A-Z|a-z|-]+$', '', msa.names[matches.1])
#    rv = gsub('[-|(].*$', '', rv)

    rv = sapply(strsplit(str.matches, '[-\\(]'), function(elem){elem[[1]]})
    rv = trimws(rv)
    rv
}

MSA.REGEX.1 = '^([^,]*)(,? +|, *)([a-z]|[A-Z|-|\\.]+)$'
MSA.REGEX.2 = '^([^,]*),([a-zA-Z -\\.]+)$'
get.msa.name.states <- function(msa.names, split=T, return.raw.names=F,
                                mappings=DEFAULT.LOCALE.MAPPING)
{
    matches.1 = grepl(MSA.REGEX.1, msa.names)
    matches.2 = !matches.1 & grepl(MSA.REGEX.2, msa.names)

    rv = rep(NA, length(msa.names))
    rv[matches.1] = gsub(MSA.REGEX.1, '\\3', msa.names[matches.1])
    rv[matches.2] = gsub(MSA.REGEX.2, '\\2', msa.names[matches.2])

    if (return.raw.names)
        return (rv)

    split.states = strsplit(rv, '-')
    split.states = lapply(split.states, trimws)
    split.states = lapply(split.states, robust.match.state.name, mappings=mappings)

    if (split)
        split.states
    else
        sapply(split.states, function(states){paste0(states, collapse='-')})
}

msa.division.code.for.name <- function(division.names,
                                       cbsas,
                                       mappings=DEFAULT.LOCALE.MAPPING)
{
    rv = sapply(1:length(division.names), function(i){
        cbsa.mask = mappings$msas$cbsa == as.character(cbsas[i])
        name.mask = grepl(division.names[i], mappings$msas$division_name[cbsa.mask], ignore.case = T)

        mask = cbsa.mask
        mask[mask] = name.mask

        if (any(mask))
            mappings$msas$division_code[mask][1]
        else if (tolower(division.names[i]) == 'silver spring' && cbsas[i]==47900)
            23224
        else
        {
            first.city = strsplit(division.names[i], '-')[[1]][1]
            first.name.mask = grepl(first.city, mappings$msas$division_name[cbsa.mask], ignore.case = T)

            mask = cbsa.mask
            mask[mask] = first.name.mask

            if (any(mask))
                mappings$msas$division_code[mask][1]
            else
                NA
        }
    })


    rv
}

division.to.msa <- function(division.codes, mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(as.integer(division.codes), function(code){
        if (any(mappings$msas$cbsa==as.character(code)))
            code
        else
        {
            mask = !is.na(mappings$msas$division_code) & mappings$msas$division_code==code
            if (any(mask))
                as.character(mappings$msas$cbsa[mask][1])
            else
                NA
        }
    })
}


##-- MSA to STATE and BACK --##

msas.for.state <- function(states, mappings=DEFAULT.LOCALE.MAPPING)
{
    states = tolower(states)
    mask = sapply(as.character(mappings$msas.to.state$state_fips), function(fips){
        any(states==fips)
    }) |
    sapply(tolower(mappings$msas.to.state$state_name), function(st){
        any(states==st)
    }) |
    sapply(tolower(mappings$msas.to.state$state_abbreviation), function(st){
        any(states==st)
    })

    mappings$msas.to.state$cbsa[mask]
}

states.for.msa <- function(cbsas.or.division, mappings=DEFAULT.LOCALE.MAPPING,
                          return.state.abbreviations=T)
{
    cbsas.or.division = as.character(cbsas.or.division)
    rv = unique(unlist(sapply(cbsas.or.division, function(cbsa){
        mappings$msas.to.state$state_fips[mappings$msas.to.state$cbsa==cbsa]
    })))

    if (return.state.abbreviations)
        mappings$states[as.character(rv), 'abbreviation']
    else
        rv
}

zips.for.county <- function(combined.fips,
                           mappings=DEFAULT.LOCALE.MAPPING)
{
    combined.fips = format.combined.fips(combined.fips)
    mask = sapply(mappings$zip_codes$combined_fips, function(fips){any(fips==combined.fips)})
    mappings$zip_codes$zip[mask]
}

counties.for.zip <- function(zips,
                           mappings=DEFAULT.LOCALE.MAPPING)
{
    zips = as.character(zips)
    mappings$zip_codes[zips, 'combined_fips']
}

zip3s.for.county <- function(combined.fips,
                             mappings=DEFAULT.LOCALE.MAPPING)
{
    combined.fips = format.combined.fips(combined.fips)
    mask = sapply(mappings$zip_codes$combined_fips, function(fips){any(fips==combined.fips)})
    unique(mappings$zip_codes$zip3[mask])
}

counties.for.zip3 <- function(zip3,
                            mappings=DEFAULT.LOCALE.MAPPING)
{
    zip3 = format.zip3(zip3)
    unique(unlist(lapply(zip3, function(zip){
        mask = zip == mappings$zip_codes$zip3
        if (any(mask))
            mappings$zip_codes$combined_fips[mask]
        else
            character()
    })))
}

state.name.to.abbreviation <- function(state.names, mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(tolower(state.names), function(st){
        mappings$states$abbreviation[tolower(mappings$states$name)==st][1]
    })
}

state.fips.to.abbreviation <- function(state.fips, mappings=DEFAULT.LOCALE.MAPPING)
{
    mappings$states[as.character(state.fips), 'abbreviation']
}

state.fips.to.name <- function(state.fips, mappings=DEFAULT.LOCALE.MAPPING)
{
    mappings$states[as.character(state.fips), 'name']
}

state.abbreviation.to.fips <- function(state.abbreviations, mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(toupper(state.abbreviations), function(st){
        mappings$states$state_fips[mappings$states$abbreviation==st][1]
    })
}

robust.match.state.name <- function(state.names, return.na.if.no.match = F,
                                    mappings=DEFAULT.LOCALE.MAPPING)
{
    #strip out periods, uppercase it all
    rv = orig.stripped = gsub('\\.', '', toupper(state.names))

    cannot.match = is.na(state.names) | nchar(state.names)==1

#    name.match = state.name.to.abbreviation(state.names)
#    rv[!is.na(name.match)] = name.match[!is.na(name.match)]

    first.pass.match.mask = !is.na(state.abbreviation.to.fips(rv))
    unmatched = rv[!first.pass.match.mask]

    rv[!first.pass.match.mask] = sapply(unmatched, function(st){
        regex = paste0("^", st, ".*$")
        mask = grepl(regex, mappings$states$name, ignore.case = T)
        mappings$states$abbreviation[mask][1]
    })

    second.pass.unmatched.mask = is.na(rv)
    unmatched = orig.stripped[second.pass.unmatched.mask]

    other.matches = c(NMEX='NM',
                      'N ME'='NM',
                      FLA='FL')

    rv[second.pass.unmatched.mask] = other.matches[unmatched]


    if (return.na.if.no.match)
        rv[cannot.match] = NA
    else
    {
        rv[is.na(rv)] = state.names[is.na(rv)]
        rv[cannot.match] = state.names[cannot.match]
    }

    rv
}

state.to.region <- function(states)
{
    mapping = c(
        'Pacific'= 'West',
        'Mountain'= 'West',
        'West North Central'= 'Midwest',
        'East North Central'= 'Midwest',
        'West South Central'= 'South',
        'East South Central'= 'South',
        'South Atlantic'= 'South',
        'Mid-Atlantic'= 'Northeast',
        'New England'= 'Northeast'
    )
    
    rv = state.to.region.division(states)
    if (any(is.na(rv)))
      rv[!is.na(rv)] = mapping[rv[!is.na(rv)]]
    rv
}

state.to.region.division <- function(states)
{
    mapping = c(WA='Pacific',
      OR='Pacific',
      CA='Pacific',
      AK='Pacific',
      HI='Pacific',
      
      MT='Mountain',
      ID='Mountain',
      WY='Mountain',
      NV='Mountain',
      UT='Mountain',
      CO='Mountain',
      AZ='Mountain',
      NM='Mountain',
      
      ND='West North Central',
      MN='West North Central',
      SD='West North Central',
      IA='West North Central',
      NE='West North Central',
      KS='West North Central',
      MO='West North Central',
      
      WI='East North Central',
      MI='East North Central',
      IL='East North Central',
      IN='East North Central',
      OH='East North Central',
      
      OK='West South Central',
      TX='West South Central',
      AR='West South Central',
      LA='West South Central',
      
      KY='East South Central',
      TN='East South Central',
      MS='East South Central',
      AL='East South Central',
      
      WV='South Atlantic',
      MD='South Atlantic',
      DE='South Atlantic',
      DC='South Atlantic',
      VA='South Atlantic',
      NC='South Atlantic',
      SC='South Atlantic',
      GA='South Atlantic',
      FL='South Atlantic',
      
      NY='Mid-Atlantic',
      PA='Mid-Atlantic',
      NJ='Mid-Atlantic',
      
      ME='New England',
      NH='New England',
      VT='New England',
      MA='New England',
      CT='New England',
      RI='New England'
    )
    
    rv = mapping[as.character(states)]
    if (any(is.na(rv)))
      rv[is.na(rv)] = mapping[state.name.to.abbreviation(states[is.na(rv)])]
    
    rv
}


#-- NSDUH Substate Regions --#

counties.for.nsduh.substate.regions <- function(region.codes,
                                                mappings=DEFAULT.LOCALE.MAPPING)
{
    unlist(lapply(region.codes, function(code){
        mask = mappings$nsduh.substate.regions$substate.code == code
        if (any(mask))
            mappings$nsduh.substate.regions$combined.fips[mask]
    }))
}

states.for.nsduh.substate.regions <- function(region.codes,
                                              mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(region.codes, function(code){
        mask = mappings$nsduh.substate.regions$substate.code == code
        if (any(mask))
            mappings$nsduh.substate.regions$state.abbreviation[mask][1]
        else
            NA
    })
}

nsduh.substate.region.names <- function(region.codes,
                                        mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(region.codes, function(code){
        mask = mappings$nsduh.substate.regions$substate.code == code
        if (any(mask))
            mappings$nsduh.substate.regions$region.name[mask][1]
        else
            NA
    })
}

ndsuh.substate.regions.for.counties <- function(county.fips,
                                                mappings=DEFAULT.LOCALE.MAPPING)
{
    sapply(county.fips, function(code){
        mask = mappings$nsduh.substate.regions$combined.fips == code
        if (any(mask))
            mappings$nsduh.substate.regions$substate.code[mask]
        else
            NA
    })
}

# states should be two-letter state abbreviations
state.and.region.name.to.nsduh.region.code <- function(states,
                                                       region.names,
                                                       mappings=DEFAULT.LOCALE.MAPPING)
{
    if (length(states) == 1)
        rep(states, length(region.names))
    if (length(states) != length(region.names))
        stop("states must be either a single value, or a vector with the same length as region.names")
    
    sapply(1:length(states), function(i){
        mask = tolower(states[i]) == tolower(mappings$nsduh.substate.regions$state.abbreviation) &
            tolower(region.names[i]) == tolower(mappings$nsduh.substate.regions$region.name)
        if (any(mask))
            mappings$nsduh.substate.regions$substate.code[mask][1]
        else
            NA
    })
}

##------------------------------##
##-- READ IN DEFAULT MAPPINGS --##
##------------------------------##

#if (!exists('DEFAULT.LOCALE.MAPPING'))
#{
#    print("Reading in a default locale mapping")
#    DEFAULT.LOCALE.MAPPING = create.locale.mappings()
#}

