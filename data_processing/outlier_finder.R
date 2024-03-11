## --------HOW TO CALL--------##
# outcome: 'diagnosed.prevalence'
# data.manager: SURVEILLANCE.MANAGER
# locations: 'MSAS.OF.INTEREST'
# stratification.dimensions: something like c('risk', 'sex') -- don't include 'year' or 'location', which are implied
# adjudication.data.frame: an optional data frame that describes values you will say keep or don't keep on. Has columns year, location, source, ontology, and another column per additional dimension in 'stratification.dimensions'. The last column should be "adjudication" and contain 'T' for keeping a row, 'F' for retaining a row, and NA for undecided rows.
# phi: a percent change from one year to another that does not depend on how many years apart the data points are from
# theta: a multiplier that produces a percent change based on how many years apart two samples are. The maximum allowed percent change uses phi + (1 + theta)^(year difference) - 1.
# minimum.flagged.change: an integer that indicates what the smallest change between years that will be flagged as problematic is.
## ---------------------------##

## ONLY USE THIS. THE REST ARE ITS HELPERS.
find.outlier.data = function(outcome, data.manager = get.default.data.manager(), locations, stratification.dimensions=c(), adjudication.data.frame=NULL, phi=0.15, theta=0.05, minimum.flagged.change=50) {
    
    error.prefix = "Error finding outliers: "
    
    stratification.name = "year__location"
    if (any(sapply(c('year', 'location'), function(d) {d %in% stratification.dimensions})))
        stop(paste0(error.prefix, "'stratification.dimensions' should be a vector excluding 'year' and 'location', which are assumed to be present"))
    stratification.dimensions = sort(stratification.dimensions)
    if (length(stratification.dimensions)>0)
        stratification.name = paste0(stratification.name, "__", paste0(stratification.dimensions, collapse = "__"))

    expected.col.names = c('year', 'location', 'source', 'ontology', stratification.dimensions, 'adjudication')
    if (!is.null(adjudication.data.frame) && (!is.data.frame(adjudication.data.frame) ||  !setequal(colnames(adjudication.data.frame), expected.col.names)))
        stop(paste0(error.prefix, "'adjudication.data.frame' must be either NULL or a data frame with columns year, location, source, ontology, adjudication, and one per additional dimension in 'stratification.dimensions'."))
    
    adjudication.vector = adjudication.data.frame[['adjudication']]

    flag.list = do.get.outliers.for.outcome(outcome,
                                            data.manager=data.manager,
                                            locations=locations,
                                            stratification.name=stratification.name,
                                            adj.data.frame = adjudication.data.frame,
                                            adj.vector = adjudication.vector,
                                            phi = phi,
                                            theta = theta,
                                            minimum.flagged.change=minimum.flagged.change)
    return(convert.to.data.frame(flag.list, stratification.name))
    
}




### -- HELPERS -- ####
convert.to.data.frame = function(list.of.lists, stratification.name) {
    # How long is our list of lists, flattened?
    full.length = length(unlist(list.of.lists))
    
    # Get our year, location, and ... dimensions
    stratification.dimension.names = unlist(strsplit(stratification.name, split="__"))
    stratification.dimension.columns = setNames(lapply(stratification.dimension.names, function(d) {character(full.length)}), stratification.dimension.names)

    # Initialize data frame with full size for maximum efficiency
    if (identical(stratification.name, "year__location"))
        df = data.frame(stratification.dimension.columns['year'],
                        stratification.dimension.columns['location'],
                        source=character(full.length),
                        ontology=character(full.length))
    else
        df = data.frame(stratification.dimension.columns['year'],
                        stratification.dimension.columns['location'],
                        source=character(full.length),
                        ontology=character(full.length),
                        stratification.dimension.columns[!(stratification.dimension.names %in% c('year', 'location'))])
    
    # list of sources / lists of ontologies / lists of stratifications / lists of strata / lists of locations / vector of years
    current.row = 1
    for (source.name in names(list.of.lists)) {
        for (ont.name in names(list.of.lists[[source.name]])) {
            for (stratum.name in names(list.of.lists[[source.name]][[ont.name]])) {
                # Assume our stratum names are in the same order as the stratification name... safe since they came from the same place

                if (stratum.name == "TOTALS") variable.dimension.values = NULL
                else variable.dimension.values = unlist(strsplit(stratum.name, split="__"))
                
                for (location in names(list.of.lists[[source.name]][[ont.name]][[stratum.name]])) {
                    for (year in list.of.lists[[source.name]][[ont.name]][[stratum.name]][[location]]) {
                        
                        df[current.row,] = c(year, location, source.name, ont.name, variable.dimension.values)
                        current.row = current.row + 1
                    }
                }
            }
        }
    }
    df = cbind(df, adjudication=as.logical(rep(NA, nrow(df))))
    return(df)
}

do.get.outliers.for.outcome = function(outcome, data.manager, locations, stratification.name, adj.data.frame, adj.vector, phi, theta, minimum.flagged.change) {
    
    # for now, stratification can be as written, no parsing needed, like "year__location__sex"
    do.find.outliers = generate.find.outliers.function(phi, theta, minimum.flagged.change)
    
    source.names = names(data.manager$data[[outcome]]$estimate)
    all.flagged.list = lapply(source.names, function(source.name) {
        
        ont.names = names(data.manager$data[[outcome]]$estimate[[source.name]])
        flagged.this.source = lapply(ont.names, function(ont.name) {
            
            data.this.stratification = data.manager$data[[outcome]]$estimate[[source.name]][[ont.name]][[stratification.name]]
            strata = expand.grid(dimnames(data.this.stratification)[!(names(dimnames(data.this.stratification)) %in% c('year', 'location'))], stringsAsFactors = FALSE)
            strata.names = do.call(paste, c(strata, sep="__"))
            
            if (nrow(strata) > 0) {
                flagged.this.ont = lapply(1:nrow(strata), function(stratum.row) {
                    
                    flagged.this.stratum = lapply(locations, function(city.name) {
                        if (!(city.name %in% dimnames(data.this.stratification)$location)) return(NULL)
                        dimension.values = setNames(as.list(strata[stratum.row,]), colnames(strata))
                        years = dimnames(data.this.stratification)[['year']]
                        city.data = setNames(as.vector(array.access(data.this.stratification, location=city.name, dimension.values)), years)
                        years = years[!is.na(city.data)]
                        city.data = city.data[!is.na(city.data)]
                        result = do.find.outliers(city.data,
                                                  years = years,
                                                  get.adjudication(years=years,
                                                                   location=city.name,
                                                                   source=source.name,
                                                                   ontology=ont.name,
                                                                   stratum.vars=strata[stratum.row,],
                                                                   adj.data.frame = adj.data.frame,
                                                                   adj.vector = adj.vector))
                        if (length(result)>0) return(result)
                        else return(NULL)
                        
                    })
                    cities.which.flagged = !sapply(flagged.this.stratum, is.null)
                    flagged.this.stratum = flagged.this.stratum[cities.which.flagged]
                    if (length(flagged.this.stratum)>0) return(setNames(flagged.this.stratum, locations[cities.which.flagged]))
                    else return(NULL)
                    
                })
                strata.which.flagged = !sapply(flagged.this.ont, is.null)
                flagged.this.ont = flagged.this.ont[strata.which.flagged]
                if (length(flagged.this.ont)>0) return(setNames(flagged.this.ont, strata.names[strata.which.flagged]))
                else return(NULL)
            }
            
            else {
                flagged.this.ont = lapply(locations, function(city.name) {
                    if (!(city.name %in% dimnames(data.this.stratification)$location)) return(NULL)
                    years = dimnames(data.this.stratification)[['year']]
                    city.data = setNames(as.vector(array.access(data.this.stratification, location=city.name)), years)
                    years = years[!is.na(city.data)]
                    city.data = city.data[!is.na(city.data)]
                    result = do.find.outliers(city.data,
                                              years = years,
                                              get.adjudication(years=years,
                                                               location=city.name,
                                                               source=source.name,
                                                               ontology=ont.name,
                                                               stratum.vars=character(0),
                                                               adj.data.frame = adj.data.frame,
                                                               adj.vector = adj.vector))
                    if (length(result)>0) return(result)
                    else return(NULL)
                    
                })
                cities.which.flagged = !sapply(flagged.this.ont, is.null)
                flagged.this.ont = flagged.this.ont[cities.which.flagged]
                if (length(flagged.this.ont)>0) return(list(TOTALS=setNames(flagged.this.ont, locations[cities.which.flagged])))
                else return(NULL)
            }
            
        })
        onts.which.flagged = !sapply(flagged.this.source, is.null)
        flagged.this.source = flagged.this.source[onts.which.flagged]
        if (length(flagged.this.source)>0) return(setNames(flagged.this.source, ont.names[onts.which.flagged]))
        else return(NULL)
        
    })
    sources.which.flagged = !sapply(all.flagged.list, is.null)
    all.flagged.list = all.flagged.list[sources.which.flagged]
    return(setNames(all.flagged.list, source.names[sources.which.flagged]))
}

generate.find.outliers.function = function(phi, theta, minimum.flagged.change) {
    
    do.find.outliers = function(data.vector, years, adjudication.result, debug=F) {
        if (debug) browser()
        # Check that we can find a starting point that isn't zero, because we'll need to divide by it
        last.good.year = NULL
        used.years = NULL
        for (y in years) {
            if (data.vector[[y]] != 0) {
                last.good.year = y
                used.years = years[y:length(years)]
                break
            }
        }
        if (is.null(last.good.year)) return(character(0))
        
        # Find change from the last.good.year to another
        flagged.years = character(0)
        for (y in years) {
            abs.difference = abs(data.vector[[y]] - data.vector[[last.good.year]])
            difference.in.years = as.numeric(y)-as.numeric(last.good.year)
            tolerable.percent.difference = phi + (1 + theta) ^ difference.in.years - 1
            if (is.na(adjudication.result[y])) {
                if (abs.difference >= minimum.flagged.change && abs.difference / data.vector[[last.good.year]] > tolerable.percent.difference)
                    flagged.years = c(flagged.years, y)
                else
                    last.good.year = y
            }
            else if (TRUE) {
                tryCatch(
                    {if (!adjudication.result[y]) last.good.year=y},
                    error=function(e) {browser()}
                )
            }
            else if (!adjudication.result[y])
                last.good.year = y
        }
        return (flagged.years)
    }
}

get.adjudication = function(years, location, source, ontology, stratum.vars, adj.data.frame, adj.vector) {
    
    if (is.null(adj.data.frame) || is.null(adj.vector))
        return (rep(NA, length(years)))
    
    # for each year, find the index of the combination in the data frame (it might not exist)
    result = sapply(years, function(year) {
        identifier = c(year, location, source, ontology, as.vector(unlist(stratum.vars)))
        n.var.dimensions = length(stratum.vars)
        n.fixed.dimensions = 4
        
        # Reduce just does & on a variable number of vectors, which will be one per dimension
        # if (year=='2014' &&location=='C.47900' && source=='cdc.surveillance.reports' && ontology=='cdc.msa.reports'&& stratum.vars[[2]]=='female') browser()
        match = which(Reduce(`&`, lapply(1:(n.fixed.dimensions + n.var.dimensions), function(i) {
            adj.data.frame[i] == identifier[[i]]
        })))
        if (length(match)>1)
            stop(paste0("Error: duplicate rows for the same record"))
        if (length(match)==1)
            return(adj.vector[match])
        else
            return (NA)
    })
    # if (any(!is.na(result))) browser()
    return (result)
}

city.names = c('C.47900', 'C.16980')
jj = find.outlier.data('diagnosed.prevalence', ss, city.names, stratification.dimensions = c('age', 'sex'))
# input.v = rep(c(T,F), nrow(jj)/2) # works if jj has even number of rows
jj$adjudication[[1]] = T
jj2 = find.outlier.data('diagnosed.prevalence', ss, city.names, stratification.dimensions = c('age', 'sex'), adjudication.data.frame = jj)
