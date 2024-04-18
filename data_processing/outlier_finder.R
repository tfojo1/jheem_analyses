## --------HOW TO CALL--------##
# outcome: 'diagnosed.prevalence'
# data.manager: SURVEILLANCE.MANAGER
# locations: 'MSAS.OF.INTEREST'
# stratifications: something like list('sex', c('age', 'sex'), 'race') etc.
# adjudication.data.frame: an optional data frame that describes values you will say keep or don't keep on. Has columns year, location, source, ontology, and another column per additional dimension in 'stratification.dimensions'. The last column should be "adjudication" and contain 'T' for keeping a row, 'F' for retaining a row, and NA for undecided rows.
# phi: a percent change from one year to another that does not depend on how many years apart the data points are from
# theta: a multiplier that produces a percent change based on how many years apart two samples are. The maximum allowed percent change uses phi + (1 + theta)^(year difference) - 1.
# minimum.flagged.change: an integer that indicates what the smallest change between years that will be flagged as problematic is.
## ---------------------------##

# outlier remover

# takes in an adjudication data frame.
# removes what should be removed
# runs outlier finder and report back if there are any flagged points not adjudicated yet.

run.outlier.process = function(outcome, stratifications=list(), data.manager = get.default.data.manager(), locations, adjudication.data.frame=NULL, phi=0.15, theta=0.05, minimum.flagged.change=50) {
    
    # Step 1: remove outliers as requested if we have an adjudication data frame
    if (!is.null(adjudication.data.frame)) {
        remove.outliers(outcome, stratifications, data.manager, adjudication.data.frame)
    }
    
    # Step 2: run outlier finder on what remains
    output.data.frame = find.outlier.data(outcome=outcome, stratifications=stratifications, data.manager=data.manager, locations=locations, adjudication.data.frame=adjudication.data.frame, phi=phi, theta=theta, minimum.flagged.change=minimum.flagged.change)
    
    # return the output.data.frame
    return(output.data.frame)
}




#### ALL BELOW ARE HELPERS ####
remove.outliers = function(outcome, stratifications, data.manager, adjudication.data.frame) {
    
    # for every point in the list, put the value NA into the data.manager
    
    all.sources = unique(adjudication.data.frame$source)
    all.ontologies = unique(adjudication.data.frame$ontology)
    
    # remove rows with NA adjudication
    adjudication.data.frame = subset(adjudication.data.frame, !is.na(adjudication))
    
    for (stratification in stratifications) {
        
        this.adj.data.frame = adjudication.data.frame[names(adjudication.data.frame) %in% c('year', 'location', stratification, 'source', 'ontology', 'adjudication')]
        # pick only rows that are non-NA (except in the adjudication column, where it is okay to be NA)
        
        if (!is.null(this.adj.data.frame))
            this.adj.data.frame = this.adj.data.frame[apply(
                this.adj.data.frame,
                1,
                function(row) {!any(is.na(row[names(row)!='adjudication']))}
            ),]
        
        stratification.name = "year__location"
        stratification.dimensions = sort(stratification)
        if (length(stratification.dimensions)>0)
            stratification.name = paste0(stratification.name, "__", paste0(stratification.dimensions, collapse = "__"))
        
        for (source.name in all.sources) {
            
            ontologies.this.source = intersect(all.ontologies, names(data.manager$data[[outcome]][['estimate']][[source.name]]))
            
            for (ont.name in ontologies.this.source) {
                
                removal.points.this.source.ont = subset(this.adj.data.frame, source==source.name & ontology==ont.name & adjudication==T)
                
                for (i in 1:nrow(removal.points.this.source.ont)) {
                    # Must do one put per replaced data point
                    data.manager$put(data=as.numeric(NA),
                                     outcome=outcome,
                                     source=source.name,
                                     ontology.name=ont.name,
                                     dimension.values = c(list(year=removal.points.this.source.ont[[i,'year']],
                                                               location=removal.points.this.source.ont[[i,'location']]),
                                                          setNames(lapply(stratification, function(x) {removal.points.this.source.ont[[i,x]]}), stratification)),
                                     url="removed",
                                     details="removed",
                                     allow.na.to.overwrite=T)
                }
            }
        } 
    }
}

find.outlier.data = function(outcome, stratifications=list(), data.manager = get.default.data.manager(), locations, adjudication.data.frame=NULL, phi=0.15, theta=0.05, minimum.flagged.change=50) {
    # fastest way to recode this to have all stratifications in one data frame is to make this a wrapper for what I already had
    
    all.data.frames = lapply(stratifications, function(stratification) {
        this.adj.data.frame = adjudication.data.frame[names(adjudication.data.frame) %in% c('year', 'location', stratification, 'source', 'ontology', 'adjudication')]
        # pick only rows that are non-NA (except in the adjudication column, where it is okay to be NA)
        
        if (!is.null(this.adj.data.frame))
            this.adj.data.frame = this.adj.data.frame[apply(
                this.adj.data.frame,
                1,
                function(row) {!any(is.na(row[names(row)!='adjudication']))}
            ),]
        find.outlier.data.per.stratification(outcome=outcome, data.manager=data.manager, locations=locations, stratification.dimensions=stratification, adjudication.data.frame=this.adj.data.frame, phi=phi, theta=theta, minimum.flagged.change = minimum.flagged.change)
    })
    
    # Combine them into one big data frame with all the dimensions
    complete.colnames = c('year', 'location', unique(unlist(stratifications)), 'source', 'ontology', 'adjudication')
    
    all.expanded.data.frames = lapply(all.data.frames, function(this.data.frame) {
        extra.cols.needed = setdiff(complete.colnames, colnames(this.data.frame))
        if (length(extra.cols.needed)>0) {
            extra.cols = lapply(extra.cols.needed, function(x) {rep(NA, nrow(this.data.frame))})
            names(extra.cols)=extra.cols.needed
            this.expanded.data.frame = cbind(this.data.frame, extra.cols)   
        } else this.expanded.data.frame = this.data.frame
        return(this.expanded.data.frame[complete.colnames])
    })
    
    return(Reduce(rbind, all.expanded.data.frames))
}


### -- HELPERS -- ####
find.outlier.data.per.stratification = function(outcome, data.manager = get.default.data.manager(), locations, stratification.dimensions=c(), adjudication.data.frame=NULL, phi=0.15, theta=0.05, minimum.flagged.change=50) {
    
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
                        if (length(years)==0) return(NULL)
                        if (length(city.data)==0) return(NULL)
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
                    if (length(years)==0) return(NULL)
                    if (length(city.data)==0) return(NULL)
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
    
    # NEW ALGORITHM: START AT 2019. GO FORWARDS WITH ALGORITHM AND ALSO GO BACKWARDS. IF 2019 ISN'T AVAILABLE START WITH THE NEXT EARLIER YEAR, OR IF NONE AVAILABLE BELOW 2020, PICK FIRST YEAR ABOVE 2019.
    
    do.find.outliers = function(data.vector, years, adjudication.result, debug=F) {
        if (debug) browser()
        flagged.years = character(0)
        
        # Pick 2019 or earlier, else pick first year above 2019. Must be nonzero because we divide by it to find a percent change.
        if (any(as.numeric(years)<=2019 & as.numeric(years)!=0))
            baseline.year = max(as.numeric(years)[as.numeric(years)<2020 & as.numeric(years)!=0])
        else if (any(as.numeric(years)>2019 & as.numeric(years)!=0))
            baseline.year = min(as.numeric(years)[as.numeric(years)!=0])
        else return(flagged.years)
        
        backward.years = years[as.numeric(years)<baseline.year]
        forward.years = years[as.numeric(years)>baseline.year]
        
        baseline.year = as.character(baseline.year)
        
        for (direction in c('forward', 'backward')) {
            
            if (direction=='forward' && length(forward.years)==0) next
            if (direction=='backward' && length(backward.years)==0) next
            
            last.good.year = baseline.year
            if (direction=='forward') years.to.check = forward.years
            if (direction=='backward') years.to.check = rev(backward.years)
            
            # Flag years whose values are too different from the last acceptable year's value, updating the last acceptable year when the change is small enough
            for (y in years.to.check) {
                abs.difference = abs(data.vector[[y]] - data.vector[[last.good.year]])
                difference.in.years = abs(as.numeric(y)-as.numeric(last.good.year))
                tolerable.percent.difference = phi + (1 + theta) ^ difference.in.years - 1
                if (is.na(adjudication.result[y])) {
                    if (abs.difference >= minimum.flagged.change && abs.difference / data.vector[[last.good.year]] > tolerable.percent.difference)
                        flagged.years = c(flagged.years, y)
                    else
                        last.good.year = y
                }
                else if (!adjudication.result[y])
                    last.good.year = y
            }
            
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

# city.names = c('C.47900', 'C.16980')
# jj = find.outlier.data('diagnosed.prevalence', stratifications = list(c(), 'age', 'sex', c('age', 'sex')), data.manager = ss, locations =  city.names)
# # input.v = rep(c(T,F), nrow(jj)/2) # works if jj has even number of rows
# jj$adjudication[[1]] = T
# jj2 = find.outlier.data('diagnosed.prevalence', ss, city.names, stratification.dimensions = c('age', 'sex'), adjudication.data.frame = jj)
