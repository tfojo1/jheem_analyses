## --------HOW TO CALL--------##
# outcome: 'diagnosed.prevalence'
# data.manager: SURVEILLANCE.MANAGER
# locations: 'MSAS.OF.INTEREST'
# percent.change.per.year: what percent change should you tolerate from one year to the next?
# ignore.below: observations of what size and below should be ignored, since large percent changes are much more likely with them?
## ---------------------------##


# per outcome
# try just for diagnosed prevalence
# DC MSA surve report should fail. Agg counties should pass. Most other MSAs should pass.
# do 20% for now
# Chicago census agg counties
# do this once per city, source, and stratum. (32*2*100? = 10000?)
# keep a working "last.good.point"



# city.names = 'C.47900'
# city.names = c('C.47900', 'C.16980')

get.outliers.for.outcome = function(outcome, data.manager, locations, percent.change.per.year = 0.2, ignore.below = 100) {
    
    find.outliers = generate.find.outliers.function(threshold.of.change, ignore.below)
    
    source.names = names(data.manager$data[[outcome]]$estimate)
    all.flagged.list = lapply(source.names, function(source.name) {
        
        ont.names = names(data.manager$data[[outcome]]$estimate[[source.name]])
        flagged.this.source = lapply(ont.names, function(ont.name) {
            
            stratification.names = names(data.manager$data[[outcome]]$estimate[[source.name]][[ont.name]])
            flagged.this.ont = lapply(stratification.names, function(stratification.name) {
                data.this.stratification = data.manager$data[[outcome]]$estimate[[source.name]][[ont.name]][[stratification.name]]
                strata = expand.grid(dimnames(data.this.stratification)[!(names(dimnames(data.this.stratification)) %in% c('year', 'location'))], stringsAsFactors = FALSE)
                strata.names = do.call(paste, c(strata, sep="__"))
                
                if (nrow(strata) > 0) {
                    flagged.this.stratification = lapply(1:nrow(strata), function(stratum.row) {
                        
                        flagged.this.stratum = lapply(locations, function(city.name) {
                            if (!(city.name %in% dimnames(data.this.stratification)$location)) return(NULL)
                            dimension.values = setNames(as.list(strata[stratum.row,]), colnames(strata))
                            city.data = set.array.dimnames(array.access(data.this.stratification, location=city.name, dimension.values),
                                                              dimnames(data.this.stratification)['year'])
                            result = find.outliers(city.data[!is.na(city.data)])
                            if (length(result)>0) return(result)
                            else return(NULL)
                            
                        })
                        cities.which.flagged = !sapply(flagged.this.stratum, is.null)
                        flagged.this.stratum = flagged.this.stratum[cities.which.flagged]
                        if (length(flagged.this.stratum)>0) return(setNames(flagged.this.stratum, locations[cities.which.flagged]))
                        else return(NULL)
                        
                    })
                    strata.which.flagged = !sapply(flagged.this.stratification, is.null)
                    flagged.this.stratification = flagged.this.stratification[strata.which.flagged]
                    if (length(flagged.this.stratification)>0) return(setNames(flagged.this.stratification, strata.names[strata.which.flagged]))
                    else return(NULL)
                }
                
                else {
                    flagged.this.stratification = lapply(locations, function(city.name) {
                        if (!(city.name %in% dimnames(data.this.stratification)$location)) return(NULL)
                        city.data = set.array.dimnames(array.access(data.this.stratification, location=city.name),
                                                          dimnames(data.this.stratification)['year'])
                        result = find.outliers(city.data[!is.na(city.data)])
                        if (length(result)>0) return(result)
                        else return(NULL)
                        
                    })
                    cities.which.flagged = !sapply(flagged.this.stratification, is.null)
                    flagged.this.stratification = flagged.this.stratification[cities.which.flagged]
                    if (length(flagged.this.stratification)>0) return(list(TOTALS=setNames(flagged.this.stratification, locations[cities.which.flagged])))
                    else return(NULL)
                }
                
            })
            stratifications.which.flagged = !sapply(flagged.this.ont, is.null)
            flagged.this.ont = flagged.this.ont[stratifications.which.flagged]
            if (length(flagged.this.ont)>0) return(setNames(flagged.this.ont, stratification.names[stratifications.which.flagged]))
            else return(NULL)
            
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

generate.find.outliers.function = function(percent.change.per.year, ignore.below) {
    find.outliers = function(data.vector, percent.change.per.year = 0.25, ignore.below = 100, debug=F) {
        if (debug) browser()
        if (any(data.vector < ignore.below)) return(character(0))
        years = dimnames(data.vector)$year
        last.good.year = years[[1]]
        flagged.years = character(0)
        for (y in years) {
            if (data.vector[[last.good.year]]==0) break # WHAT DO WE DO WITH 0, NOT TO MENTION SMALL NUMBERS??
            abs.percent.difference = abs(data.vector[[y]] - data.vector[[last.good.year]]) / data.vector[[last.good.year]]
            difference.in.years = as.numeric(y)-as.numeric(last.good.year)
            tolerable.percent.difference = (1 + percent.change.per.year) ^ difference.in.years - 1
            if (abs.percent.difference > tolerable.percent.difference )
                flagged.years = c(flagged.years, y)
            else
                last.good.year = y
        }
        return (flagged.years)
    }
}


