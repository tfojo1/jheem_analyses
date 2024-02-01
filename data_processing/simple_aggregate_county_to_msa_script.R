# script to aggregate MSA deaths from county deaths
# does only TOTALS because of the particular application this was requested for (deaths)

get.msa.totals.from.county.simple = function(outcome, # deaths
                                             metric='estimate',
                                             msas, # MSAS.OF.INTEREST
                                             source, # census.deaths
                                             details.for.put,
                                             census.manager)
{
    error.prefix = paste0("Error aggregating county data to msa totals for outcome '", outcome, "'")
    for (location in msas) {

        counties = locations::get.contained.locations(location, "county")
        county.data.all.ontologies = census.manager$data[[outcome]][[metric]][[source]]
        county.url.all.ontologies = census.manager$url[[outcome]][[metric]][[source]]
        
        for (ont.name in names(county.data.all.ontologies)) {
            
            county.data.this.ontology = county.data.all.ontologies[[ont.name]]
            if ("year__location" %in% names(county.data.this.ontology)) {
                
                county.totals.this.ontology = county.data.this.ontology[["year__location"]]
                county.totals.url.this.ontology = county.url.all.ontologies[["year__location"]]
                
                # Throw error if we don't have data for all the counties we wanted, something that shouldn't happen for deaths when totals
                relevant.counties.found.here = intersect(counties, dimnames(county.data.this.ontology)$location)
                if (length(relevant.counties.found.here)>0 && length(relevant.counties.found.here)<length(counties))
                    stop(paste0(error.prefix, "'", outcome, "' data found for some but not all counties in MSA: '", location, "'"))
                
                # Subset the data by the counties we need
                relevant.county.totals.this.ontology = county.totals.this.ontology[get.array.access.indices(dimnames(county.totals.this.ontology), dimension.values = list(location=relevant.counties.found.here))]
                relevant.county.totals.url.this.ontology = county.totals.url.this.ontology[get.array.access.indices(dimnames(county.totals.url.this.ontology), dimension.values = list(location=relevant.counties.found.here))]
                if (is.null(relevant.county.totals.this.ontology)) next
                
                # Aggregate county data to MSA
                data.aggregated.to.msa.total = apply(relevant.county.totals.this.ontology, MARGIN = "year", FUN = sum, na.rm=F) # so that we see any missingness
                
                # Find corresponding url, assuming they are all the same
                url = relevant.county.totals.url.this.ontology[[1]]
                
                # Verify they are all the same url
                if (any(sapply(relevant.county.totals.url.this.ontology , function(x) {!identical(x, url) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", outcome, "'data for ontology '", ont.name, "' do not all have the same 'url'"))
                
                # put data
                census.manager$put(data = data.aggregated.to.msa.total,
                                   outcome = outcome,
                                   metric = metric,
                                   source = source,
                                   ontology.name = ont.name,
                                   dimension.values = list(location=relevant.counties.found.here),
                                   url = url,
                                   details = details.for.put,
                                   allow.na.to.overwrite = F)
                
                }
                
            }
            
        }
        
    }
}