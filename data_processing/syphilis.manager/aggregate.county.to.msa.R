
my.msas = get.all.for.type("CBSA")

# Using data only for the one source/ontology/stratification we care about
all.births.data = syphilis.manager$data$births.numerator.for.fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity
all.female.pop.data = syphilis.manager$data$female.population.denominator.for.fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity

# This may grab non-county locations, but it won't matter for how we use it
counties.present.in.both.datasets = intersect(dimnames(all.births.data)$location,
                                              dimnames(all.female.pop.data)$location)

for (msa in my.msas) {
    
    # Only use counties that we have in both datasets 
    counties.this.msa = locations::get.contained.locations(msa, 'county')
    counties.to.use = intersect(counties.this.msa, counties.present.in.both.datasets)
    if (length(counties.to.use)==0) next
    
    births.this.msa = all.births.data[,counties.to.use,,,,drop=F]
    female.pop.this.msa = all.female.pop.data[,counties.to.use,,,,drop=F]
    
    # To aggregate, assume NA means suppressed data, or if it's missing, hope it's missing in both sets
    non.location.margin = setdiff(names(dim(births.this.msa)), 'location')
    births.this.msa = apply(births.this.msa, MARGIN=non.location.margin, FUN=sum, na.rm=T)
    female.pop.this.msa = apply(female.pop.this.msa, MARGIN=non.location.margin, FUN=sum, na.rm=T)
    
    fertility.rate.this.msa = births.this.msa / female.pop.this.msa
    
    syphilis.manager$put(
        fertility.rate.this.msa,
        outcome = 'fertility.rate',
        source = 'cdc.wonder.natality',
        ontology.name = 'cdc.fertility',
        dimension.values = list(location=msa),
        url = 'https://wonder.cdc.gov/natality.html',
        details = 'CDC Wonder Natality Data')
} 
