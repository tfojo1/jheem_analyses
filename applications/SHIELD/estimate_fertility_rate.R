# MAPPING FERTILITY RATE DATA AS A FUNCTION OF TIME AND OTHER COVARIATES 


if (!exists('CENSUS.MANAGER')){
  # cat("Loading Census Manager (may take a minute or two)...")
  CENSUS.MANAGER = load.data.manager.from.cache('census.manager.rdata', set.as.default=F)
  # print("Census manager read")
}
CENSUS.MANAGER$outcomes
CENSUS.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity

# years for which fertility data is reported:
YEARS=2007:2023

#####
# Total
# @Todd: if we dont include 'location' in the keep.dimension, it will return the total?
fertility.rate = CENSUS.MANAGER$pull(outcome='fertility.rate',
                                     location = 'US',
                                     year= YEARS,
                                     keep.dimensions = c('location','age','race', 'ethnicity'),
                                     na.rm=TRUE)
dimnames(fertility.rate)
##
specification.metadata=get.specification.metadata('shield','US') # specification.metadata$dim.names
target.dimnames=specification.metadata$dim.names[c('age','race')];target.dimnames

#@Todd: this doesnt run
mapped.fertility.rate=map.value.ontology(fertility.rate, 
                                         target.dim.names = target.dimnames,
                                         na.rm = TRUE)

###############

fertility.rate = CENSUS.MANAGER$pull(outcome='fertility.rate',
                                        location = 'US',
                                        year= YEARS,
                                        keep.dimensions = c('location','age','race', 'ethnicity','year'), #@Todd,Andrew: it should work without the location but it fails
                                        na.rm=TRUE)
dimnames(fertility.rate)
##
specification.metadata=get.specification.metadata('shield','US')
# specification.metadata$dim.names
# target.dimnames=specification.metadata$dim.names[c('age','race')] #Todd: this doesnt have year

target.dimnames=target.dimnames <- list(
  age = c("0-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", 
          "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-64 years", "65+ years"),
  race = c("black", "hispanic", "other"),
  year = as.character(YEARS)
);target.dimnames

mapped.fertility.rate=map.value.ontology(fertility.rate, 
                                         target.dim.names = target.dimnames,
                                         na.rm = TRUE)
