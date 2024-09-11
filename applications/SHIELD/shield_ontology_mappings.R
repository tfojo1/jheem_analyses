#registers mapping between racial groups in the census data and the JHEEM data
#Todd: where are we using this?
register.ontology.mapping('census.to.jheem.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'other'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'other'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'other')))
