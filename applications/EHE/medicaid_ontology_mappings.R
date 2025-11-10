#--------------#
#-- FROM ACS --#
#--------------#

register.ontology.mapping('acs.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'other'),
                                           c('asian/native hawaiian or pacific islander', 'other'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('white', 'other')))