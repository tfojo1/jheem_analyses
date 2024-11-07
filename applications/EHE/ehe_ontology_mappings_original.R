register.ontology.mapping('lump.other.risk.and.heterosexual',
                          from.dimensions = 'risk',
                          to.dimensions = 'risk',
                          mappings = rbind(c('msm', 'msm'),
                                           c('idu', 'idu'),
                                           c('msm_idu', 'msm_idu'),
                                           c('heterosexual', 'heterosexual'),
                                           c('other', 'heterosexual')))

register.ontology.mapping('idu.to.active.prior',
                          from.dimensions = 'risk',
                          to.dimensions = 'risk',
                          mappings = rbind(c('never_IDU', 'non.idu'),
                                           c('active_IDU', 'idu'),
                                           c('IDU_in_remission', 'idu')))

register.ontology.mapping('non.idu.to.never.prior',
                          from.dimensions = 'risk',
                          to.dimensions = 'risk',
                          mappings = rbind(c('never_IDU', 'non.active.idu'),
                                           c('active_IDU', 'active.idu'),
                                           c('IDU_in_remission', 'non.active.idu')))

register.ontology.mapping('jheem.to.cdc.sex.risk',
                          from.dimensions = c('sex', 'risk'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'never_IDU', 'male', 'msm'),
                                           c('msm', 'active_IDU', 'male', 'msm_idu'),
                                           c('msm', 'IDU_in_remission', 'male', 'msm_idu'),
                                           c('heterosexual_male', 'never_IDU', 'male', 'heterosexual'),
                                           c('heterosexual_male', 'active_IDU', 'male', 'idu'),
                                           c('heterosexual_male', 'IDU_in_remission', 'male', 'idu'),
                                           c('female', 'never_IDU', 'female', 'heterosexual'),
                                           c('female', 'active_IDU', 'female', 'idu'),
                                           c('female', 'IDU_in_remission', 'female', 'idu')))

# Not sure if this would work - might give us some unwanted dimensions in the mapped result
register.ontology.mapping('jheem.to.msm.or.not.risk',
                          from.dimensions = c('sex'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'male', 'msm'),
                                           c('heterosexual_male', 'male', 'not_msm'),
                                           c('female', 'female', 'not_msm')))

register.ontology.mapping('cdc.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black/african american', 'black'),
                                           c('hispanic/latino', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'other'),
                                           c('white', 'other')))

register.ontology.mapping('cdc.msa.reports.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black/african american', 'black'),
                                           c('hispanic/latino', 'hispanic'),
                                           c('white', 'other')))

register.ontology.mapping('cdc.aids.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('white', 'other')))

register.ontology.mapping('wonder.to.census.race.1',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'american indian or alaska native'),
                                           c('asian or pacific islander', 'asian or pacific islander'),
                                           c('black or african american', 'black'),
                                           c('white', 'white')))

register.ontology.mapping('wonder.to.census.ethnicity.1',
                          from.dimensions = 'ethnicity',
                          to.dimensions = 'ethnicity',
                          mappings = rbind(c('hispanic or latino', 'hispanic'),
                                           c('not hispanic or latino', 'not hispanic')))

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

register.ontology.mapping('census.to.jheem.ethnicity.only',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic', 'hispanic'),
                                           c('not hispanic', 'other')))

register.ontology.mapping('wonder.to.jheem.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('white', 'hispanic or latino', 'hispanic'),
                                           c('white', 'not hispanic or latino', 'other'),
                                           c('black or african american', 'hispanic or latino', 'hispanic'),
                                           c('black or african american', 'not hispanic or latino', 'black'),
                                           c('american indian or alaska native', 'hispanic or latino', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic or latino', 'other'),
                                           c('asian or pacific islander', 'hispanic or latino', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic or latino', 'other')))

register.ontology.mapping('wonder.to.jheem.race.2',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('white', 'other')))

register.ontology.mapping('jheem.to.cdc.sex',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('heterosexual_male', 'male'),
                                           c('msm', 'male'),
                                           c('female', 'female')))

register.ontology.mapping('census.immigration.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic or latino', 'hispanic'),
                                           c('white, non-hispanic', 'white'),
                                           c('black', 'black'),
                                           c('other', 'other'))) # needed?

register.ontology.mapping('emory.sex.to.cdc.sex.temporary',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('male', 'male'),
                                           c(NA, 'female')))

register.ontology.mapping('proportion.tested.to.adult.population',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic or latino', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic or latino', 'american indian/alaska native'),
                                           c('asian or pacific islander', 'hispanic or latino', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic or latino', 'asian/pacific islander'),
                                           c('black or african american', 'hispanic or latino', 'hispanic'),
                                           c('black or african american', 'not hispanic or latino', 'black'),
                                           c('white','hispanic or latino','hispanic'),
                                           c('white','not hispanic or latino','White'),
                                           c(NA, NA, 'other')))

register.ontology.mapping('proportion.tested.to.adult.population.2',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'american indian/alaska native'),
                                           c('asian', 'asian/pacific islander'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'asian/pacific islander'),
                                           c('other race', 'other'),
                                           c('white', 'white')))

register.ontology.mapping('proportion.tested.to.jheem.race',
                          from.dimension = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'other'),
                                           c('other race', 'other'),
                                           c('white', 'other')))

register.ontology.mapping('census.to.brfss.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'american indian/alaska native'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'asian/pacific islander'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'white'),
                                           c(NA, NA, 'other')))

register.ontology.mapping('nhanes.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black, non hispanic', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('other', 'other'),
                                           c('white, non hispanic', 'other')))

register.ontology.mapping('stratified.census.to.census.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions =  c('race', 'ethnicity'),
                          mappings = rbind(c('american indian and alaska native', 'hispanic', 'american indian or alaska native', 'hispanic'),
                                           c('american indian and alaska native', 'not hispanic', 'american indian or alaska native', 'not hispanic'),
                                           c('asian', 'hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('asian', 'not hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('black', 'hispanic', 'black', 'hispanic'),
                                           c('black', 'not hispanic', 'black', 'not hispanic'),
                                           c('native hawaiian and other pacific islander', 'hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('native hawaiian and other pacific islander', 'not hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('white', 'hispanic', 'white', 'hispanic'),
                                           c('white', 'not hispanic', 'white', 'not hispanic')))
