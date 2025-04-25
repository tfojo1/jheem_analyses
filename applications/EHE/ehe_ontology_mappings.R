

##----------------------------##
##-- MAPPING for RISK / SEX --##
##----------------------------##

#-----------------#
#-- MAPPING IDU --#
#-----------------#

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

#------------------------#
#-- MAPPING MSM to SEX --#
#------------------------#

register.ontology.mapping('jheem.to.cdc.sex',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('heterosexual_male', 'male'),
                                           c('msm', 'male'),
                                           c('female', 'female')))

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

register.ontology.mapping('jheem.to.msm.or.not.risk',
                          from.dimensions = c('sex'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'male', 'msm'),
                                           c('heterosexual_male', 'male', 'not_msm'),
                                           c('female', 'female', 'not_msm'))
)


#----------------------------------------#
#-- LUMPING OTHER IN WITH HETEROSEXUAL --#
#----------------------------------------#

register.ontology.mapping('lump.other.risk.and.heterosexual',
                          from.dimensions = 'risk',
                          to.dimensions = 'risk',
                          mappings = rbind(c('msm', 'msm'),
                                           c('idu', 'idu'),
                                           c('msm_idu', 'msm_idu'),
                                           c('heterosexual', 'heterosexual'),
                                           c('other', 'heterosexual'))
)

##-----------------------##
##-----------------------##
##-- MAPPINGS for RACE --##
##-----------------------##
##-----------------------##


#--------------#
#-- FROM CDC --#
#--------------#

register.ontology.mapping('cdc.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black/african american', 'black'),
                                           c('hispanic/latino', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'other'),
                                           c('white', 'other'))
)

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


#-----------------#
#-- FROM CENSUS --#
#-----------------#

register.ontology.mapping('census.to.jheem.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'other'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'other'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'other')))

register.ontology.mapping('census.immigration.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic or latino', 'hispanic'),
                                           c('black', 'black'),
                                           c('other', 'other'))) # needed?

register.ontology.mapping('census.state.immigration.to.jheem.race',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('white non hispanic', 'other'),
                                           c('asian non hispanic', 'other'),
                                           c('hispanic', 'hispanic'),
                                           c('american indian and alaska native non hispanic', 'other'),
                                           c('native hawaiian and pacific islander non hispanic', 'other'),
                                           c('black non hispanic', 'black'),
                                           c('other race non hispanic', 'other')))


#-----------------#
#-- FROM WONDER --#
#-----------------#

register.ontology.mapping('wonder.to.jheem.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic or latino', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic or latino', 'other'),
                                           c('asian or pacific islander', 'hispanic or latino', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic or latino', 'other'),
                                           c('black or african american', 'hispanic or latino', 'hispanic'),
                                           c('black or african american', 'not hispanic or latino', 'black'),
                                           c('white', 'hispanic or latino', 'hispanic'),
                                           c('white', 'not hispanic or latino', 'other')))

#-----------------#
#-- FROM NHANES --#
#-----------------#

register.ontology.mapping('nhanes.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black, non hispanic', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('other', 'other'),
                                           c('white, non hispanic', 'other')))

#-----------------#
#-- FROM AIDS VU --#
#-----------------#

register.ontology.mapping('aidsvu.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('white', 'other')))

#-----------------#
#-- For RYAN WHITE --#
#-----------------#

register.ontology.mapping('ryan.white.to.jheem.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('white', 'other'),
                                           c('american indian alaska native', 'other'),
                                           c('native hawaiian pacific islander', 'other'),
                                           c('asian', 'other')))

#-----------------#
#-- RYAN WHITE TO CDC --#
#-----------------#

register.ontology.mapping('ryan.white.to.cdc',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black', 'black/african american'),
                                           c('hispanic', 'hispanic/latino'),
                                           c('white', 'white'),
                                           c('american indian alaska native', 'american indian/alaska native'),
                                           c('native hawaiian pacific islander', 'native hawaiian/other pacific islander'),
                                           c('asian', 'asian')))


