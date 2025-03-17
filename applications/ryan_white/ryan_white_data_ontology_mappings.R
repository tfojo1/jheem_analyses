
register.ontology.mapping(name = 'drop.trans.mapping',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(
                            c('msm', 'male'),
                            c('heterosexual_male','male'),
                            c('female', 'female'),
                            c(NA, 'trans')
                          ) )

register.ontology.mapping(name = 'ryan.white.race.mapping',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(
                            c('American Indian/Alaska Native', 'other'),
                            c('Asian', 'other'),
                            c('Black/African American', 'black'),
                            c('Hispanic/Latino', 'hispanic'),
                            c('Native Hawaiian/Pacific Islander', 'other'),
                            c('White', 'other'),
                            c('Multiple races', 'other')
                          ))

register.ontology.mapping(name = 'idu.het.risk.only',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk'),
                          mappings = rbind(
                            c('heterosexual_male', 'never_IDU', 'heterosexual'),
                            c('heterosexual_male', 'active_IDU', 'idu'),
                            c('heterosexual_male', 'IDU_in_remission', 'idu'),
                            c('female', 'never_IDU', 'heterosexual'),
                            c('female', 'active_IDU', 'idu'),
                            c('female', 'IDU_in_remission', 'idu')
                          ))

register.ontology.mapping(name = 'idu.het.risk.female.only.with.to.sex',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk','sex'),
                          mappings = rbind(
                            c('female', 'never_IDU', 'heterosexual','female'),
                            c('female', 'active_IDU', 'idu','female'),
                            c('female', 'IDU_in_remission', 'idu','female')
                          ))

register.ontology.mapping(name = 'male.risk.only.with.to.sex',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk','sex'),
                          mappings = rbind(
                            c('msm', 'never_IDU', 'msm', 'male'),
                            c('msm', 'active_IDU', 'msm_idu', 'male'),
                            c('msm', 'IDU_in_remission', 'msm_idu', 'male'),
                            c('heterosexual_male', 'never_IDU', 'heterosexual','male'),
                            c('heterosexual_male', 'active_IDU', 'idu','male'),
                            c('heterosexual_male', 'IDU_in_remission', 'idu','male')
                          ))

register.ontology.mapping(name = 'male.risk.only.with.to.sex.omit.female',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk','sex'),
                          mappings = rbind(
                            c('msm', 'never_IDU', 'msm', 'male'),
                            c('msm', 'active_IDU', 'msm_idu', 'male'),
                            c('msm', 'IDU_in_remission', 'msm_idu', 'male'),
                            c('heterosexual_male', 'never_IDU', 'heterosexual','male'),
                            c('heterosexual_male', 'active_IDU', 'idu','male'),
                            c('heterosexual_male', 'IDU_in_remission', 'idu','male'),
                            c(NA, NA, 'heterosexual','female'),
                            c(NA, NA, 'idu','female'),
                            c(NA, NA, 'idu','female')
                          ))

register.ontology.mapping(name = 'female.risk.only.with.to.sex.omit.male',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk','sex'),
                          mappings = rbind(
                            c(NA, NA, 'msm', 'male'),
                            c(NA, NA, 'msm_idu', 'male'),
                            c(NA, NA, 'msm_idu', 'male'),
                            c(NA, NA, 'heterosexual','male'),
                            c(NA, NA, 'idu','male'),
                            c(NA, NA, 'idu','male'),
                            c('female', 'never_IDU', 'heterosexual','female'),
                            c('female', 'active_IDU', 'idu','female'),
                            c('female', 'IDU_in_remission', 'idu','female')
                          ))

register.ontology.mapping(name = 'idu.het.risk.only.with.to.sex',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('sex','risk'),
                          mappings = rbind(
                            c('heterosexual_male', 'never_IDU', 'male','heterosexual'),
                            c('heterosexual_male', 'active_IDU', 'male','idu'),
                            c('heterosexual_male', 'IDU_in_remission', 'male','idu'),
                            c('female', 'never_IDU', 'female', 'heterosexual'),
                            c('female', 'active_IDU', 'female', 'idu'),
                            c('female', 'IDU_in_remission', 'female', 'idu')
                          ))

register.ontology.mapping(name = 'idu.risk.only',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('risk'),
                          mappings = rbind(
                            c('heterosexual_male', 'active_IDU', 'idu'),
                            c('heterosexual_male', 'IDU_in_remission', 'idu'),
                            c('female', 'active_IDU', 'idu'),
                            c('female', 'IDU_in_remission', 'idu')
                          ))

register.ontology.mapping(name = 'idu.risk.only.with.to.sex',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('sex','risk'),
                          mappings = rbind(
                            c('heterosexual_male', 'active_IDU', 'male','idu'),
                            c('heterosexual_male', 'IDU_in_remission', 'male','idu'),
                            c('female', 'active_IDU', 'female', 'idu'),
                            c('female', 'IDU_in_remission', 'female', 'idu')
                          ))

register.ontology.mapping(name = 'idu.risk.only.with.to.sex.incl.msm',
                          from.dimensions = c('sex','risk'),
                          to.dimensions = c('sex','risk'),
                          mappings = rbind(
                            c('msm', 'active_IDU', 'male','idu'),
                            c('msm', 'IDU_in_remission', 'male','idu'),
                            c('heterosexual_male', 'active_IDU', 'male','idu'),
                            c('heterosexual_male', 'IDU_in_remission', 'male','idu'),
                            c('female', 'active_IDU', 'female', 'idu'),
                            c('female', 'IDU_in_remission', 'female', 'idu')
                          ))


# register.ontology.mapping(name = 'do.our.best.to.risk.only',
#                           from.dimensions = c('sex','risk'),
#                           to.dimensions = 'risk',
#                           mappings = rbind(
#                             c('msm', 'never_IDU', 'msm'),
#                             c('msm', 'active_IDU', 'msm_idu'),
#                             c('msm', 'IDU_in_remission', 'msm_idu'),
#                             c('heterosexual_male', 'never_IDU', 'heterosexual'),
#                             c('heterosexual_male', 'active_IDU', 'heterosexual'),
#                             c('heterosexual_male', 'IDU_in_remission', 'heterosexual'),
#                             c('female', 'never_IDU', 'heterosexual'),
#                             c('female', 'active_IDU', 'heterosexual'),
#                             c('female', 'IDU_in_remission', 'heterosexual'),
#                             c(NA, NA, 'perinatal'),
#                             c(NA, NA, 'other')
#                           ) )
# 
# register.ontology.mapping(name = 'fold.perinatal.into.heterosexual',
#                           from.dimensions = 'risk',
#                           to.dimensions = 'risk',
#                           mappings = rbind(
#                             c('msm', 'msm'),
#                             c('idu', 'idu'),
#                             c('msm_idu', 'msm_idu'),
#                             c('heterosexual', 'heterosexual'),
#                             c('other', 'heterosexual'),
#                             c('perinatal','heterosexual')
#                           ))
