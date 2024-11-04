
# track all mortality, but then only include infection-specific
track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'aids.specific.deaths',
                      outcome.metadata = NULL,
                      save = F,
                      scale = 'non.negative.number',
                      dynamic.quantity.name = 'mortality',
                      groups = 'infected',
                      include.tags = "infection.specific.mortality", # inherently excludes emigration
                      keep.dimensions = c("location","race","sex","risk"),
                      from.year = 1980, 
                      to.year = 2001)

# track all mortality, but then only include general (non-infection-specific) within diagnosed
track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'non.aids.deaths.in.diagnosed',
                      outcome.metadata = NULL,
                      save = F,
                      scale = 'non.negative.number',
                      dynamic.quantity.name = 'mortality',
                      groups = 'infected',
                      include.tags = c('non.idu.general.mortality','idu.mortality'),  
                      keep.dimensions = c("location","race","sex","risk"),
                      subset.dimension.values = list(continuum = "diagnosed.states"),                      
                      from.year = 1980, 
                      to.year = 2001)

# combine the two 
track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'aids.deaths',
                         outcome.metadata = create.outcome.metadata(display.name = 'AIDS Deaths',
                                                                    description = "Number of People with HIV who Died of AIDS in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Deaths',
                                                                    units = 'deaths',
                                                                    singular.unit = 'death'),
                         value=expression(aids.specific.deaths + non.aids.deaths.in.diagnosed),
                         corresponding.data.outcome = 'aids.deaths',
                         keep.dimensions = c("location","race","sex","risk"),
                         from.year = 1980, 
                         to.year = 2001)