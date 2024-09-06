
##------------------------##
##-- THE MAIN FUNCTIONS --##
##------------------------##

DEP.APPLY.PARAMETERS.FN = function(model.settings, parameters)
{
  specification.metadata = model.settings$specification.metadata
  
  races <- specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "depression.incidence",
                                                   alpha.name = 'value',
                                                   values = parameters[paste0(races,'.depression.incidence.rr')],
                                                   dimension = "race", 
                                                   applies.to.dimension.values = races)
  
  sexes <- specification.metadata$dim.names$sex
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.incidence",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(sexes,'.depression.incidence.rr')],
                                                 dimension = "sex", 
                                                 applies.to.dimension.values = sexes)
  
  ages <- specification.metadata$dim.names$age
  age.labels <- paste0("age",1:5)
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.incidence",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(age.labels,'.depression.incidence.rr')],
                                                 dimension = "age", 
                                                 applies.to.dimension.values = ages)
  
  risks <- c("never_IDU", "active_IDU", "IDU_in_remission")
  risk.labels <- c("idu.never", "idu.current","idu.prior")
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.incidence",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(risk.labels,'.depression.incidence.rr')],
                                                 dimension = "risk", 
                                                 applies.to.dimension.values = risks)
      
##### 
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.proportion.tx",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,'.depression.proportion.tx.or')],
                                                 dimension = "race", 
                                                 applies.to.dimension.values = races)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.proportion.tx",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(sexes,'.depression.proportion.tx.or')],
                                                 dimension = "sex", 
                                                 applies.to.dimension.values = sexes)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.proportion.tx",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(age.labels,'.depression.proportion.tx.or')],
                                                 dimension = "age", 
                                                 applies.to.dimension.values = ages)
  
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "depression.proportion.tx",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(risk.labels,'.depression.proportion.tx.or')],
                                                 dimension = "risk", 
                                                 applies.to.dimension.values = risks)
  
  
  }

