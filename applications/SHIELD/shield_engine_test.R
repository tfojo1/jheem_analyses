
##################
# create the SHIELD.SPECIFICATION
# create an engine object
# engine.run()

source('applications/SHIELD/shield_specification.R')
library(roxygen2)
roxygen2::roxygenise()

engine = create.jheem.engine('shield', 'C.12580', 2025) #@Todd: to update the code for ontology to work with the US location 
#@Zoe: will add a line for US data (we dont need the county information for the US)
params=get.medians(SHIELD.PARAMETERS.PRIOR)
params['global.trate']=1
params['msm.trate.multiplier1']=1000
sim = engine$run(params)
# sim$parameters
# 
# params2=c(global.trate=2)
# sim2 = engine$run(params2)

# =generate.random.samples(SHIELD.PARAMETERS.PRIOR,10)


simplot(sim, 'incidence')
# ,split.by = 'age',facet.by = 'race')
# ,dimension.values =  list(age='0-14 years'))
        # dimension.values = list(year = 2000:2020))
simplot(sim, 'trt.initiation')

sim$params



# we ned to setup the parameter