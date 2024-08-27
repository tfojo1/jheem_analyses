# Next steps: 
# contact proportions
# decompose the transmission 
# vary contact by age, sex, race (modeled as independant margines)
# sexual.transmission.rates : Next step
# setup a dumy contact amtrix and define the transmisison rate 

# fix the initial population (we have census for 2007, what are the good values to see historical sysphilis rates )


##################
# create the SHIELD.SPECIFICATION
# create an engine object
# engine.run()

source('applications/SHIELD/shield_specification.R')
engine = create.jheem.engine('shield', 'US', 2025)
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