
##################
# create the SHIELD.SPECIFICATION
# create an engine object
# engine.run()

source('applications/SHIELD/shield_specification.R')

# Baltimore MSA : C.12580
engine = create.jheem.engine('shield', 'C.12580', 2025)
# engine = create.jheem.engine('shield', 'US', 2025)

params=get.medians(SHIELD.PARAMETERS.PRIOR)
# params['global.trate']=1
# params['msm.trate.multiplier1']=1000

sim = engine$run(params)

#plotting:
#population data for Baltimore MSA
SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census.data$year__location[, "C.12580"]
simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER) #still doesnt plot the data

SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[, "C.12580"]
simplot(sim,"diag.ps",data.manager = SURVEILLANCE.MANAGER)
SURVEILLANCE.MANAGER$outcomes

SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[, "C.12580"]

SURVEILLANCE.MANAGER$source.info


# simplot(sim, 'population')
# sim$population
# sim$parameters
#
# params2=c(global.trate=2)
# sim2 = engine$run(params2)

# =generate.random.samples(SHIELD.PARAMETERS.PRIOR,10)




# simplot(sim, 'incidence')
# # ,split.by = 'age',facet.by = 'race')
# # ,dimension.values =  list(age='0-14 years'))
#         # dimension.values = list(year = 2000:2020))
# simplot(sim, 'trt.initiation')
#
# sim$params



# we ned to setup the parameter
