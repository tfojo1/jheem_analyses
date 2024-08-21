# PK: This is the current verion


# Next: 
# do some plotting
# add dynamic variables to transmssion model and experiemnt with them
##################
# create the SHIELD.SPECIFICATION
# create an engine object
# engine.run()

source('applications/SHIELD/shield_specification.R')
engine = create.jheem.engine('shield', 'US', 2025)
sim = engine$run()
 
sim$trt.initiation
simplot(sim, 'incidence',split.by = 'age',facet.by = 'race',dimension.values =  list(age='0-14 years'))
        # dimension.values = list(year = 2000:2020))
simplot(sim, 'trt.initiation')

sim$parameters
sim$params



# we ned to setup the parameter