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
 
