

# You just need to do this once to set up dummy simsets on your local machine
if (1==2)
{
    source('test/set_up_dummy_ehe_sims.R')
}

CALIBRATION.CODE = NULL #for now, we are going to use 'uncalibrated' (ie, manually generated) simulations
LOCATIONS = c('C.12580','C.33100') # for now, Baltimore and Miami

PREP.UPSCALE.INTERVENTION.CODES = c('yourcode1', 'yourcode2') #@padma - update this

# Build a simset collection of the cities and interventions you want to consider
collection = create.simset.collection(version='ehe',
                                      calibration.code=CALIBRATION.CODE,
                                      locations = LOCATIONS,
                                      interventions = PREP.UPSCALE.INTERVENTION.CODES)

# Run all the interventions - you should just need to do this once
collection$run(start.year = 2025, end.year = 2035, 
               verbose = T,
               overwrite.prior = F) #NB: this last argument means it is not going to re-run interventions - so if you change the intervention, you need to set this to T to re-run

# Get statistics out of your run simsets
# Do this as often as you like
results = collection$get(outcomes = c('incidence','population'),
                         dimension.values = list(year='2035'),
                         keep.dimensions = c('race'))



#you should be able to get your irr's by dividing the 'incidence' values by the 'population' values