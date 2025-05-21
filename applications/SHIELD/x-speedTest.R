# SHIELD SPEED
# 1) One thing you can do is play with the tolerances on the differential equation solver. I had set them to be as tolerant as I thought worked for the HIV model, but it looks like the syphilis model here can be even more tolerant without big changes:
#     You can do:
#     solver = create.solver.metadata(rtol = 0.01, atol = 0.1)
#     and passing that as solver.metadata in your create.jheem.engine() and register.calibration.info() functions
#     I tried it, and for the engine test it cuts the number of diffeq evaluations almost in half, and doesn’t seem to change the numerical output very much

#     Note: Absolute Tolerance (atol): When the solution value is near zero, this sets a hard limit on the smallest allowable error.
#     Note: Relative Tolerance (rtol): More important when the solution values are large, to keep proportional accuracy
#     solvers control the local error per time step to satisfy: error≤atol+rtol×∣solution∣

# 2) Once I did that, the majority of the diffeq evaluation steps (70%) are before 1970. I do notice that the current sims make wild swings in syphilis infections early on before reaching a steady-ish state. 
#     You could consider (a) trying to pick starting parameters / starting population closer to steady state or (b) starting in 1960
# 
# 3) I took out the outcomes, and that didn’t help us much in terms of number of evaluations, but we could still be more efficient with them.
# Right now, we track ‘diagnosis.total’ for all stages of syphilis, and the subset that to get ps diagnoses, latent diagnoses, etc. 
# Except that right now, we track cns diagnoses and congenital diagnoses explicitly as dynamic outcomes which is a waster
# But I wonder if we shouldn’t actually track each diagnosis outcome explicitly with only the dimensions we need.
# Ie, if we don’t need CNS diagnoses by age, sex, race, set up a track.dynamic.outcome call to that with only the dimensions we need
# 
# I would start by looking at the state outcomes in the sim: $infected and $uninfected
# Are any of the population cells really, really small? That’s usually a place where we need lots of small steps to evaluate

source('applications/SHIELD/shield_specification.R')
VERSION='shield' ; LOCATION='C.12580' #Baltimore MSA

# create an engine:
engine1 = create.jheem.engine( VERSION,  LOCATION, end.year = 2030)
specification.metadata=get.specification.metadata(VERSION,LOCATION)
params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
sim1 = engine1$run(params)
# Number of elements
ds1=engine1$extract.diffeq.settings()
ds1$state_and_dx_sizes  
sum(ds1$state_and_dx_sizes)
sim1$solver.metadata

# Now increase the tolerance:
solver = create.solver.metadata(rtol = 0.01, atol = .1)
engine2 = create.jheem.engine( VERSION,  LOCATION, end.year = 2030,solver.metadata = solver)
sim2 = engine2$run(params)
# compare the # of diff equation evaluations: 
ds2=engine2$extract.diffeq.settings()
sim2$solver.metadata
