print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

print("SETTING UP MCMC")
CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
N.ITER = 10000
# SF, LA, Riverside, Memphis had a bug with 'proportion.msm.of.male' being NA or empty 
# NOLA had a bug with mobility data[] subscript out of bounds 

par.names = c("black.birth.rate.multiplier",
              "hispanic.birth.rate.multiplier",
              "other.birth.rate.multiplier",
              "age1.non.idu.general.mortality.rate.multiplier",
              "age2.non.idu.general.mortality.rate.multiplier",
              "age3.non.idu.general.mortality.rate.multiplier",
              "age4.non.idu.general.mortality.rate.multiplier",
              "age5.non.idu.general.mortality.rate.multiplier",
              "black.age1.aging.multiplier",
              "hispanic.age1.aging.multiplier",
              "other.age1.aging.multiplier",
              "black.age2.aging.multiplier",
              "hispanic.age2.aging.multiplier",
              "other.age2.aging.multiplier",
              "black.age3.aging.multiplier",
              "hispanic.age3.aging.multiplier",
              "other.age3.aging.multiplier",
              "age4.aging.multiplier",
              "black.domino.aging.multiplier",
              "hispanic.domino.aging.multiplier",
              "other.domino.aging.multiplier",
              "immigration.multiplier.time.1",
              "immigration.multiplier.time.2",
              "emigration.multiplier.time.1",
              "emigration.multiplier.time.2",
              "black.migration.multiplier.time.1",
              "black.migration.multiplier.time.2",
              "hispanic.migration.multiplier.time.1",
              "hispanic.migration.multiplier.time.2",
              "other.migration.multiplier.time.1",
              "other.migration.multiplier.time.2",
              "age1.migration.multiplier.time.1",
              "age1.migration.multiplier.time.2",
              "age2.migration.multiplier.time.1",
              "age2.migration.multiplier.time.2",
              "age3.migration.multiplier.time.1",
              "age3.migration.multiplier.time.2",
              "age4.migration.multiplier.time.1",
              "age4.migration.multiplier.time.2",
              "age5.migration.multiplier.time.1",
              "age5.migration.multiplier.time.2"
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          start.year = 1970,
                          end.year = 2030, 
                          parameter.names = par.names,
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)

