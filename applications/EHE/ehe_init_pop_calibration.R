source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')

CALIBRATION.CODE = 'init.pop.ehe.heavy'
N.ITER = 2500

par.names = c("black.birth.rate.multiplier",
              "hispanic.birth.rate.multiplier",
              "other.birth.rate.multiplier",
              "black.birth.rate.slope.multiplier",
              "hispanic.birth.rate.slope.multiplier",
              "other.birth.rate.slope.multiplier",
              "age1.non.idu.general.mortality.rate.multiplier",
              "age2.non.idu.general.mortality.rate.multiplier",
              "age3.non.idu.general.mortality.rate.multiplier",
              "age4.non.idu.general.mortality.rate.multiplier",
              "age5.non.idu.general.mortality.rate.multiplier",
              "age1.aging.multiplier",
              "age2.aging.multiplier",
              "age3.aging.multiplier",
              "age4.aging.multiplier")

clear.calibration.cache(version='ehe',
                        location='C.12580',
                        calibration.code = CALIBRATION.CODE,
                        root.dir = '../test_runs')

register.calibration.info(CALIBRATION.CODE,
                          likelihood.instructions = heavy.weight.population.two.way.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          start.year = 1970,
                          end.year = 2030, 
                          parameter.names = par.names,
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          description = "A quick run to get population parameters in the general vicinity"
                          )

set.up.calibration(version='ehe',
                   location='C.12580',
                   calibration.code = CALIBRATION.CODE,
                   root.dir = '../test_runs',
                   cache.frequency = 250)

set.seed(12345)
start.time = Sys.time()
mcmc = run.calibration(version = 'ehe',
                location = "C.12580",
                calibration.code = CALIBRATION.CODE,
                root.dir = '../test_runs',
                chains = 1,
                update.frequency = 100,
                update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run ",
             format(N.ITER, big.mark = ","),
             " simulations (",
             round(run.time / N.ITER, 1), " seconds per simulation on average)"))

#mcmc = assemble.mcmc.from.calibration(version = 'ehe',
#                                      location = "C.12580",
#                                      calibration.code = CALIBRATION.CODE,
#                                      root.dir = '../test_runs',
#                                      chains = 1)

sim = mcmc@simulations[[length(mcmc@simulations)]]

simplot(sim, 'population')
