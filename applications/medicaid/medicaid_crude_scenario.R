source('../jheem_analyses/applications/EHE/ehe_specification.R')
library("RColorBrewer")
pal = c(brewer.pal(n=12,"Paired")[4],brewer.pal(n=12,"Paired")[5]) 
location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 
intervention.style.manager = create.style.manager(color.sim.by = "simset",
                                                  sim.palette = pal)

LOCATION = 'LA'
CALIBRATION.CODE = 'final.ehe.state'
YEARS.TO.CONSIDER = as.character(2026:2031)
END.NAME = "supp.93"

# suppression effect 
# assume 40% of PWH are on Medicaid (national estimate, will calibrate)
# assume 10 % of those on Medicaid would lose coverage (total estimate from https://shvs.org/resource/senate-passed-h-r-1-updated-estimates-on-impact-to-state-medicaid-coverage-and-expenditures-hospital-expenditures-including-impacts-by-congressional-district/)
 # --> 4% of PWH would lose coverage --> set suppression to 96% of what it was previously 

# greatest loss = 40% * 16% (Louisiana) --> 6.4% loss --> 93.6% suppression 
    # (Oregon actually has a 18% loss but we don't model Oregon)

supp.96 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .96,
                                     times = 2027, # immediate?
                                     apply.effects.as = "multiplier", # (2) 'multiplier' - the value is multiplied by the value the quantity would otherwise have taken
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = T, # this is the reverse as in a positive intervention (normally set to F)
                                     allow.values.greater.than.otherwise = F) # " (normally set to T)

supp.93 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .93,
                                     times = 2027, # immediate?
                                     apply.effects.as = "multiplier", # (2) 'multiplier' - the value is multiplied by the value the quantity would otherwise have taken
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = T, # this is the reverse as in a positive intervention (normally set to F)
                                     allow.values.greater.than.otherwise = F) # " (normally set to T)

int.supp.96 = create.intervention(supp.96,WHOLE.POPULATION,code = "supp.96") 
int.supp.93 = create.intervention(supp.93,WHOLE.POPULATION,code = "supp.93") 
no.int = get.null.intervention()

INTERVENTIONS=c("noint", "supp.93","supp.96")
sim.collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATION, interventions = INTERVENTIONS, n.sim=1000)

if(1==2){ # ran collection already
    sim.collection$run(2025, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=F)    
}

# simset = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, n.sim=1000)
# simset.no.int = no.int$run(simset,end.year = 2035)
# simset.supp.96 = int.supp.96$run(simset,end.year = 2035)
# simset.supp.93 = int.supp.93$run(simset,end.year = 2035)

simset.baseline = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE,n.sim=1000)
simset.no.int = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "noint", n.sim=1000)
simset.supp.96 = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "supp.96", n.sim=1000)
simset.supp.93 = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "supp.93", n.sim=1000)

simplot(simset.baseline,
        simset.supp.93,
        outcomes = c("new"), 
        summary.type = "median.and.interval",
        #style.manager = source.style.manager, 
        style.manager = intervention.style.manager, 
        dimension.values = list(year = 2000:2035)) 

simplot(simset.baseline,
        simset.supp.93,
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = intervention.style.manager,
        dimension.values = list(year = 2000:2030)) 

total.results = sim.collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                'suppression', 'population',
                                                'hiv.mortality',
                                                'sexual.transmission.rates','prep.uptake','testing'),
                                   output = 'numerator',
                                   dimension.values=list(year=2025:2035),
                                   keep.dimensions=c('year'),
                                   verbose = T)

total.incidence = array.access(total.results, outcome='incidence', drop = T)
total.new = array.access(total.results, outcome='new', drop = T)

apply(total.incidence,c("year","intervention"),mean)

tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections 2026-2031 if Medicaid Continues: ", 
             format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

abs.delta.cessation.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.cessation.tot.inf.noint1 = abs.delta.cessation.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections 2026-2031 with Medicaid Cuts: ", 
             format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 with Medicaid Cuts: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

