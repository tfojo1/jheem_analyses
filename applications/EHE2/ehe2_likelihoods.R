


# BEST-PRACTICE CONVENTIONS

# joint likelihoods for a calibration are named
#  ehe2.stage<x>.calibration.likelihood.instructions

# likelihood components are named
#  ehe2.<outcome>.<unstratified/which specific dimensions are stratifie+one-way/two-way/etc>.likelihood.instructions
# pass the weight at the joint likelihood stage

# explicitly always define to-year and from-year

# any calculated sds/cvs/etc should have literal string constant number in code, with reference to code and date from which it was generated



##---------------##
##---------------##
##-- CONSTANTS --##
##---------------##
##---------------##

EHE2.DIAGNOSES.CV = 0.03331971 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnoses", output='cv.and.exponent.of.variance')
EHE2.DIAGNOSES.EXP.OF.VAR = 0.3893292 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnoses", output='cv.and.exponent.of.variance')
EHE2.PREVALENCE.CV = 0.07956432 #from calculating_error_terms_for_ehe_likelihoods.R - calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.fixed.exponent.of.variance',PREVALENCE.EXP.OF.VAR)
EHE2.PREVALENCE.EXP.OF.VAR = 0.590671901063418 #from error_for_prevalence_formula.R

##---------------------##
##---------------------##
##-- FULL LIKELIHOOD --##
##---------------------##
##---------------------##

#----------------------------#
#-- POPULATION LIKELIHOODS --#
#----------------------------#

#------------------------------#
#-- TRANSMISSION LIKELIHOODS --#
#------------------------------#

ehe2.new.diagnoses.likelihood.instructions.full = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                         outcome.for.sim = "new",
                                         dimensions = c("age","sex","race","risk"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = list(EHE2.DIAGNOSES.CV, EHE2.DIAGNOSES.EXP.OF.VAR), 
                                         error.variance.type = c('cv','exp.of.variance'),
                                         minimum.error.sd = 1,
                                         weights = 1,
                                         equalize.weight.by.year = T
    )

#---------------------------#
#-- MORTALITY LIKELIHOODS --#
#---------------------------#

#--------------------------------#
#-- AIDS DIAGNOSES LIKELIHOODS --#
#--------------------------------#

#---------------------------#
#-- CONTINUUM LIKELIHOODS --#
#---------------------------#

#----------------------#
#-- PrEP LIKELIHOODS --#
#----------------------#

#---------------------#
#-- IDU LIKELIHOODS --#
#---------------------#

ehe2.idu.active.prior.ratio.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'idu.active.prior.ratio',
    
    compute.function = function(sim, data, log=T)
    {
        pop = sim$optimized.get(data$optimized.get.instr)
        
        active.prior.ratios.by.age = colSums(pop[,,'active_IDU']) /colSums(pop[,,'IDU_in_remission'])
        active.prior.ratios.by.age[is.na(active.prior.ratios.by.age)] = 1
        
        # active.prior.ratios.by.age = sim$get('population',
        #                                      dimension.values=list(year = data$active.to.remission.ratios$years,
        #                                                            risk = 'active_IDU'),
        #                                      keep.dimensions = 'age',
        #                                      drop.single.sim.dimension = T) /
        #     sim$get('population',
        #             dimension.values=list(year = data$active.to.remission.ratios$years,
        #                                   risk = 'IDU_in_remission'),
        #             keep.dimensions = 'age',
        #             drop.single.sim.dimension = T)
        
        d = dlnorm(x = active.prior.ratios.by.age,
                   meanlog = base::log(as.numeric(data$active.to.remission.ratios$age)),
                   sdlog = log(1.25) / 2,
                   log = log)
        
        if (log)
            sum(d)
        else
            prod(d)
    },
    
    get.data.function = function(version, location)
    {
        active.to.remission.ratios = get.cached.object.for.version('active.to.remission.ratios', version=version)
        
        sim.metadata = get.simulation.metadata(version=version, location=location)
        optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
            'population', 
            dimension.values=list(year = active.to.remission.ratios$years,
                                  risk = c('active_IDU','IDU_in_remission')),
            keep.dimensions = c('year','age','risk'),
            drop.single.sim.dimension = T
        )
        
        list(
            active.to.remission.ratios = active.to.remission.ratios,
            optimized.get.instr = optimized.get.instr
        )
    }
)

#-----------------------#
#-- COVID LIKELIHOODS --#
#-----------------------#

#--------------------------------#
#-- PROPORTION MSM LIKELIHOODS --#
#--------------------------------#

ehe2.proportion.msm.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'proportion.msm',
    
    compute.function = function(sim, data, log=T)
    {
        sim.total = sim$optimized.get(data$total.optimized.get.instr)
        total.d = dnorm(data$totals, sim.total, data$total.sd)
        
        if (is.null(data$by.race))
            race.d = 0
        else
        {
            sim.by.race = sim$optimized.get(data$race.optimized.get.instr)
            race.d = dnorm(data$by.race, sim.by.race, data$race.sd)
        }
        
        d = sum(total.d, na.rm=T) + sum(race.d, na.rm=T)
        
        if (log)
            sum(d)
        else
            prod(d)
    },
    
    get.data.function = function(version, location)
    {
        sim.metadata = get.simulation.metadata(version=version, location=location)
        spec.metadata = get.specification.metadata(version=version, location=location)
        
        counties = get.contained.locations(location, 'county')
        
        #-- Pull the total data --#
        total.data = SURVEILLANCE.MANAGER$pull('proportion.msm',
                                               dimension.values = list(location=counties,
                                                                       sex='male'))[,,1,drop=F]
        if (is.null(total.data))
            stop(paste0("Could not pull data on 'proportion.msm' totals for location '", location, "'"))
        
        total.years = dimnames(total.data)$year
        males = CENSUS.MANAGER$pull(outcome = 'population',
                                    keep.dimensions = c('location', 'age','race','ethnicity'),
                                    dimension.values = list(location = counties,
                                                            year = total.years,
                                                            sex = 'male'),
                                    from.ontology.names = 'census')[,,,,1,drop=F]
        
        if (length(total.years)==1)
        {
            dim.names = c(list(year=total.years),
                          dimnames(males))
            
            dim(males) = sapply(dim.names, length)
            dimnames(males) = dim.names
        }
        
        males = apply(males, c('year','location'), sum)
        
        total.proportion.msm = rowSums(total.data * as.numeric(males)) / rowSums(males)
        
        #-- Pull the race data, map it, and aggregated it --#
        race.data = SURVEILLANCE.MANAGER$pull('proportion.msm',
                                              dimension.values = list(location=location, sex='male'),
                                              keep.dimensions = c('year','race'))       
        
        if (is.null(race.data))
        {
            #            stop(paste0("Could not pull race-specific data on 'proportion.msm' for location '", location, "'"))
            proportion.msm.by.race = NULL
        }
        else
        {
            race.years = dimnames(race.data)$year
            
            males = SURVEILLANCE.MANAGER$pull('adult.population',
                                              dimension.values = list(location=location, sex='male', year=race.years),
                                              keep.dimensions = c('year','race','ethnicity'))[,,,1]
            
            if (is.null(males))
                stop(paste0("Could not pull population data for location '", location, "'"))
            
            dim.names = list(year = race.years,
                             race = spec.metadata$dim.names$race)
            
            proportion.msm.by.race = array(NA, dim = sapply(dim.names, length), dimnames = dim.names)
            
            proportion.msm.by.race[,'hispanic'] = race.data[,'hispanic',1]
            proportion.msm.by.race[,'black'] = race.data[,'black',1]
            
            p.white = race.data[,'white',1]
            n.white = males[,'white','not hispanic']
            
            p.aapi = 0.9 * race.data[,'asian',1] + 0.1 * race.data[,'native hawaiian/other pacific islander',1]
            n.aapi = males[,'asian or pacific islander', 'not hispanic']
            
            p.aian = race.data[,'american indian/alaska native',1]
            n.aian = males[,'american indian or alaska native', 'not hispanic']
            
            p.other = cbind(p.white, p.aapi, p.aian)
            n.other = cbind(n.white, n.aapi, n.aian)
            n.other[is.na(p.other)] = NA
            
            proportion.msm.by.race[,'other'] = rowSums(p.other * n.other, na.rm=T) / rowSums(n.other, na.rm=T)
        }
        
        #-- Set up optimized get instructions --#
        
        total.optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
            'proportion.msm', 
            dimension.values=list(year = total.years),
            keep.dimensions = c('year'),
            drop.single.sim.dimension = T
        )
        
        if (is.null(proportion.msm.by.race))
            race.optimized.get.instr = NULL
        else
        {
            race.optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
                'proportion.msm', 
                dimension.values=list(year = total.years, race=dimnames(proportion.msm.by.race)$race),
                keep.dimensions = c('year','race'),
                drop.single.sim.dimension = T
            )
        }
        
        #-- Package it up --#
        
        
        list(
            totals = total.proportion.msm,
            by.race = proportion.msm.by.race,
            
            total.optimized.get.instr = total.optimized.get.instr,
            race.optimized.get.instr = race.optimized.get.instr,
            
            total.sd = 0.005,
            race.sd = 0.025
        )
    }
)


#------------------------------------#
#-- ASSEMBLE into JOINT LIKELIHOOD --#
#------------------------------------#

ehe2.stage0.calibration.likelihood.instructions = join.likelihood.instructions(
    
    # DEMOGRAPHIC LIKELIHOODS
    ehe2.population.likelihood.instructions.stage0,
    ehe2.immigration.likelihood.instructions.stage0,
    ehe2.emigration.likelihood.instructions.stage0,
    ehe2.general.mortality.likelihood.instructions.stage0,
    ehe2.proportion.msm.likelihood.instructions,

    # CASE REPORTING
    ehe2.unstratified.new.diagnoses.likelihood.instructions.stage0,
    ehe2.unstratified.prevalence.likelihood.instructions.stage0,
    ehe2.unstratified.aids.diagnoses.likelihood.instructions.stage0
    
)

ehe2.stage1.calibration.likelihood.instructions = join.likelihood.instructions(
    
    # DEMOGRAPHIC
    ehe2.population.likelihood.instructions.stage1,
    
    # CASE REPORTING
    ehe2.race.risk.one.way.new.diagnoses.likelihood.instructions.stage1,
    ehe2.race.risk.one.way.prevalence.likelihood.instructions.stage1,
    ehe2.race.risk.sex.one.way.aids.diagnoses.likelihood.instructions.stage1,
    ehe2.sex.one.way.hiv.mortality.biased.likelihood.instructions,
    
    # IDU
    ehe2.heroin.likelihood.instructions,
    ehe2.cocaine.likelihood.instructions,
    ehe2.idu.active.prior.ratio.likelihood.instructions,
    
    # 
    
    # Future Incidence?
)

ehe2.stage2.calibration.likelihood.instructions = join.likelihood.instructions(
    
    # DEMOGRAPHIC LIKELIHOODS
    ehe2.population.likelihood.instructions.stage2,
    ehe2.immigration.likelihood.instructions.stage2,
    ehe2.emigration.likelihood.instructions.stage2,
    ehe2.general.mortality.likelihood.instructions.stage2,
    ehe2.proportion.msm.likelihood.instructions,
    
    # CASE REPORTING LIKELIHOODS
    ehe2.new.diagnoses.likelihood.instructions.stage2,
    ehe2.prevalence.likelihood.instructions.stage2,
    ehe2.non.age.aids.diagnoses.likelihood.instructions.stage2,
    ehe2.sex.one.way.hiv.mortality.biased.likelihood.instructions,
    
    # CONTINUUM LIKELIHOODS
    ehe2.proportion.tested.likelihood.instructions,
    ehe2.hiv.test.positivity.likelihood.instructions, 
    ehe2.awareness.likelihood.instructions,
    ehe2.suppression.likelihood.instructions,
    
    # PREP LIKELIHOODS
    ehe2.prep.uptake.likelihood.instructions,
    ehe2.prep.indications.likelihood.instructions,
    
    # IDU LIKELIHOODS
    ehe2.heroin.likelihood.instructions,
    ehe2.cocaine.likelihood.instructions,
    ehe2.idu.active.prior.ratio.likelihood.instructions,
    
    # COVID LIKELIHOODS
    ehe2.number.of.tests.year.on.year.change.likelihood.instructions,
    ehe2.gonorrhea.year.on.year.change.likelihood.instructions,
    ehe2.ps.syphilis.year.on.year.change.likelihood.instructions
    
    # FUTURE INCIDENCE PENALTY
    #future.new.incidence.change.likelihood.instructions,
)