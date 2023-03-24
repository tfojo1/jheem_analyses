

if (1==2)
{
    # source this file (continuum_manager3.R) AFTER sourcing source_code
    cm = create.continuum.manager(national.surveillance = national.surveillance,
                                  suppression.anchor.year = 2020,
                                  testing.anchor.year = 2010)
    ALL.DATA.MANAGERS$continuum = cm
    save(ALL.DATA.MANAGERS, file='cached/ALL.DATA.MANAGERS.Rdata')
}

####----------------------####
####----------------------####
####-- ACCESS FUNCTIONS --####
####----------------------####
####----------------------####

get.continuum.model <- function(continuum.manager,
                               type,
                               location)
{
    if (is.null(continuum.manager[[type]]))
        stop(paste0("No model has been set for type '", type, "' in the continuum manager"))
    
    base.model = continuum.manager[[type]]
    
    # Creating a new object below lets us use the most up-to-date code for models
    create.logistic.linear.functional.form(intercept = base.model$intercept,
                                           slope = base.model$slope,
                                           anchor.year = base.model$anchor.year,
                                           min = base.model$min.proportion,
                                           max = base.model$max.proportion,
                                           parameters.are.on.logit.scale = T)
#                                           log.ors = base.model$log.orgs)
}


get.suppression.model <- function(continuum.manager,
                                  location,
                                  specification.info)
{
    get.continuum.model(continuum.manager, type='suppression', location)
}

#Returns a list with 3 components, representing a linear log-odds model of testing
# $intercept - an age x race x sex x risk array of log-odds intercepts
# $slope - an age x race x sex x risk array of log-odds slopes (relative to time)
# $anchor year - the year that corresponds to the intercept


get.testing.model <- function(continuum.manager,
                              location,
                              specification.info)
{
    
    get.continuum.model(continuum.manager=continuum.manager, type='testing', location=location)
}


##---------------------------##
##-- GETTING TESTING RATES --##
##---------------------------##

get.proportion.ever.tested <- function(cm, location)
{
    location.mask = cm$testing$total.ever.tested$location==location
    if (any(location.mask))
    {
        list(ever.tested = cm$testing$total.ever.tested$frac.ever[location.mask],
             sample.size = cm$testing$total.ever.tested$sample.size[location.mask],
             years = cm$testing$total.ever.tested$year[location.mask])
    }
    else
        NULL
}

get.proportion.ever.tested.in.state.msas <- function(cm, location)
{
    location.states = states.for.msa(location)
    locations = unique(cm$testing$total.ever.tested$location)
    same.state.mask = sapply(locations, function(loc){
        states.for.loc = states.for.msa(loc)
        any(sapply(location.states, function(st){
            any(st == states.for.loc)
        }))
    })
    if (any(same.state.mask))
    {
        locations = locations[same.state.mask]
        location.mask = sapply(cm$testing$total.ever.tested$location, function(loc){
            any(loc==locations)
        })
    }
    else
        location.mask = T
    
    years = sort(unique(cm$testing$total.ever.tested$year[location.mask]))
    ever.tested = sapply(years, function(year){
        year.mask = location.mask & cm$testing$total.ever.tested$year == year
        sum(cm$testing$total.ever.tested$frac.ever[year.mask] * cm$testing$total.ever.tested$sample.size[year.mask]) /
            sum(cm$testing$total.ever.tested$sample.size[year.mask])
    })
    sample.size = sapply(years, function(year){
        year.mask = location.mask & cm$testing$total.ever.tested$year == year
        ceiling(mean(cm$testing$total.ever.tested$sample.size[year.mask]))
    })
    
    list(ever.tested=ever.tested,
         sample.size=sample.size,
         years=years)
}

get.proportion.ever.tested.in.states <- function(cm,
                                                 states,
                                                 age=F,
                                                 race=F,
                                                 sex=F)
{
    if (age)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.age[,states,],
                      denominators = cm$state.testing$denominators.age[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.age[,states,], c('year','age'), sum, na.rm=T),
                      denominators = apply(cm$state.testing$denominators.age[,states,], c('year','age'), sum, na.rm=T))
    }
    else if (race)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.race[,states,],
                      denominators = cm$state.testing$denominators.race[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.race[,states,], c('year','race'), sum, na.rm=T),
                      denominators = apply(cm$state.testing$denominators.race[,states,], c('year','race'), sum, na.rm=T))
    }
    else if (sex)
    {
        if (length(states)==1)
            rv = list(numerators = cm$state.testing$numerators.sex[,states,],
                      denominators = cm$state.testing$denominators.sex[,states,])
        else
            rv = list(numerators = apply(cm$state.testing$numerators.sex[,states,], c('year','sex'), sum, na.rm=T),
                      denominators = apply(cm$state.testing$denominators.sex[,states,], c('year','sex'), sum, na.rm=T))
    }
    else
        stop("One of the arguments 'age', 'sex', or 'race' must be set to TRUE")
    
    rv$years = as.numeric(dimnames(rv$numerators)[['year']])
    rv
}


####----------------------####
####----------------------####
####-- SET-UP FUNCTIONS --####
####----------------------####
####----------------------####


##---------------------------------------##
##-- THE CONTROLLER SET-UP FUNCTION(s) --##
##---------------------------------------##

create.continuum.manager <- function(dir='cleaned_data/', 
                                     expanded.continuum.file = 'cleaned_data/continuum/cnics_regression.Rdata',
                                     specification = get.specification.for.version('collapsed_1.0'),
                                     national.surveillance = national.surveillance,
                                     
                                     suppression.anchor.year = 2020,
                                     testing.anchor.year = 2010,
                                     linkage.anchor.year = 2020,
                                     
                                     max.tested.proportion = 0.9,
                                     max.suppressed.proportion = 0.9,
                                     max.linked.proportion = 0.95,
                                     
                                     max.naive.to.suppressed.proportion = 0.95,
                                     max.failing.to.suppressed.proportion = 0.9,
                                     min.recently.suppressed.to.failing.proportion = 0.05,
                                     min.durably.suppressed.to.failing.proportion = 0,
                                     
                                     max.reengaged.proportion = 0.8,
                                     
                                     min.naive.retention = 0.5,
                                     min.failing.retention = 0.5,
                                     min.recently.suppressed.retention = 0.5,
                                     min.durably.suppressed.retention = 0.5,
                                     
                                     max.naive.retention = 0.98,
                                     max.failing.retention = 0.95,
                                     max.recently.suppressed.retention = 0.98,
                                     max.durably.suppressed.retention = 0.98,
                                     
                                     verbose=T
)
{
    cm = list()
    
    #-- Expanded Continuum --#
    
    cm = setup.expanded.continuum.models(cm,
                                         file=expanded.continuum.file,
                                         specification=specification,
                                         
                                         max.naive.to.suppressed.proportion = max.naive.to.suppressed.proportion,
                                         max.failing.to.suppressed.proportion = max.failing.to.suppressed.proportion,
                                         min.recently.suppressed.to.failing.proportion = min.recently.suppressed.to.failing.proportion,
                                         min.durably.suppressed.to.failing.proportion = min.durably.suppressed.to.failing.proportion,
                                         
                                         max.reengaged.proportion = max.reengaged.proportion,
                                         
                                         min.naive.retention = min.naive.retention,
                                         min.failing.retention = min.failing.retention,
                                         min.recently.suppressed.retention = min.recently.suppressed.retention,
                                         min.durably.suppressed.retention = min.durably.suppressed.retention,
                                         
                                         max.naive.retention = max.naive.retention,
                                         max.failing.retention = max.failing.retention,
                                         max.recently.suppressed.retention = max.recently.suppressed.retention,
                                         max.durably.suppressed.retention = max.durably.suppressed.retention,
                                         
                                         verbose=verbose)
    
    
    #-- Linkage --#
    if (verbose)
        print('Reading Linkage')
    cm = run.linkage.regressions(cm,
                                 dir=dir,
                                 anchor.year = linkage.anchor.year,
                                 max.linked.proportion = max.linked.proportion,
                                 specification=specification)
    
    
    if (verbose)
        print("Reading Suppression")
    cm = run.suppression.regressions(cm,
                                     national.surveillance = national.surveillance,
                                     max.suppressed.proportion=max.suppressed.proportion,
                                     anchor.year=suppression.anchor.year,
                                     specification=specification)
    
    if (verbose)
        print("Reading Testing")
    cm = run.testing.regressions(cm,
                                 dir=dir,
                                 verbose=verbose,
                                 max.tested.proportion=max.tested.proportion,
                                 anchor.year = testing.anchor.year,
                                 specification=specification)
    cm$state.testing = read.brfss.state(file = file.path(dir, 'continuum/state/BRFSS_state.csv'))
    
    
    #-- Return --#
    cm
}

##----------------------------------------------------------##
##-- Sub-Function to Set Up Models For Expanded Continuum --##
##----------------------------------------------------------##

setup.expanded.continuum.models <- function(cm,
                                            file, 
                                            specification,
                                            
                                            max.naive.to.suppressed.proportion,
                                            max.failing.to.suppressed.proportion,
                                            min.recently.suppressed.to.failing.proportion,
                                            min.durably.suppressed.to.failing.proportion,
                                            
                                            max.reengaged.proportion,
                                            
                                            min.naive.retention,
                                            min.failing.retention,
                                            min.recently.suppressed.retention,
                                            min.durably.suppressed.retention,
                                            
                                            max.naive.retention,
                                            max.failing.retention,
                                            max.recently.suppressed.retention,
                                            max.durably.suppressed.retention,
                                            
                                            verbose=T)
{
    if (verbose)
        print("Setting up time to start ART model")
    
    cm = setup.start.art.model(cm,
                               specification = specification)
  
    load(file) #-->output list
    
    if (verbose)
      print("Setting up naive-to-suppressed")
    cm$naive.to.suppressed = setup.logistic.model(anchor.year=output$anchor.year,
                                                  specification = specification,
                                                  max.proportion=max.naive.to.suppressed.proportion,
                                                  
                                                  total.intercept = output$naive.to.suppressed.noslopes.coefficients['(Intercept)'],
                                                  msm.intercept = 0,
                                                  msm.idu.intercept = output$naive.to.suppressed.noslopes.coefficients['sex.riskmsm_idu'],
                                                  heterosexual.intercept = 0,
                                                  male.heterosexual.intercept=output$naive.to.suppressed.noslopes.coefficients['sex.riskheterosexual_male'],
                                                  female.heterosexual.intercept=output$naive.to.suppressed.noslopes.coefficients['sex.riskheterosexual_female'],
                                                  idu.intercept = 0,
                                                  male.idu.intercept = output$naive.to.suppressed.noslopes.coefficients['sex.riskidu_male'],
                                                  female.idu.intercept = output$naive.to.suppressed.noslopes.coefficients['sex.riskidu_female'],
                                                  
                                                  black.intercept=output$naive.to.suppressed.noslopes.coefficients['raceblack'],
                                                  hispanic.intercept=output$naive.to.suppressed.noslopes.coefficients['racehispanic'],
                                                  other.intercept=0,
                                                  
                                                  age1.intercept=output$naive.to.suppressed.noslopes.coefficients['age.category13-25'],
                                                  age2.intercept=output$naive.to.suppressed.noslopes.coefficients['age.category25-35'],
                                                  age3.intercept=0,
                                                  age4.intercept=output$naive.to.suppressed.noslopes.coefficients['age.category45-55'],
                                                  age5.intercept=output$naive.to.suppressed.noslopes.coefficients['age.category55+'],
                                                  
                                                  total.slope = output$naive.to.suppressed.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up naive-to-lost")
    cm$naive.to.disengaged = setup.logistic.model(anchor.year=output$anchor.year,
                                                  max.proportion=1-min.naive.retention,
                                                  min.proportion=1-max.naive.retention,     
                                                  specification=specification,
                                            
                                                  total.intercept = output$naive.to.lost.noslopes.coefficients['(Intercept)'],
                                                  msm.intercept = 0,
                                                  msm.idu.intercept = output$naive.to.lost.noslopes.coefficients['sex.riskmsm_idu'],
                                                  heterosexual.intercept = 0,
                                                  male.heterosexual.intercept=output$naive.to.lost.noslopes.coefficients['sex.riskheterosexual_male'],
                                                  female.heterosexual.intercept=output$naive.to.lost.noslopes.coefficients['sex.riskheterosexual_female'],
                                                  idu.intercept = 0,
                                                  male.idu.intercept = output$naive.to.lost.noslopes.coefficients['sex.riskidu_male'],
                                                  female.idu.intercept = output$naive.to.lost.noslopes.coefficients['sex.riskidu_female'],
                                                  
                                                  black.intercept=output$naive.to.lost.noslopes.coefficients['raceblack'],
                                                  hispanic.intercept=output$naive.to.lost.noslopes.coefficients['racehispanic'],
                                                  other.intercept=0,
                                                  
                                                  age1.intercept=output$naive.to.lost.noslopes.coefficients['age.category13-25'],
                                                  age2.intercept=output$naive.to.lost.noslopes.coefficients['age.category25-35'],
                                                  age3.intercept=0,
                                                  age4.intercept=output$naive.to.lost.noslopes.coefficients['age.category45-55'],
                                                  age5.intercept=output$naive.to.lost.noslopes.coefficients['age.category55+'],
                                                  
                                                  total.slope = output$naive.to.lost.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up failing-to-suppressed")
    cm$failing.to.suppressed = setup.logistic.model(anchor.year=output$anchor.year,
                                            max.proportion=max.failing.to.suppressed.proportion,     
                                            specification=specification,
                                            
                                            total.intercept = output$failing.to.suppressed.noslopes.coefficients['(Intercept)'],
                                            msm.intercept = 0,
                                            msm.idu.intercept = output$failing.to.suppressed.noslopes.coefficients['sex.riskmsm_idu'],
                                            heterosexual.intercept = 0,
                                            male.heterosexual.intercept=output$failing.to.suppressed.noslopes.coefficients['sex.riskheterosexual_male'],
                                            female.heterosexual.intercept=output$failing.to.suppressed.noslopes.coefficients['sex.riskheterosexual_female'],
                                            idu.intercept = 0,
                                            male.idu.intercept = output$failing.to.suppressed.noslopes.coefficients['sex.riskidu_male'],
                                            female.idu.intercept = output$failing.to.suppressed.noslopes.coefficients['sex.riskidu_female'],
                                            
                                            black.intercept=output$failing.to.suppressed.noslopes.coefficients['raceblack'],
                                            hispanic.intercept=output$failing.to.suppressed.noslopes.coefficients['racehispanic'],
                                            other.intercept=0,
                                            
                                            age1.intercept=output$failing.to.suppressed.noslopes.coefficients['age.category13-25'],
                                            age2.intercept=output$failing.to.suppressed.noslopes.coefficients['age.category25-35'],
                                            age3.intercept=0,
                                            age4.intercept=output$failing.to.suppressed.noslopes.coefficients['age.category45-55'],
                                            age5.intercept=output$failing.to.suppressed.noslopes.coefficients['age.category55+'],
                                            
                                            total.slope = output$failing.to.suppressed.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up failing-to-lost")
    cm$failing.to.disengaged = setup.logistic.model(anchor.year=output$anchor.year,
                                                    max.proportion=1-min.failing.retention,
                                                    min.proportion=1-max.failing.retention,     
                                                    specification=specification,
                                                    
                                                    total.intercept = output$failing.to.lost.noslopes.coefficients['(Intercept)'],
                                                    msm.intercept = 0,
                                                    msm.idu.intercept = output$failing.to.lost.noslopes.coefficients['sex.riskmsm_idu'],
                                                    heterosexual.intercept = 0,
                                                    male.heterosexual.intercept=output$failing.to.lost.noslopes.coefficients['sex.riskheterosexual_male'],
                                                    female.heterosexual.intercept=output$failing.to.lost.noslopes.coefficients['sex.riskheterosexual_female'],
                                                    idu.intercept = 0,
                                                    male.idu.intercept = output$failing.to.lost.noslopes.coefficients['sex.riskidu_male'],
                                                    female.idu.intercept = output$failing.to.lost.noslopes.coefficients['sex.riskidu_female'],
                                                    
                                                    black.intercept=output$failing.to.lost.noslopes.coefficients['raceblack'],
                                                    hispanic.intercept=output$failing.to.lost.noslopes.coefficients['racehispanic'],
                                                    other.intercept=0,
                                                    
                                                    age1.intercept=output$failing.to.lost.noslopes.coefficients['age.category13-25'],
                                                    age2.intercept=output$failing.to.lost.noslopes.coefficients['age.category25-35'],
                                                    age3.intercept=0,
                                                    age4.intercept=output$failing.to.lost.noslopes.coefficients['age.category45-55'],
                                                    age5.intercept=output$failing.to.lost.noslopes.coefficients['age.category55+'],
                                                    
                                                    total.slope = output$failing.to.lost.noslopes.coefficients['relative.year'])

    if (verbose)
      print("Setting up recently-suppressed-to-failing")
    cm$recently.suppressed.to.failing = setup.logistic.model(anchor.year=output$anchor.year,
                                                             min.proportion=min.recently.suppressed.to.failing.proportion,     
                                                             specification=specification,
                                                             
                                                             total.intercept = output$recent.to.failing.noslopes.coefficients['(Intercept)'],
                                                             msm.intercept = 0,
                                                             msm.idu.intercept = output$recent.to.failing.noslopes.coefficients['sex.riskmsm_idu'],
                                                             heterosexual.intercept = 0,
                                                             male.heterosexual.intercept=output$recent.to.failing.noslopes.coefficients['sex.riskheterosexual_male'],
                                                             female.heterosexual.intercept=output$recent.to.failing.noslopes.coefficients['sex.riskheterosexual_female'],
                                                             idu.intercept = 0,
                                                             male.idu.intercept = output$recent.to.failing.noslopes.coefficients['sex.riskidu_male'],
                                                             female.idu.intercept = output$recent.to.failing.noslopes.coefficients['sex.riskidu_female'],
                                                             
                                                             black.intercept=output$recent.to.failing.noslopes.coefficients['raceblack'],
                                                             hispanic.intercept=output$recent.to.failing.noslopes.coefficients['racehispanic'],
                                                             other.intercept=0,
                                                             
                                                             age1.intercept=output$recent.to.failing.noslopes.coefficients['age.category13-25'],
                                                             age2.intercept=output$recent.to.failing.noslopes.coefficients['age.category25-35'],
                                                             age3.intercept=0,
                                                             age4.intercept=output$recent.to.failing.noslopes.coefficients['age.category45-55'],
                                                             age5.intercept=output$recent.to.failing.noslopes.coefficients['age.category55+'],
                                                             
                                                             total.slope = output$recent.to.failing.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up recently-suppressed-to-lost")
    cm$recently.suppressed.to.disengaged = setup.logistic.model(anchor.year=output$anchor.year,
                                                                max.proportion=1-min.recently.suppressed.retention,
                                                                min.proportion=1-max.recently.suppressed.retention,     
                                                                specification=specification,
                                                                
                                                                total.intercept = output$recent.to.lost.noslopes.coefficients['(Intercept)'],
                                                                msm.intercept = 0,
                                                                msm.idu.intercept = output$recent.to.lost.noslopes.coefficients['sex.riskmsm_idu'],
                                                                heterosexual.intercept = 0,
                                                                male.heterosexual.intercept=output$recent.to.lost.noslopes.coefficients['sex.riskheterosexual_male'],
                                                                female.heterosexual.intercept=output$recent.to.lost.noslopes.coefficients['sex.riskheterosexual_female'],
                                                                idu.intercept = 0,
                                                                male.idu.intercept = output$recent.to.lost.noslopes.coefficients['sex.riskidu_male'],
                                                                female.idu.intercept = output$recent.to.lost.noslopes.coefficients['sex.riskidu_female'],
                                                                
                                                                black.intercept=output$recent.to.lost.noslopes.coefficients['raceblack'],
                                                                hispanic.intercept=output$recent.to.lost.noslopes.coefficients['racehispanic'],
                                                                other.intercept=0,
                                                                
                                                                age1.intercept=output$recent.to.lost.noslopes.coefficients['age.category13-25'],
                                                                age2.intercept=output$recent.to.lost.noslopes.coefficients['age.category25-35'],
                                                                age3.intercept=0,
                                                                age4.intercept=output$recent.to.lost.noslopes.coefficients['age.category45-55'],
                                                                age5.intercept=output$recent.to.lost.noslopes.coefficients['age.category55+'],
                                                                
                                                                total.slope = output$recent.to.lost.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up durably-suppressed-to-failing")
    cm$durably.suppressed.to.failing = setup.logistic.model(anchor.year=output$anchor.year,
                                                             min.proportion=min.durably.suppressed.to.failing.proportion,     
                                                             specification=specification,
                                                             
                                                             total.intercept = output$durable.to.failing.noslopes.coefficients['(Intercept)'],
                                                             msm.intercept = 0,
                                                             msm.idu.intercept = output$durable.to.failing.noslopes.coefficients['sex.riskmsm_idu'],
                                                             heterosexual.intercept = 0,
                                                             male.heterosexual.intercept=output$durable.to.failing.noslopes.coefficients['sex.riskheterosexual_male'],
                                                             female.heterosexual.intercept=output$durable.to.failing.noslopes.coefficients['sex.riskheterosexual_female'],
                                                             idu.intercept = 0,
                                                             male.idu.intercept = output$durable.to.failing.noslopes.coefficients['sex.riskidu_male'],
                                                             female.idu.intercept = output$durable.to.failing.noslopes.coefficients['sex.riskidu_female'],
                                                             
                                                             black.intercept=output$durable.to.failing.noslopes.coefficients['raceblack'],
                                                             hispanic.intercept=output$durable.to.failing.noslopes.coefficients['racehispanic'],
                                                             other.intercept=0,
                                                             
                                                             age1.intercept=output$durable.to.failing.noslopes.coefficients['age.category13-25'],
                                                             age2.intercept=output$durable.to.failing.noslopes.coefficients['age.category25-35'],
                                                             age3.intercept=0,
                                                             age4.intercept=output$durable.to.failing.noslopes.coefficients['age.category45-55'],
                                                             age5.intercept=output$durable.to.failing.noslopes.coefficients['age.category55+'],
                                                             
                                                             total.slope = output$durable.to.failing.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up durably-suppressed-to-lost")
    cm$durably.suppressed.to.disengaged = setup.logistic.model(anchor.year=output$anchor.year,
                                                                max.proportion=1-min.durably.suppressed.retention,
                                                                min.proportion=1-max.durably.suppressed.retention,     
                                                                specification=specification,
                                                                
                                                                total.intercept = output$durable.to.lost.noslopes.coefficients['(Intercept)'],
                                                                msm.intercept = 0,
                                                                msm.idu.intercept = output$durable.to.lost.noslopes.coefficients['sex.riskmsm_idu'],
                                                                heterosexual.intercept = 0,
                                                                male.heterosexual.intercept=output$durable.to.lost.noslopes.coefficients['sex.riskheterosexual_male'],
                                                                female.heterosexual.intercept=output$durable.to.lost.noslopes.coefficients['sex.riskheterosexual_female'],
                                                                idu.intercept = 0,
                                                                male.idu.intercept = output$durable.to.lost.noslopes.coefficients['sex.riskidu_male'],
                                                                female.idu.intercept = output$durable.to.lost.noslopes.coefficients['sex.riskidu_female'],
                                                                
                                                                black.intercept=output$durable.to.lost.noslopes.coefficients['raceblack'],
                                                                hispanic.intercept=output$durable.to.lost.noslopes.coefficients['racehispanic'],
                                                                other.intercept=0,
                                                                
                                                                age1.intercept=output$durable.to.lost.noslopes.coefficients['age.category13-25'],
                                                                age2.intercept=output$durable.to.lost.noslopes.coefficients['age.category25-35'],
                                                                age3.intercept=0,
                                                                age4.intercept=output$durable.to.lost.noslopes.coefficients['age.category45-55'],
                                                                age5.intercept=output$durable.to.lost.noslopes.coefficients['age.category55+'],
                                                                
                                                                total.slope = output$durable.to.lost.noslopes.coefficients['relative.year'])
    
    if (verbose)
      print("Setting up disengaged-to-reengage")
    cm$reengagement = setup.logistic.model(anchor.year=output$anchor.year,
                                                 max.proportion=max.reengaged.proportion,     
                                           specification=specification,
                                                 
                                                 total.intercept = output$disengaged.noslopes.coefficients['(Intercept)'],
                                                 msm.intercept = 0,
                                                 msm.idu.intercept = output$disengaged.noslopes.coefficients['sex.riskmsm_idu'],
                                                 heterosexual.intercept = 0,
                                                 male.heterosexual.intercept=output$disengaged.noslopes.coefficients['sex.riskheterosexual_male'],
                                                 female.heterosexual.intercept=output$disengaged.noslopes.coefficients['sex.riskheterosexual_female'],
                                                 idu.intercept = 0,
                                                 male.idu.intercept = output$disengaged.noslopes.coefficients['sex.riskidu_male'],
                                                 female.idu.intercept = output$disengaged.noslopes.coefficients['sex.riskidu_female'],
                                                 
                                                 black.intercept=output$disengaged.noslopes.coefficients['raceblack'],
                                                 hispanic.intercept=output$disengaged.noslopes.coefficients['racehispanic'],
                                                 other.intercept=0,
                                                 
                                                 age1.intercept=output$disengaged.noslopes.coefficients['age.category13-25'],
                                                 age2.intercept=output$disengaged.noslopes.coefficients['age.category25-35'],
                                                 age3.intercept=0,
                                                 age4.intercept=output$disengaged.noslopes.coefficients['age.category45-55'],
                                                 age5.intercept=output$disengaged.noslopes.coefficients['age.category55+'],
                                                 
                                                 total.slope = output$disengaged.noslopes.coefficients['relative.year'])
    
    # Return
    cm
}


##----------------------------##
##-- SET UP START ART MODEL --##
##----------------------------##

setup.start.art.model <- function(cm,
                                  specification)
{
    YEAR = 2010
#    LATENCY = 10/12 #10 months
    LATENCY = mean(c(1.54, 1.48)) #1.5 years
    
    rate = 1/LATENCY
    p = 1-exp(-rate)
    
    cm$start.art = setup.logistic.model(anchor.year=YEAR,
                                                  specification = specification,
                                                  max.proportion=1,
                                                  
                                                  total.intercept = logit(p),
                                                  total.slope = 0
    )
    
    cm
}


##--------------------------##
##-- SET UP LINKAGE MODEL --##
##--------------------------##

run.linkage.regressions <- function(cm,
                                    dir='cleaned_data',
                                    anchor.year=2020,
                                    max.linked.proportion=0.95,
                                    specification=get.specification.for.version('collapsed_1.0')
)
{
    df = read.csv(file.path(dir, 'continuum/national/linkage/national_linkage.csv'),
                  stringsAsFactors = F, header = F)
    dimnames(df)[[1]] = df[,1]
    
    n.mask = df[2,]=='new'
    p.mask = df[2,]=='linked'
    
    
    #-- Fit age --#
    
    df.age = rbind(data.frame(value=as.numeric(df['13-24',p.mask]),
                              n=as.numeric(df['13-24',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=1,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['25-34',p.mask]),
                              n=as.numeric(df['25-34',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=1,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['35-44',p.mask]),
                              n=as.numeric(df['35-44',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=1,
                              age4=0,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['45-54',p.mask]),
                              n=as.numeric(df['45-54',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=1,
                              age5=0,
                              total=0),
                   data.frame(value=as.numeric(df['>=55',p.mask]),
                              n=as.numeric(df['>=55',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=1,
                              total=0),
                   data.frame(value=as.numeric(df['total',p.mask]),
                              n=as.numeric(df['total',n.mask]),
                              year=as.numeric(df['year',p.mask])-anchor.year,
                              age1=0,
                              age2=0,
                              age3=0,
                              age4=0,
                              age5=0,
                              total=1))
    
    df.age$value = pmin(0.999, df.age$value/max.linked.proportion)
    
    fit.age = glm(value ~ age1 + age2 + age3 + age4 + age5 +
                      year + year:age1 + year:age2 + year:age3 + year:age4 + year:age5, 
                  data=df.age, weights = df.age$n,
                  family='binomial')
    
    
    #-- Fit Race --#
    
    df.race = rbind(data.frame(value=as.numeric(df['black',p.mask]),
                               n=as.numeric(df['black',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=1,
                               hispanic=0,
                               other=0,
                               total=0),
                    data.frame(value=as.numeric(df['hispanic',p.mask]),
                               n=as.numeric(df['hispanic',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=1,
                               other=0,
                               total=0),
                    data.frame(value=as.numeric(df['other',p.mask]),
                               n=as.numeric(df['other',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=0,
                               other=1,
                               total=0),
                    data.frame(value=as.numeric(df['total',p.mask]),
                               n=as.numeric(df['total',n.mask]),
                               year=as.numeric(df['year',p.mask])-anchor.year,
                               black=0,
                               hispanic=0,
                               other=0,
                               total=1))
    
    df.race$value = pmin(0.999, df.race$value/max.linked.proportion)
    
    fit.race = glm(value ~ black + hispanic + other + year + year:black + year:hispanic + year:other,
                   data=df.race, weights = df.race$n,
                   family='binomial')
    
    
    #-- Fit Sex/Risk Factor --#
    
    df.sex.risk = rbind(data.frame(value=as.numeric(df['msm',p.mask]),
                                   n=as.numeric(df['msm',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=1,
                                   msm_idu=0,
                                   het_male=0,
                                   het_female=0,
                                   idu_male=0,
                                   idu_female=0),
                        data.frame(value=as.numeric(df['msm-idu',p.mask]),
                                   n=as.numeric(df['msm-idu',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=0,
                                   msm_idu=1,
                                   het_male=0,
                                   het_female=0,
                                   idu_male=0,
                                   idu_female=0),
                        data.frame(value=as.numeric(df['het-male',p.mask]),
                                   n=as.numeric(df['het-male',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=0,
                                   msm_idu=0,
                                   het_male=1,
                                   het_female=0,
                                   idu_male=0,
                                   idu_female=0),
                        data.frame(value=as.numeric(df['het-female',p.mask]),
                                   n=as.numeric(df['het-female',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=0,
                                   msm_idu=0,
                                   het_male=0,
                                   het_female=1,
                                   idu_male=0,
                                   idu_female=0),
                        data.frame(value=as.numeric(df['idu-male',p.mask]),
                                   n=as.numeric(df['idu-male',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=0,
                                   msm_idu=0,
                                   het_male=0,
                                   het_female=0,
                                   idu_male=1,
                                   idu_female=0),
                        data.frame(value=as.numeric(df['idu-female',p.mask]),
                                   n=as.numeric(df['idu-female',n.mask]),
                                   year=as.numeric(df['year',p.mask])-anchor.year,
                                   msm=0,
                                   msm_idu=0,
                                   het_male=0,
                                   het_female=0,
                                   idu_male=0,
                                   idu_female=1))
    
    df.sex.risk$value = pmin(0.999, df.sex.risk$value/max.linked.proportion)
    
    fit.sex.risk = glm(value ~ msm + msm_idu + het_male + het_female + idu_male + idu_female + 
                           year:msm + year:msm_idu + year:het_male + year:het_female + year:idu_male + year:idu_female +0,
                       data=df.sex.risk, weights = df.sex.risk$n,
                       family='binomial')
    
    
    #-- Put it all together --#
    
    cm$linkage = list(log.ors = numeric())
    
    dim.names = list(age=specification$AGES$labels, race=specification$RACES, sex=specification$SEXES, risk=specification$RISK_STRATA)
    cm$linkage$stratified.log.odds.slope = cm$linkage$stratified.log.odds.intercept =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Age
    for (age in 1:5)
    {
        cm$linkage$stratified.log.odds.intercept[age,,,] = cm$linkage$stratified.log.odds.intercept[age,,,] +
            fit.age$coefficients[paste0('age',age)]
        cm$linkage$stratified.log.odds.slope[age,,,] = cm$linkage$stratified.log.odds.slope[age,,,] +
            fit.age$coefficients[paste0('age',age,':year')]
        
        cm$linkage$log.ors[paste0('age',age)] = fit.age$coefficients[paste0('age',age)]
        cm$linkage$log.ors[paste0('age',age,"_slope")] = fit.age$coefficients[paste0('age',age,':year')]
    }
    
    # Race
    cm$linkage$stratified.log.odds.intercept[,'black',,] = cm$linkage$stratified.log.odds.intercept[,'black',,] +
        fit.race$coefficients['black']
    cm$linkage$stratified.log.odds.slope[,'black',,] = cm$linkage$stratified.log.odds.slope[,'black',,] +
        fit.race$coefficients['black:year']
    cm$linkage$log.ors['black'] = fit.race$coefficients['black']
    cm$linkage$log.ors['black_slope'] = fit.race$coefficients['black:year']
    
    cm$linkage$stratified.log.odds.intercept[,'hispanic',,] = cm$linkage$stratified.log.odds.intercept[,'hispanic',,] +
        fit.race$coefficients['hispanic']
    cm$linkage$stratified.log.odds.slope[,'hispanic',,] = cm$linkage$stratified.log.odds.slope[,'hispanic',,] +
        fit.race$coefficients['hispanic:year']
    cm$linkage$log.ors['hispanic'] = fit.race$coefficients['hispanic']
    cm$linkage$log.ors['hispanic_slope'] = fit.race$coefficients['hispanic:year']
    
    cm$linkage$stratified.log.odds.intercept[,'other',,] = cm$linkage$stratified.log.odds.intercept[,'other',,] +
        fit.race$coefficients['other']
    cm$linkage$stratified.log.odds.slope[,'other',,] = cm$linkage$stratified.log.odds.slope[,'other',,] +
        fit.race$coefficients['other:year']
    cm$linkage$log.ors['other'] = fit.race$coefficients['other']
    cm$linkage$log.ors['other_slope'] = fit.race$coefficients['other:year']
    
    
    # Sex Risk
    idu = c('active_IDU','IDU_in_remission')
    non.idu = 'never_IDU'
    
    cm$linkage$stratified.log.odds.intercept[,,'msm',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'msm',non.idu] +
        fit.sex.risk$coefficients['msm']
    cm$linkage$stratified.log.odds.slope[,,'msm',non.idu] = cm$linkage$stratified.log.odds.slope[,,'msm',non.idu] +
        fit.sex.risk$coefficients['msm:year']
    cm$linkage$log.ors['msm'] = fit.sex.risk$coefficients['msm']
    cm$linkage$log.ors['msm_slope'] = fit.sex.risk$coefficients['msm:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'msm',idu] = cm$linkage$stratified.log.odds.intercept[,,'msm',idu] +
        fit.sex.risk$coefficients['msm_idu']
    cm$linkage$stratified.log.odds.slope[,,'msm',idu] = cm$linkage$stratified.log.odds.slope[,,'msm',idu] +
        fit.sex.risk$coefficients['msm_idu:year']
    cm$linkage$log.ors['msm_idu'] = fit.sex.risk$coefficients['msm_idu']
    cm$linkage$log.ors['msm_idu_slope'] = fit.sex.risk$coefficients['msm_idu:year']
    
    
    cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',non.idu] +
        fit.sex.risk$coefficients['het_male']
    cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',non.idu] = cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',non.idu] +
        fit.sex.risk$coefficients['het_male:year']
    cm$linkage$log.ors['het_male'] = fit.sex.risk$coefficients['het_male']
    cm$linkage$log.ors['het_male_slope'] = fit.sex.risk$coefficients['het_male:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',idu] = cm$linkage$stratified.log.odds.intercept[,,'heterosexual_male',idu] +
        fit.sex.risk$coefficients['idu_male']
    cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',idu] = cm$linkage$stratified.log.odds.slope[,,'heterosexual_male',idu] +
        fit.sex.risk$coefficients['idu_male:year']
    cm$linkage$log.ors['idu_male'] = fit.sex.risk$coefficients['idu_male']
    cm$linkage$log.ors['idu_male_slope'] = fit.sex.risk$coefficients['idu_male:year']
    
    
    cm$linkage$stratified.log.odds.intercept[,,'female',non.idu] = cm$linkage$stratified.log.odds.intercept[,,'female',non.idu] +
        fit.sex.risk$coefficients['het_female']
    cm$linkage$stratified.log.odds.slope[,,'female',non.idu] = cm$linkage$stratified.log.odds.slope[,,'female',non.idu] +
        fit.sex.risk$coefficients['het_female:year']
    cm$linkage$log.ors['het_female'] = fit.sex.risk$coefficients['het_female']
    cm$linkage$log.ors['het_female_slope'] = fit.sex.risk$coefficients['het_female:year']
    
    cm$linkage$stratified.log.odds.intercept[,,'female',idu] = cm$linkage$stratified.log.odds.intercept[,,'female',idu] +
        fit.sex.risk$coefficients['idu_female']
    cm$linkage$stratified.log.odds.slope[,,'female',idu] = cm$linkage$stratified.log.odds.slope[,,'female',idu] +
        fit.sex.risk$coefficients['idu_female:year']
    cm$linkage$log.ors['idu_female'] = fit.sex.risk$coefficients['idu_female']
    cm$linkage$log.ors['idu_female_slope'] = fit.sex.risk$coefficients['idu_female:year']
    
    
   # names(cm$linkage)[names(cm$linkage)=='stratified.log.odds.intercept'] = 'intercept'
#    names(cm$linkage)[names(cm$linkage)=='stratified.log.odds.slope'] = 'slope'
    
    #-- Return it --#
    cm$linkage = create.logistic.model(intercept = cm$linkage$stratified.log.odds.intercept,
                                       slope = cm$linkage$stratified.log.odds.slope,
                                       anchor.year = anchor.year,
                                       max.proportion = max.linked.proportion,
                                       min.proportion = 0,
                                       log.ors = cm$linkage$log.ors)
    cm
}

##------------------------##
##-- SET UP SUPPRESSION --##
##------------------------##

run.suppression.regressions <- function(cm,
                                        national.surveillance,
                                        max.suppressed.proportion=0.9,
                                        anchor.year=2020,
                                        specification = get.specification.for.version('collapsed_1.0'))
{   
    #-- Fit Sex x Risk --#
    
    df = melt(national.surveillance$suppression.sex.risk)
    counts = reshape2::melt(national.surveillance$prevalence.for.continuum.sex.risk)
    df$n = counts$value
    df = df[df$sex != 'female' | (df$risk!='msm' & df$risk!='msm_idu'),]
    
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df$year = df$year - anchor.year
    
    df$msm = as.numeric(df$risk=='msm')
    df$msm_idu = as.numeric(df$risk=='msm_idu')
    df$heterosexual_female = as.numeric(df$risk=='heterosexual' & df$sex=='female')
    df$heterosexual_male = as.numeric(df$risk=='heterosexual' & df$sex=='male')
    df$idu_female = as.numeric(df$risk=='idu' & df$sex=='female')
    df$idu_male = as.numeric(df$risk=='idu' & df$sex=='male')
    
    fit.sex.risk = suppressWarnings(glm(value ~ msm + year:msm +
                                            msm_idu + year:msm_idu +
                                            heterosexual_female + year:heterosexual_female +
                                            heterosexual_male + year:heterosexual_male +
                                            idu_female + year:idu_female +
                                            idu_male + year:idu_male +
                                            0,
                                        data=df, family='binomial'))
    
    #-- The 'All' Data Frame --#
    
    df.all = reshape2::melt(national.surveillance$suppression.all)
    df.all$n = national.surveillance$prevalence.for.continuum.all[,1]
    
    df.all$value = pmin(df.all$value / max.suppressed.proportion, .9999)
    
    #-- Fit Race --#
    
    df = reshape2::melt(national.surveillance$suppression.race)
    df$n = reshape2::melt(national.surveillance$prevalence.for.continuum.all)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          race='all',
                          value=df.all$value,
                          n=df.all$n))
    
    df$black = as.numeric(df$race=='black')
    df$hispanic = as.numeric(df$race=='hispanic')
    df$other = as.numeric(df$race=='other')
    
    df$year = df$year - anchor.year
    
    fit.race = suppressWarnings(glm(value ~ black + hispanic + other +
                                        black:year + hispanic:year + other:year + year,
                                    data=df, family='binomial', weight=n))
    
    #-- Fit Age --#
    
    df = reshape2::melt(national.surveillance$suppression.age)
    df$n = reshape2::melt(national.surveillance$prevalence.for.continuum.age)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    ages = sort(unique(as.character(df$age)))
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          age='all',
                          value=df.all$value,
                          n=df.all$n))
    
    
    for (age in 1:length(ages))
        df[,paste0('age',age)] = as.numeric(df$age==ages[age])
    
    df$year = df$year - anchor.year
    
    ff = as.formula(paste0('value ~ ',
                           paste0('age', 1:length(ages), collapse=' + '),
                           " + ",
                           paste0('year:age', 1:length(ages), collapse=' + '),
                           ' + year'))
    
    fit.age = suppressWarnings(glm(ff, data=df, family='binomial'))
    
    
    
    #-- Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk] --#
    cm$suppression = list()
    
    # Save the log ORs for readability/reference (NOT used by model)
    cm$suppression$log.ors = numeric()
    
    dim.names = list(age=specification$AGES$labels, race=specification$RACES, sex=specification$SEXES, risk=specification$RISK_STRATA)
    cm$suppression$stratified.log.odds.intercept = cm$suppression$stratified.log.odds.slope =
        array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    n.ages = length(dim.names[['age']])
    for (age in 1:n.ages)
    {
        cm$suppression$stratified.log.odds.intercept[age,,,] = cm$suppression$stratified.log.odds.intercept[age,,,] + 
            fit.age$coefficients[paste0('age',age)]
        cm$suppression$stratified.log.odds.slope[age,,,] = cm$suppression$stratified.log.odds.slope[age,,,] + 
            fit.age$coefficients[paste0('age',age,':year')]
    }
    cm$suppression$log.ors[paste0('age',1:n.ages)] = fit.age$coefficients[paste0('age',1:n.ages)]
    cm$suppression$log.ors[paste0('age',1:n.ages,"_slope")] = fit.age$coefficients[paste0('age',1:n.ages,":year")]
    
    for (race in specification$RACES)
    {
        cm$suppression$stratified.log.odds.intercept[,race,,] = cm$suppression$stratified.log.odds.intercept[,race,,] + 
            fit.race$coefficients[race]
        cm$suppression$stratified.log.odds.slope[,race,,] = cm$suppression$stratified.log.odds.slope[,race,,] + 
            fit.race$coefficients[paste0(race,":year")]
    }
    cm$suppression$log.ors[specification$RACES] = fit.race$coefficients[specification$RACES]
    cm$suppression$log.ors[paste0(specification$RACES,"_slope")] = fit.race$coefficients[paste0(specification$RACES,":year")]
    
    idu.strata = setdiff(specification$RISK_STRATA, 'never_IDU')
    non.idu.strata = 'never_IDU'
    
    
    # Non-IDU, heterosexual female
    cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_female']
    cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_female']
    cm$suppression$log.ors['heterosexual_female'] = fit.sex.risk$coefficients['heterosexual_female']
    cm$suppression$log.ors['heterosexual_female_slope'] = fit.sex.risk$coefficients['year:heterosexual_female']
    
    # Non-IDU, heterosexual male
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['heterosexual_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] + 
        fit.sex.risk$coefficients['year:heterosexual_male']
    cm$suppression$log.ors['heterosexual_male'] = fit.sex.risk$coefficients['heterosexual_male']
    cm$suppression$log.ors['heterosexual_male_slope'] = fit.sex.risk$coefficients['year:heterosexual_male']
    
    # IDU female
    cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['idu_female']
    cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'female',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_female']
    cm$suppression$log.ors['idu_female'] = fit.sex.risk$coefficients['idu_female']
    cm$suppression$log.ors['idu_female_slope'] = fit.sex.risk$coefficients['year:idu_female']
    
    # IDU, heterosexual male
    cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['idu_male']
    cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'heterosexual_male',idu.strata] + 
        fit.sex.risk$coefficients['year:idu_male']
    cm$suppression$log.ors['idu_male'] = fit.sex.risk$coefficients['idu_male']
    cm$suppression$log.ors['idu_male_slope'] = fit.sex.risk$coefficients['year:idu_male']
    
    # Non-IDU MSM
    cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm']
    cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'msm',non.idu.strata] + 
        fit.sex.risk$coefficients['msm:year']
    cm$suppression$log.ors['msm'] = fit.sex.risk$coefficients['msm']
    cm$suppression$log.ors['msm_slope'] = fit.sex.risk$coefficients['msm:year']
    
    # IDU MSM
    cm$suppression$stratified.log.odds.intercept[,,'msm',idu.strata] = 
        cm$suppression$stratified.log.odds.intercept[,,'msm',idu.strata] + 
        fit.sex.risk$coefficients['msm_idu']
    cm$suppression$stratified.log.odds.slope[,,'msm',idu.strata] = 
        cm$suppression$stratified.log.odds.slope[,,'msm',idu.strata] + 
        fit.sex.risk$coefficients['year:msm_idu']
    cm$suppression$log.ors['msm_idu'] = fit.sex.risk$coefficients['msm_idu']
    cm$suppression$log.ors['msm_idu_slope'] = fit.sex.risk$coefficients['year:msm_idu']
    
    
 #   names(cm$suppression)[names(cm$suppression)=='stratified.log.odds.intercept'] = 'intercept'
 #   names(cm$suppression)[names(cm$suppression)=='stratified.log.odds.slope'] = 'slope'
    
    #-- Return --#
    cm$suppression = create.logistic.model(intercept = cm$suppression$stratified.log.odds.intercept,
                                           slope = cm$suppression$stratified.log.odds.slope,
                                           anchor.year=anchor.year,
                                           max.proportion=max.suppressed.proportion,
                                           min.proportion=0,
                                           log.ors = cm$suppression$log.ors)
    cm
}

##--------------------##
##-- SET UP TESTING --##
##--------------------##

run.testing.regressions <- function(cm,
                                    dir='cleaned_data',
                                    verbose=T,
                                    anchor.year=2020,
                                    max.tested.proportion=0.9,
                                    specification=get.specification.for.version('collapsed_1.0'))
{
    # We our working towards a model such that
    # logit(p_12mo_s) = logit(p_12mo_prime_s) + mu
    # for some stratum s
    # where p_12mo represents best-guess prob testing (average of brfss and nhbs)
    #  and p_12mo_prime represents the (overly optimistic) prob testing by nhbs
    # we let logit(p_12mo_prime_s) = X beta
    
    #-- STEP 0: Set-Up --#
    #Read in our three main files:
    #NHBS Testing (ever tested and tested in past year, by msa and stratified)
    dfs = read.nhbs.testing(dir=file.path(dir, 'continuum/national/testing'), verbose=verbose)
    #BRFSS at MSA level (total ever tested)
    brfss.msa = read.brfss.msa(file.path(dir, 'continuum/msa/BRFSS_msa.csv'))
    #BRFSS at state level (ever tested by race)
    
    cm$testing = list()
    
    #Save the BRFSS files
    cm$testing$total.ever.tested = brfss.msa
    names(cm$testing$total.ever.tested)[names(cm$testing$total.ever.tested)=='msa'] = 'location'
    
    #-- STEP 1: --#
    #Use NHBS data to estimate the relationship between probability of testing in the past year
    # and probability of testing ever (use a constant rate model)
    df.msa = dfs$msa
    fit.12mo.to.ever = lm(log(1-frac.12mo) ~ log(1-frac.ever)+0, data=df.msa)
    #this model assumes a constant rate = -log(1-p)
    #we are now using the reverse regression (ever ~ 12mo) but this is here for backward compatibility
    cm$testing$rate.ever.to.12mo.mult = fit.12mo.to.ever$coefficients[1]
    cm$testing$sd.rate.ever.to.12mo.mult = sqrt(vcov(fit.12mo.to.ever)[1,1])
    
    fit.ever.to.12mo = lm(log(1-frac.ever) ~ log(1-frac.12mo)+0, data=df.msa)
    cm$testing$ever.to.12mo.model = list(mean=fit.ever.to.12mo$coefficients[1],
                                         sd=sqrt(vcov(fit.ever.to.12mo)[1,1]))
    
    #-- STEP 2: Link NHBS and BRFSS to correct the overly optimistic NHBS estimates --#
    # This gives up the mu for our overriding equation
    
    df.nhbs.vs.brfss = data.frame(msa=df.msa$msa,
                                  nhbs.ever=df.msa$frac.ever,
                                  nhbs.12mo=df.msa$frac.12mo,
                                  year=df.msa$year,
                                  brfss.ever=sapply(1:dim(df.msa)[1], function(i){
                                      mask = brfss.msa$msa==df.msa$msa[i] & brfss.msa$year==df.msa$year[i]
                                      brfss.msa$frac.ever[i]
                                  }))
    
    logit = function(p){log(p)-log(1-p)}
    df.nhbs.vs.brfss$est.nhbs.12mo = 1 - (1-df.nhbs.vs.brfss$nhbs.ever) ^ cm$testing$rate.ever.to.12mo.mult
    df.nhbs.vs.brfss$est.brfss.12mo = 1 - (1-df.nhbs.vs.brfss$brfss.ever) ^ cm$testing$rate.ever.to.12mo.mult
    
    df.nhbs.vs.brfss$mean.ever = (df.nhbs.vs.brfss$brfss.ever + df.nhbs.vs.brfss$nhbs.ever) / 2
    df.nhbs.vs.brfss$est.mean.12mo = 1 - (1-df.nhbs.vs.brfss$mean.ever) ^ cm$testing$rate.ever.to.12mo.mult
    
    base.log.or = mean(logit(df.nhbs.vs.brfss$est.mean.12mo) - logit(df.nhbs.vs.brfss$est.nhbs.12mo))
    #    base.log.or = 0
    
    #-- STEP 3: The Regression by Strata --#
    df.all = dfs$all
    df.all$rate = -log(1-df.all$frac.12mo)
    
    df.all$frac.12mo = pmin(df.all$frac.12mo / max.tested.proportion, .9999)
    df.all$year = df.all$year - anchor.year
    
    #massage our variables
    df.all$black = as.numeric(df.all$race=='black')
    df.all$hispanic = as.numeric(df.all$race=='hispanic')
    df.all$other = as.numeric(df.all$race=='other')
    
    df.all$msm = as.numeric(df.all$risk=='msm')
    df.all$idu = as.numeric(df.all$risk=='idu')
    df.all$heterosexual = as.numeric(df.all$risk=='heterosexual')
    
    df.all$age1 = as.numeric(df.all$age=='age1')
    df.all$age2 = as.numeric(df.all$age=='age2')
    df.all$age3 = as.numeric(df.all$age=='age3')
    df.all$age4 = as.numeric(df.all$age=='age4')
    df.all$age5 = as.numeric(df.all$age=='age5')
    
    df.all$female = as.numeric(df.all$sex=='female')
    df.all$male = as.numeric(df.all$sex=='male' & df.all$risk != 'msm')
    
    #Run the regression - an intercept and slope for each risk factor, 
    # plus ORs and slopes for age/race/sex
    fit.stratified.testing = suppressWarnings(glm(frac.12mo ~ +0 +
                                                      msm + idu + heterosexual +
                                                      msm:year + idu:year + heterosexual:year +
                                                      black + hispanic + other +
                                                      black:year + hispanic:year + other:year +
                                                      female + male +
                                                      female:year + male:year +
                                                      age1 + age2 + age3 + age4 + age5 +
                                                      age1:year + age2:year + age3:year + age4:year + age5:year,
                                                  data=df.all, family='binomial', weight=n))
    
    #-- STEP 4: PUT IT ALL TOGETHER --#
    
    # Save the log ORs for readability/reference (NOT used by model)
    cm$testing$log.ors = numeric()
    
    # Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk]
    dim.names = list(age=specification$AGES$labels, race=specification$RACES, sex=specification$SEXES, risk=specification$RISK_STRATA)
    cm$testing$stratified.log.odds.intercept = array(base.log.or, dim=sapply(dim.names, length), dimnames=dim.names)
    cm$testing$stratified.log.odds.slope = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    n.ages = length(dim.names[['age']])
    for (age in 1:n.ages)
    {
        cm$testing$stratified.log.odds.intercept[age,,,] = cm$testing$stratified.log.odds.intercept[age,,,] + 
            fit.stratified.testing$coefficients[paste0('age',age)]
        cm$testing$stratified.log.odds.slope[age,,,] = cm$testing$stratified.log.odds.slope[age,,,] + 
            fit.stratified.testing$coefficients[paste0('year:age',age)]
    }
    cm$testing$log.ors[paste0('age',1:n.ages)] = fit.stratified.testing$coefficients[paste0('age',1:n.ages)]
    cm$testing$log.ors[paste0('age',1:n.ages,'_slope')] = fit.stratified.testing$coefficients[paste0('year:age',1:n.ages)]
    
    
    for (race in specification$RACES)
    {
        cm$testing$stratified.log.odds.intercept[,race,,] = cm$testing$stratified.log.odds.intercept[,race,,] + 
            fit.stratified.testing$coefficients[race]
        cm$testing$stratified.log.odds.slope[,race,,] = cm$testing$stratified.log.odds.slope[,race,,] + 
            fit.stratified.testing$coefficients[paste0('year:',race)]
    }
    cm$testing$log.ors[paste0(specification$RACES)] = fit.stratified.testing$coefficients[specification$RACES]
    cm$testing$log.ors[paste0(specification$RACES,'_slope')] = fit.stratified.testing$coefficients[paste0('year:',specification$RACES)]
    
    
    cm$testing$stratified.log.odds.intercept[,,'female',] = cm$testing$stratified.log.odds.intercept[,,'female',] + 
        fit.stratified.testing$coefficients['female']
    cm$testing$stratified.log.odds.slope[,,'female',] = cm$testing$stratified.log.odds.slope[,,'female',] + 
        fit.stratified.testing$coefficients['year:female']
    cm$testing$log.ors['female'] = fit.stratified.testing$coefficients['female']
    cm$testing$log.ors['female_slope'] = fit.stratified.testing$coefficients['year:female']
    
    
    cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] = cm$testing$stratified.log.odds.intercept[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['male']
    cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] = cm$testing$stratified.log.odds.slope[,,'heterosexual_male',] + 
        fit.stratified.testing$coefficients['year:male']
    cm$testing$log.ors['heterosexual_male'] = fit.stratified.testing$coefficients['male']
    cm$testing$log.ors['heterosexual_male_slope'] = fit.stratified.testing$coefficients['year:male']
    
    non.active.idu = setdiff(specification$RISK_STRATA, 'active_IDU')
    
    cm$testing$stratified.log.odds.intercept[,,'msm',] = cm$testing$stratified.log.odds.intercept[,,'msm',] + 
        fit.stratified.testing$coefficients['msm']
    cm$testing$stratified.log.odds.slope[,,'msm',] = cm$testing$stratified.log.odds.slope[,,'msm',] + 
        fit.stratified.testing$coefficients['msm:year']
    cm$testing$log.ors['msm'] = fit.stratified.testing$coefficients['msm']
    cm$testing$log.ors['msm_slope'] = fit.stratified.testing$coefficients['msm:year']
    
    #we're going to treat msm+idu as msm (by omitting them from the code below adding idu log ORs)
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),'active_IDU'] + 
        fit.stratified.testing$coefficients['idu:year']
    cm$testing$log.ors['idu'] = fit.stratified.testing$coefficients['idu']
    cm$testing$log.ors['idu_slope'] = fit.stratified.testing$coefficients['idu:year']
    
    cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.intercept[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual']
    cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] = 
        cm$testing$stratified.log.odds.slope[,,c('female','heterosexual_male'),non.active.idu] + 
        fit.stratified.testing$coefficients['heterosexual:year']
    cm$testing$log.ors['heterosexual'] = fit.stratified.testing$coefficients['heterosexual']
    cm$testing$log.ors['heterosexual_slope'] = fit.stratified.testing$coefficients['heterosexual:year']
    
    cm$testing$log.ors['base'] = base.log.or
    
 #   names(cm$testing)[names(cm$testing)=='stratified.log.odds.intercept'] = 'intercept'
 #   names(cm$testing)[names(cm$testing)=='stratified.log.odds.slope'] = 'slope'
    
    #-- Return --#
    cm$testing = create.logistic.model(intercept = cm$testing$stratified.log.odds.intercept,
                                       slope = cm$testing$stratified.log.odds.slope,
                                       anchor.year=anchor.year,
                                       max.proportion=max.tested.proportion,
                                       min.proportion=0,
                                       log.ors = cm$testing$log.ors)
    cm
}

##--------------------##
##-- SET-UP HELPERS --##
##--------------------##



setup.logistic.model <- function(anchor.year,
                                 min.proportion=0,
                                 max.proportion=1,
                                 specification,
                                 
                                 # Intercepts
                                 total.intercept = 0,
                                 msm.intercept = 0,
                                 msm.idu.intercept = 0,
                                 heterosexual.intercept = 0,
                                 male.heterosexual.intercept=0,
                                 female.heterosexual.intercept=0,
                                 idu.intercept = 0,
                                 male.idu.intercept = 0,
                                 female.idu.intercept = 0,
                                 
                                 black.intercept=0,
                                 hispanic.intercept=0,
                                 other.intercept=0,
                                 
                                 age1.intercept=0,
                                 age2.intercept=0,
                                 age3.intercept=0,
                                 age4.intercept=0,
                                 age5.intercept=0,
                                 
                                 # Slopes
                                 total.slope = 0,
                                 msm.slope = 0,
                                 msm.idu.slope = 0,
                                 heterosexual.slope = 0,
                                 male.heterosexual.slope=0,
                                 female.heterosexual.slope=0,
                                 idu.slope = 0,
                                 male.idu.slope = 0,
                                 female.idu.slope = 0,
                                 
                                 black.slope=0,
                                 hispanic.slope=0,
                                 other.slope=0,
                                 
                                 age1.slope=0,
                                 age2.slope=0,
                                 age3.slope=0,
                                 age4.slope=0,
                                 age5.slope=0,
                                 
                                 # Array Dimensions
                                 idu.in.remission.is.idu=T
                                 
)
{
    create.logistic.model(anchor.year = anchor.year,
                          min.proportion = min.proportion,
                          max.proportion = max.proportion,
                          
                          intercept=setup.array.from.coefficients(
                              specification = specification,
                              
                              all=total.intercept,
                              
                              age1=age1.intercept,
                              age2=age2.intercept,
                              age3=age3.intercept,
                              age4=age4.intercept,
                              age5=age5.intercept,
                              
                              black=black.intercept,
                              hispanic=hispanic.intercept,
                              other=other.intercept,
                              
                              heterosexual = heterosexual.intercept,
                              idu = idu.intercept,
                              
                              heterosexual.male = male.heterosexual.intercept,
                              heterosexual.male.idu = male.idu.intercept,
                              msm = msm.intercept,
                              msm.idu = msm.idu.intercept,
                              female = female.heterosexual.intercept,
                              female.idu = female.idu.intercept,
                              
                              idu.in.remission.is.idu=idu.in.remission.is.idu
                          ),
                          
                          slope = setup.array.from.coefficients(
                              specification = specification,
                              
                              all=total.slope,
                              
                              age1=age1.slope,
                              age2=age2.slope,
                              age3=age3.slope,
                              age4=age4.slope,
                              age5=age5.slope,
                              
                              black=black.slope,
                              hispanic=hispanic.slope,
                              other=other.slope,
                              
                              heterosexual = heterosexual.slope,
                              idu = idu.slope,
                              
                              heterosexual.male = male.heterosexual.slope,
                              heterosexual.male.idu = male.idu.slope,
                              msm = msm.slope,
                              msm.idu = msm.idu.slope,
                              female = female.heterosexual.slope,
                              female.idu = female.idu.slope,
                              
                              idu.in.remission.is.idu=idu.in.remission.is.idu
                          ),
                          
                          log.ors = c(
                              total.intercept = total.intercept,
                              msm.intercept = msm.intercept,
                              msm.idu.intercept = msm.idu.intercept,
                              heterosexual.intercept = heterosexual.intercept,
                              male.heterosexual.intercept=male.heterosexual.intercept,
                              female.heterosexual.intercept=female.heterosexual.intercept,
                              idu.intercept = idu.intercept,
                              male.idu.intercept = male.idu.intercept,
                              female.idu.intercept = female.idu.intercept,
                              
                              black.intercept=black.intercept,
                              hispanic.intercept=hispanic.intercept,
                              other.intercept=other.intercept,
                              
                              age1.intercept=age1.intercept,
                              age2.intercept=age2.intercept,
                              age3.intercept=age3.intercept,
                              age4.intercept=age4.intercept,
                              age5.intercept=age5.intercept,
                              
                              # Slopes
                              total.slope = total.slope,
                              msm.slope = msm.slope,
                              msm.idu.slope = msm.idu.slope,
                              heterosexual.slope = heterosexual.slope,
                              male.heterosexual.slope=male.heterosexual.slope,
                              female.heterosexual.slope=female.heterosexual.slope,
                              idu.slope = idu.slope,
                              male.idu.slope = male.idu.slope,
                              female.idu.slope = female.idu.slope,
                              
                              black.slope=black.slope,
                              hispanic.slope=hispanic.slope,
                              other.slope=other.slope,
                              
                              age1.slope=age1.slope,
                              age2.slope=age2.slope,
                              age3.slope=age3.slope,
                              age4.slope=age4.slope,
                              age5.slope=age5.slope
                          )
    ) 
}

setup.array.from.coefficients <- function(specification,
                                          
                                          all=0,
                                          
                                          age1=0,
                                          age2=0,
                                          age3=0,
                                          age4=0,
                                          age5=0,
                                          
                                          black=0,
                                          hispanic=0,
                                          other=0,
                                          
                                          heterosexual=0,
                                          heterosexual.male=0,
                                          heterosexual.male.idu=0,
                                          msm=0,
                                          
                                          idu=0,
                                          msm.idu=0,
                                          female=0,
                                          female.idu=0,
                                          
                                          idu.in.remission.is.idu)
{
    dim.names = list(age = specification$AGES$labels,
                     race = specification$RACES,
                     sex = specification$SEXES,
                     risk = specification$RISK_STRATA)
    rv = array(all, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Age
    rv[1,,,] = rv[1,,,] + age1
    rv[2,,,] = rv[2,,,] + age2
    rv[3,,,] = rv[3,,,] + age3
    rv[4,,,] = rv[4,,,] + age4
    rv[5,,,] = rv[5,,,] + age5
    
    # Race
    rv[,'black',,] = rv[,'black',,] + black
    rv[,'hispanic',,] = rv[,'hispanic',,] + hispanic
    rv[,'other',,] = rv[,'other',,] + other
    
    # Sex/Risk
    non.idu.states = 'never_IDU'
    if (!idu.in.remission.is.idu)
        non.idu.states = c(non.idu.states, 'IDU_in_remission')
    idu.states = setdiff(specification$RISK_STRATA, non.idu.states)
    
    rv[,,'heterosexual_male',non.idu.states] = rv[,,'heterosexual_male',non.idu.states] + heterosexual.male
    rv[,,'heterosexual_male',idu.states] = rv[,,'heterosexual_male',idu.states] + heterosexual.male.idu
    
    rv[,,'msm',non.idu.states] = rv[,,'msm',non.idu.states] + msm
    rv[,,'msm',idu.states] = rv[,,'msm',idu.states] + msm
    
    rv[,,'female',non.idu.states] = rv[,,'female',non.idu.states] + female
    rv[,,'female',idu.states] = rv[,,'female',idu.states] + female.idu
    
    rv[,,c('heterosexual_male','female'),] = rv[,,c('heterosexual_male','female'),] + heterosexual
    
    rv[,,,idu.states] = rv[,,,idu.states] + idu
    
    # Return
    rv
}


####----------------------------------------####
####----------------------------------------####
####-- FILE READERS and LOW-LEVEL HELPERS --####
####----------------------------------------####
####----------------------------------------####


read.brfss.msa <- function(file='cleaned_data/continuum/msa/BRFSS_msa.csv')
{
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    
    
    msa.n = msa.names(df$LocationID)
    mask = !is.na(msa.n)
    
    rv = data.frame(msa=cbsa.for.msa.name(msa.n[mask]),
                    year=df[mask,'ï..Year'],
                    frac.ever = df$Data_value[mask]/100,
                    sample.size = as.numeric(gsub(',', '', df$Sample_Size[mask])),
                    stringsAsFactors=F)
    rv[!is.na(rv$msa),]
}

read.brfss.state <- function(file='cleaned_data/continuum/state/BRFSS_state.csv')
{
    print("Reading BRFSS State-Level Data...")
    
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    df = df[!is.na(df$Data_value),]
    df$Break_Out = tolower(df$Break_Out)
    df$year = as.character(df[,'ï..Year'])
    df$Sample_Size = as.numeric(gsub(',','',df$Sample_Size))
    df$frac = as.numeric(df$Data_value)/100
    
    
    df.race = df[grepl('race', df$Break_Out_Category, ignore.case = T),]
    df.sex = df[grepl('gender', df$Break_Out_Category, ignore.case = T),]
    df.age = df[grepl('age', df$Break_Out_Category, ignore.case = T),]
    
    dim.names.race = list(year=sort(unique(df$year)),
                          location=sort(unique(df$Locationabbr)),
                          race=c('black','hispanic','other'))
    dim.names.age = c(dim.names.race[1:2],
                      list(age=c('18-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years')))
    dim.names.sex = c(dim.names.race[1:2],
                      list(sex=c('male','female')))
    
    rv = list()
    rv$numerators.race = rv$denominators.race = array(0, dim=sapply(dim.names.race, length), dimnames=dim.names.race)
    rv$numerators.sex = rv$denominators.sex = array(0, dim=sapply(dim.names.sex, length), dimnames=dim.names.sex)
    rv$numerators.age = rv$denominators.age = array(0, dim=sapply(dim.names.age, length), dimnames=dim.names.age)
    
    
    print("Parsing BRFSS State-Level Data...")
    
    
    for (i in 1:dim(df.race)[1])
    {
        if (df.race$Break_Out[i]=='hispanic')
            race = 'hispanic'
        else if (df.race$Break_Out[i]=='black, non-hispanic')
            race = 'black'
        else
            race = 'other'
        
        rv$numerators.race[df.race$year[i], df.race$Locationabbr[i], race] = 
            rv$numerators.race[df.race$year[i], df.race$Locationabbr[i], race] + 
            df.race$Sample_Size[i] * df.race$frac[i]
        
        rv$denominators.race[df.race$year[i], df.race$Locationabbr[i], race] = 
            rv$denominators.race[df.race$year[i], df.race$Locationabbr[i], race] + 
            df.race$Sample_Size[i]
    }
    missing.mask = rv$denominators.race == 0
    rv$denominators.race[missing.mask] = rv$numerators.race[missing.mask] = NA
    
    age.mapping = c('18-24'=1,
                    '25-34'=2,
                    '35-44'=3,
                    '45-54'=4,
                    '55-64'=5,
                    '65+'=5)
    for (i in 1:dim(df.age)[1])
    {
        age = age.mapping[df.age$Break_Out[i]]
        
        rv$numerators.age[df.age$year[i], df.age$Locationabbr[i], age] = 
            rv$numerators.age[df.age$year[i], df.age$Locationabbr[i], age] + 
            df.age$Sample_Size[i] * df.age$frac[i]
        
        rv$denominators.age[df.age$year[i], df.age$Locationabbr[i], age] = 
            rv$denominators.age[df.age$year[i], df.age$Locationabbr[i], age] + 
            df.age$Sample_Size[i]
    }
    missing.mask = rv$denominators.age == 0
    rv$denominators.age[missing.mask] = rv$numerators.age[missing.mask] = NA
    
    for (i in 1:dim(df.sex)[1])
    {
        sex = df.sex$Break_Out[i]
        
        rv$numerators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] = 
            rv$numerators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] + 
            df.sex$Sample_Size[i] * df.sex$frac[i]
        
        rv$denominators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] = 
            rv$denominators.sex[df.sex$year[i], df.sex$Locationabbr[i], sex] + 
            df.sex$Sample_Size[i]
    }
    missing.mask = rv$denominators.sex == 0
    rv$denominators.sex[missing.mask] = rv$numerators.sex[missing.mask] = NA
    
    
    rv
}

read.nhbs.testing <- function(dir='cleaned_data/continuum/national/testing/',
                              verbose=T)
{
    files = list.files(dir)
    year = as.numeric(substr(files, 1, 4))
    risk = rep('heterosexual', length(files))
    risk[grepl('msm', files, ignore.case = T)] = 'msm'
    risk[grepl('idu', files, ignore.case = T)] = 'idu'
    
    all.dfs = lapply(1:length(files), function(i){
        if (verbose)
            print(paste0("Reading NHBS file on testing for ", year[i]))
        read.nhbs.testing.file(file.path(dir, files[i]), year=year[i], risk=risk[i])
    })
    
    if (verbose)
        print("Joining NHBS testing files")
    
    rv = all.dfs[[1]]
    if (length(all.dfs)>1)
    {
        for (i in 2:length(all.dfs))
        {
            for (name in names(rv))
            {
                #                print(i)
                #                print(name)
                rv[[name]] = rbind(rv[[name]], all.dfs[[i]][[name]])
            }
        }
    }
    
    rv$all = rbind(rv$race, rv$age, rv$sex)
    
    rv
}

read.nhbs.testing.file <- function(file,
                                   year,
                                   risk)
{
    df = read.csv(file, stringsAsFactors = F)
    
    for (i in 2:dim(df)[2])
    {
        df[,i] = gsub(',', '', df[,i])
        df[!is.na(df[,i]) & df[,i] == '', i] = NA
        df[,i] = as.numeric(df[,i])
    }
    
    N = dim(df)[1]
    header.rows = (1:N)[is.na(df$n.ever) & is.na(df$n.12mo)]
    total.row = (1:N)[grepl('Total', df[,1], ignore.case = T)][1]
    
    #-- Age --#
    age.index = (1:N)[grepl('age', df[,1], ignore.case = T)][1]
    last.age.index = header.rows[header.rows>age.index][1]-1
    
    if ((last.age.index-age.index)==6)
    {
        age1.indices = age.index + 1:2
        age1.mult = c(1,1)
        age2.indices = age.index + 3:4
        age2.mult = c(1,.5)
        age3.indices = age.index + 4:5
        age3.mult = c(.5, .5)
        age4.indices = age.index + 5:6
        age4.mult = c(.5, .25)
        age5.indices = age.index + 6
        age5.mult = .75
    }
    else
    {
        age1.indices = age.index + 1
        age1.mult = 1
        age2.indices = age.index + 2:3
        age2.mult = c(1,.5)
        age3.indices = age.index + 3:4
        age3.mult = c(.5, .5)
        age4.indices = age.index + 4:5
        age4.mult = c(.5, .25)
        age5.indices = age.index + 5
        age5.mult = .75
    }
    
    denominators = c(sum(df$n.total[age1.indices] * age1.mult),
                     sum(df$n.total[age2.indices] * age2.mult),
                     sum(df$n.total[age3.indices] * age3.mult),
                     sum(df$n.total[age4.indices] * age4.mult),
                     sum(df$n.total[age5.indices] * age5.mult))
    numerators.12 = c(sum(df$n.12mo[age1.indices] * age1.mult),
                      sum(df$n.12mo[age2.indices] * age2.mult),
                      sum(df$n.12mo[age3.indices] * age3.mult),
                      sum(df$n.12mo[age4.indices] * age4.mult),
                      sum(df$n.12mo[age5.indices] * age5.mult))
    numerators.ever = c(sum(df$n.ever[age1.indices] * age1.mult),
                        sum(df$n.ever[age2.indices] * age2.mult),
                        sum(df$n.ever[age3.indices] * age3.mult),
                        sum(df$n.ever[age4.indices] * age4.mult),
                        sum(df$n.ever[age5.indices] * age5.mult))
    
    df.age = data.frame(n=denominators,
                        frac.12mo=numerators.12/denominators,
                        frac.ever=numerators.ever/denominators,
                        year=year,
                        age=paste0('age',1:5),
                        race='all',
                        sex='all',
                        risk=risk,
                        msa='all',
                        stringsAsFactors = F)
    
    
    
    #-- Race--#
    first.race.index = (1:N)[grepl('race', df[,1], ignore.case = T)][1]+1
    last.race.index = header.rows[header.rows>first.race.index][1]-1
    
    black.index = (first.race.index:last.race.index)[grepl('black', df[first.race.index:last.race.index,1], ignore.case = T)]
    hispanic.index = (first.race.index:last.race.index)[grepl('hispanic', df[first.race.index:last.race.index,1], ignore.case = T)]
    other.indices = setdiff(first.race.index:last.race.index, c(black.index, hispanic.index))
    
    denominators = c(df$n.total[black.index], df$n.total[hispanic.index], sum(df$n.total[other.indices]))
    numerators.12 = c(df$n.12mo[black.index], df$n.12mo[hispanic.index], sum(df$n.12mo[other.indices]))
    numerators.ever = c(df$n.ever[black.index], df$n.ever[hispanic.index], sum(df$n.ever[other.indices]))
    
    
    df.race = data.frame(n=denominators,
                         frac.12mo=numerators.12/denominators,
                         frac.ever=numerators.ever/denominators,
                         year=year,
                         age='all',
                         race=c('black','hispanic','other'),
                         sex='all',
                         risk=risk,
                         msa='all',
                         stringsAsFactors = F)
    
    #Sex
    if (risk=='msm')
        df.sex = data.frame(n=df$n.total[total.row],
                            frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                            frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                            year=year,
                            age='all',
                            race='all',
                            sex='male',
                            risk=risk,
                            msa='all',
                            stringsAsFactors = F)
    else if (any(names(df)=='n.ever.male'))
        df.sex = data.frame(n=c(df$n.total.female[total.row], df$n.total.male[total.row]),
                            frac.12mo = c(df$n.12mo.female[total.row], df$n.12mo.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                            frac.ever = c(df$n.ever.female[total.row], df$n.ever.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                            year=year,
                            age='all',
                            race='all',
                            sex=c('female','male'),
                            risk=risk,
                            msa='all',
                            stringsAsFactors = F)
    else
    {
        female.mask = grepl('female', df[,1], ignore.case = T)
        male.mask = grepl('male', df[,1], ignore.case = T) & !female.mask
        
        df.sex = data.frame(n=c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.12mo=c(df$n.12mo[female.mask], df$n.12mo[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.ever=c(df$n.ever[female.mask], df$n.ever[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            year=year,
                            age='all',
                            race='all',
                            sex=c('female','male'),
                            risk=risk,
                            msa='all',
                            stringsAsFactors = F)
    }
    
    #Risk
    df.risk = data.frame(n=df$n.total[total.row],
                         frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                         frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                         year=year,
                         age='all',
                         race='all',
                         sex='all',
                         risk=risk,
                         msa='all',
                         stringsAsFactors = F)
    
    #MSA
    first.msa.index = (1:N)[grepl('Metropolitan', df[,1], ignore.case = T) | grepl("City", df[,1], ignore.case = T)][1]+1
    last.msa.index = total.row-1
    msas = cbsa.for.msa.name(gsub('â€“', '-', df[first.msa.index:last.msa.index,1]))
    msa.indices = (first.msa.index:last.msa.index)[!is.na(msas)]
    msas = msas[!is.na(msas)]
    
    df.msa = data.frame(n=df$n.total[msa.indices],
                        frac.12mo=df$n.12mo[msa.indices]/df$n.total[msa.indices],
                        frac.ever=df$n.ever[msa.indices]/df$n.total[msa.indices],
                        year=year,
                        age='all',
                        race='all',
                        sex='all',
                        risk=risk,
                        msa=msas,
                        stringsAsFactors = F
    )
    
    list(race=df.race,
         age=df.age,
         sex=df.sex,
         risk=df.risk,
         msa=df.msa)
}


logit <- function(x){log(x) - log(1-x)}
expit <- function(x){1/(1+exp(-x))}