
##---------------------------------##
##-- THE MODEL FOR PREP COVERAGE --##
##---------------------------------##

get.prep.model <- function(prep.manager,
                           location,
                           specification.metadata,
                           anchor.year=2014,
                           max.proportion=0.333,
                           apply.age.ors.to.max=F,
                           apply.race.ors.to.max=F,
                           apply.sex.ors.to.max=F,
                           logistic.after.frac.of.p.span=0.5,
                           as.proportion.of.indicated=T)
{
    base.model = make.prep.model.logistic.tail(max.proportion = max.proportion,
                                               use.logistic.tail=use.logistic.tail,
                                               logistic.after.frac.of.p.span=logistic.after.frac.of.p.span,
                                               as.proportion.of.indicated=as.proportion.of.indicated,
                                               specification.metadata=specification.metadata)
}


make.logistic.tail.model <- function(intercept,
                                     slope,
                                     anchor.year,
                                     additional.slope,
                                     additional.slope.after.year,
                                     max.p=1,
                                     logistic.after.frac.of.max.p=0.5)
{
    logistic.after.p = logistic.after.frac.of.max.p * max.p
    model = list(intercept=intercept,
                 slope=slope,
                 anchor.year=anchor.year,
                 additional.slope=additional.slope,
                 additional.after.year=additional.slope.after.year,
                 max.p=max.p,
                 logistic.after.p=logistic.after.p)
    
    model$logistic.slope.sans.additional = slope / logistic.after.frac.of.max.p / (1-logistic.after.frac.of.max.p) / max.p
    model$logistic.slope.with.additional = (slope + additional.slope) / logistic.after.frac.of.max.p / (1-logistic.after.frac.of.max.p) / max.p
    
    p.at.additional.year = intercept + slope * (additional.slope.after.year - anchor.year)
    
    logistic.after.year = anchor.year + (logistic.after.p - intercept) / slope
    mask = p.at.additional.year < logistic.after.p
    logistic.after.year.after.additional = additional.slope.after.year + (logistic.after.p - p.at.additional.year) / (slope + additional.slope)
    logistic.after.year[mask] = logistic.after.year.after.additional[mask]
    
    slope.at.logistic.after = slope
    slope.at.logistic.after[mask] = (slope + additional.slope)[mask]
    
    model$logistic.intercept = log(logistic.after.frac.of.max.p) - log(1-logistic.after.frac.of.max.p) - 
        model$logistic.slope.sans.additional * (pmin(logistic.after.year, additional.slope.after.year) - anchor.year)-
        model$logistic.slope.with.additional * pmax(0, logistic.after.year - additional.slope.after.year)
    
    model
}

# Helper
calculate.logistic.tail.values <- function(model,
                                    year)
{
    # The value based off linear model
    rv = model$intercept + 
        model$slope * (year-model$anchor.year) +
        model$additional.slope * max(0, year-model$additional.after.year)
    
    mask = rv > model$logistic.after.p
    
    # The value based off logistic model
    log.ors = model$logistic.intercept[mask] +
        model$logistic.slope.sans.additional[mask] * (pmin(year, model$additional.after.year)-model$anchor.year) +
        model$logistic.slope.with.additional[mask] * pmax(0, year-model$additional.after.year)
    rv[mask] = model$max.p / (1 + exp(-log.ors))
    
    rv[rv<0] = 0
    rv
}

##----------------------##
##-- PREP PERSISTENCE --##
##----------------------##

# In general, the mapping from Rx (in a year) to prep coverage is
# Rx_1y * fraction_rx_>_1mo * ratio_rx_3mo_rx_1y * persistence_at_3mo / fraction_Rx_recorded

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6286244/
FRACTION.PREP.STARTS.RECORDED = 0.83
FRACTION.PREP.STARTS.RECORDED.SD = 0.05

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7147726/
FRACTION.PREP.GT.1MO = 75839/95683
FRACTION.PREP.GT.1MO.SD = 0.05

# MacCannell T, Verma S, Shvachko V, Rawlings K, Mera R. Validation of a Truvada for PrEP Algorithm using an Electronic Medical Record. 8th IAS Conference on HIV Pathogenesis, Treatment & Prevention. Vancouver Canada July 2015.
# http://205.186.56.104/largeDatabase/sample_details.php?id=75332
#FRACTION.PREP.MISCLASSIFIED = .04 + .099
#FRACTION.PREP.MISCLASSIFIED.SD = ((.125-.079) + (.058-.028)) / 2 / 1.96
FRACTION.PREP.MISCLASSIFIED = .099 # just the misclassifications of PEP - we'll assume as we scale up there's not a lot of HBV
FRACTION.PREP.MISCLASSIFIED.SD = (.125-.079) / 2 / 1.96
#FRACTION.PREP.CORRECTLY.CLASSIFIED = 0.724 #The ppv
#FRACTION.PREP.CORRECTLY.CLASSIFIED.SD = (75.8 - 68.9)/100 / 2 / 1.96# * 3 #inflate by 3 to allow for location specific differences
FRACTION.PREP.CORRECTLY.CLASSIFIED = 1-FRACTION.PREP.MISCLASSIFIED
FRACTION.PREP.CORRECTLY.CLASSIFIED.SD = FRACTION.PREP.MISCLASSIFIED.SD * 3 #inflate by 3 to allow for location specific differences

# the 3mo numbers from:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6286209/
# the year numbers are the sum of the AIDSVu Data in 2017
PREP.RX.3MO.To.1Y = c(
    total=70395/134584,
    female=3229/6986,
    male=67166/110678
)
PREP.RX.3MO.To.1Y.SD = c(total=0.03823962) #sd(prep.3mo.to.1y.ratios.by.region())

INDIV.PREP.RETENTION = c(
    (88/116),#^.5, #6mo - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6535209/
    (1-65/197),#^.5, #6mo - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6204096/
    #   1- .69/2, #The median time to disc among 69% who did was 159d ~ 5.3mo - 
    # https://pubmed.ncbi.nlm.nih.gov/31499518/
    0.42,#^.5, #6mo - https://pubmed.ncbi.nlm.nih.gov/30341556/
    0.57,#^.5, #6mo - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4908080/
    0.48 #^.8 #60-165d - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6594905/
            #0.8 = 90/(mean(c(60,165)))
)^c(.5, .5, .5, .5, 0.8)
INDIV.PREP.RET.N = c(
    116,
    197,
#    440,
    107,
    216,
    696
)
PREP.RETENTION.3MO = sum(INDIV.PREP.RETENTION*INDIV.PREP.RET.N)/sum(INDIV.PREP.RET.N)
PREP.RETENTION.3MO.SD = sqrt(sum(INDIV.PREP.RET.N*(INDIV.PREP.RETENTION-PREP.RETENTION.3MO)^2) / sum(INDIV.PREP.RET.N))

# Ratio v1 to v2 - from the first calibration sent to annals vs updated (more) PrEP
PREP.RATIO.V2.TO.V1 = 0.5677996 / 0.301984

# No longer using this
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6378757/
PREP.PERSISTENCE = c(total=0.56,
                     male=0.57,
                     female=0.34,
                     age1=0.43,
                     age2=0.53,
                     age3=0.56,
                     age4=0.64,
                     age5=0.65)
PREP.PERSISTENCE.SD = sqrt(PREP.PERSISTENCE * (1-PREP.PERSISTENCE) /
                               c(total=7148,
                                 male=6900,
                                 female=244,
                                 age1=784,
                                 age2=1552,
                                 age3=2521,
                                 age4=1432,
                                 age5=855)
) * 5



convert.true.prep.to.rx <- function(x)
{
    x / FRACTION.PREP.CORRECTLY.CLASSIFIED / PREP.RX.3MO.To.1Y['total'] * FRACTION.PREP.STARTS.RECORDED
    #x / FRACTION.PREP.CORRECTLY.CLASSIFIED / PREP.RX.3MO.To.1Y['total'] / PREP.RETENTION.3MO * FRACTION.PREP.STARTS.RECORDED
    #x / FRACTION.PREP.GT.1MO / PREP.RX.3MO.To.1Y['total'] / PREP.RETENTION.3MO * FRACTION.PREP.STARTS.RECORDED
    # x * FRACTION.PREP.STARTS.RECORDED / PREP.RX.3MO.To.1Y['total'] / PREP.PERSISTENCE['total']
    
 #   x / 0.724 / PREP.RX.3MO.To.1Y['total'] / PREP.RETENTION.3MO * FRACTION.PREP.STARTS.RECORDED
    
}

# A helper to get an estimate of the sds
prep.3mo.to.1y.ratios.by.region <- function(prep.manager = ALL.DATA.MANAGERS$prep)
{
    states = state.for.county(prep.manager$locations)
    divisions = state.to.region.division(states)
    
    prep.3mo = c(
        'East North Central' = 8858,
        'West North Central' = 3105,
        'Mid-Atlantic' = 16628,
        'New England' = 4523,
        'East South Central' = 2253,
        'West South Central' = 5880,
        'South Atlantic' = 13297,
        'Mountain' = 3546,
        'Pacific' = 12545
    )
    
    prep.12mo = sapply(names(prep.3mo), function(div){
        mask = !is.na(divisions) & divisions==div
        fips = prep.manager$locations[mask]
        
        get.prep.data(prep.manager, locations = fips, years = 2017, collapse.locations = T)[1]
    })
    
    prep.3mo/prep.12mo
}

convert.prep.rx.to.true.prep <- function(x,
                                         description=NULL)
{   
    return (x * FRACTION.PREP.CORRECTLY.CLASSIFIED * PREP.RX.3MO.To.1Y['total'] / FRACTION.PREP.STARTS.RECORDED )
    
#    return (x * FRACTION.PREP.CORRECTLY.CLASSIFIED * PREP.RX.3MO.To.1Y['total'] * PREP.RETENTION.3MO / FRACTION.PREP.STARTS.RECORDED )
    
    #Older
    if (is.null(description))
    {
        df = reshape2::melt(x)
        tags = df[,names(df)!='year' & names(df)!='value']
        if (length(tags)==0)
            description = rep('total', dim(df)[1])
        else if (is.null(dim(tags)))
            description = tags
        else
            description = apply(tags, 1, paste0, collapse="_")
        description = as.character(description)
    }
    
    multiplier = rep(PREP.PERSISTENCE['total'], length(description))
    
    multiplier[grepl('male', description, ignore.case = T)] = PREP.PERSISTENCE['male']
    multiplier[grepl('female', description, ignore.case = T)] = PREP.PERSISTENCE['female']
    
    multiplier[grepl('24', description, ignore.case = T)] = PREP.PERSISTENCE['age1']
    multiplier[grepl('34', description, ignore.case = T)] = PREP.PERSISTENCE['age2']
    multiplier[grepl('44', description, ignore.case = T)] = PREP.PERSISTENCE['age3']
    multiplier[grepl('54', description, ignore.case = T)] = PREP.PERSISTENCE['age4']
    multiplier[grepl('55', description, ignore.case = T)] = PREP.PERSISTENCE['age5']
    
    x * multiplier / FRACTION.PREP.STARTS.RECORDED
}


##--------------------------##
##-- INDICATIONS FOR PREP --##
##--------------------------##

#based on NHBS data
# NHBS survey eligibility: 
#   https://www.cdc.gov/hiv/pdf/statistics/systems/nhbs/nhbs_round4modelsurveillanceprotocol.pdf
#   p 42, 43, 45
get.prep.indications.estimate <- function(prep.manager=ALL.DATA.MANAGERS$prep,
                                          location,
                                          specification.metadata)
{
    #-- Get probability of PrEP Indication (conditioned on sexual activity for MSM and HET) --#
    data = get.prep.lit.data(as.proportion.of.indicated = T,
                             specification.metadata=specification.metadata)
    msm = make.marginal.rr.array(p=data$nhbs$msm.2017$p.indicated,
                                 n=data$nhbs$msm.2017$n)
    idu = make.marginal.rr.array(p=data$nhbs$idu.2018$p.indicated,
                                 n=data$nhbs$idu.2018$n.sex.age)
    het = make.marginal.rr.array(p=data$nhbs$het.2016$p.indicated,
                                 n=data$nhbs$het.2016$n.sex.age)
    
    #-- Multiply MSM and HET by prob of sexual activity --#
    
    # female age1-4 from NSFG 2002, table 2
    #   https://www.cdc.gov/nchs/data/ad/ad362.pdf
    #   
    # male age1-4 from NSFG 2002,
    #   https://www.cdc.gov/nchs/data/ad/ad362.pdf  
    #   
    # age5 from Lindau 2007 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426743/
    
    p.active.male.by.age = c(
        age1=((1-.403)*10208 + (1-.09)*9883)/(10208+9883)*10/12,
        age2=1-.047,
        age3=((1-.027)*10138+(1-.019)*10557)/(10138+10557),
        age4=1-.018,
        age5=(.837*528 + .670*547 + .385*380)/1455
        )
    p.active.female.by.age = c(
        age1=((1-.367)*9834+(1-.087)*9840)/(9384+9840)*10/12,
         #reduce by 10/12 assuming no activity from 13-14yo
        age2=1-.025,
        age3=((1-.018)*10272+(1-.01)*10853)/(10272+10853),
        age4=1-.013,
        age5=(.616*492 + .395*545 + .167*513)/1550
        )
    
    msm = msm * p.active.male.by.age
    het[,,'heterosexual_male'] = het[,,'heterosexual_male'] * p.active.male.by.age
    het[,,'female'] = het[,,'female'] * p.active.female.by.age
    
    #-- Put it together into a big array --#
    
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    rv[,,'msm','never_IDU'] = rv[,,'msm','IDU_in_remission'] = msm
    rv[,,dimnames(idu)[['sex']],'active_IDU'] = idu
    rv[,,dimnames(het)[['sex']],'never_IDU'] = rv[,,dimnames(het)[['sex']],'IDU_in_remission'] = het
    rv[,,'msm','active_IDU'] = 1 - (1-msm) * (1-as.numeric(idu[,,'heterosexual_male']))
    
    #-- Return --#
    
    rv
}

##------------------------------------------##
##-- PRESCRIPTIONS OF TRUVADA FROM AIDSVu --##
##------------------------------------------##

get.prep.data <- function(prep.manager, locations, years=NULL, sex=F, age=F, na.rm=T, collapse.locations=T)
{
    # Check years
    orig.years = years
    if (is.null(years))
        years = as.character(prep.manager$years)
    else
        years = intersect(as.character(years), as.character(prep.manager$years))
    
    if (length(years)==0)
        stop(paste0("None of the requested years (",
                    paste0(orig.years, collapse=', '),
                    ") are present in the PrEP manager"))
    
    # Check location
    locations = as.character(locations)
    if (any(sapply(locations, function(loc){all(loc!=prep.manager$locations)})))
    {
        orig.locations = locations
        locations = counties.for.msa(locations)
        
        if (any(sapply(locations, function(loc){all(loc!=prep.manager$locations)})))
            stop(paste0("No PrEP data for locations ", 
                        paste0("'", locations, "'", collapse=', ')))
    }
    
    # Pull data
    if (sex && age)
        stop("Either 'sex' or 'age' can be true, but not both")
    else if (sex)
    {
        rv = prep.manager$prep.sex[years, locations,]
    }
    else if (age)
    {
        rv = prep.manager$prep.age[years, locations,]
    }
    else
    {
        rv = prep.manager$prep.all[years, locations]
    }
    
    dim.names = list(year=years,
                     location=locations,
                     sex = dimnames(prep.manager$prep.sex)[['sex']],
                     age = dimnames(prep.manager$prep.age)[['age']]
    )
    dim.names = dim.names[c(T,T,sex,age)]
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    if (collapse.locations)
    {
        all.na = apply(is.na(rv), setdiff(names(dim.names), 'location'), all)
        rv = apply(rv, setdiff(names(dim.names), 'location'), sum, na.rm=na.rm)
        rv[all.na] = NA
        
        dim.names = dim.names[setdiff(names(dim.names), 'location')]
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
    }
    
    rv
}

##------------------##
##-- CONSTRUCTORS --##
##------------------##

read.prep.manager <- function(dir='cleaned_data/prep/',
                              specification)
{
    files = list.files(file.path(dir, 'county'))
    full.files = list.files(file.path(dir, 'county'), full.names = T)
    file.years = substr(files, 1, 4)
    
    dfs = lapply(full.files, read.prep.file)
    
    all.counties = sort(unique(as.character(sapply(dfs, function(df){df$location}))))
    all.years = as.character(file.years)
    
    all.dim.names = list(year=all.years, 
                         location=all.counties, 
                         sex=c('female','male'),
                         age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'))
    
    rv = list(
        years = file.years,
        locations = all.counties,
        prep.all = array(as.numeric(NA), 
                         dim=sapply(all.dim.names[c('year','location')],  length), 
                         dimnames=all.dim.names[c('year','location')]),
        prep.sex = array(as.numeric(NA), 
                         dim=sapply(all.dim.names[c('year','location','sex')],  length), 
                         dimnames=all.dim.names[c('year','location','sex')]),
        prep.age = array(as.numeric(NA), 
                         dim=sapply(all.dim.names[c('year','location','age')],  length), 
                         dimnames=all.dim.names[c('year','location','age')])
    )
    
    
    for (i in 1:length(dfs))
    {
        year = all.years[i]
        df = dfs[[i]]
        locations = intersect(df$location, all.counties)
        df = df[sapply(df$location, function(loc){any(loc==locations)}),]
        
        
        rv$prep.all[year, locations] = df$total
        
        rv$prep.sex[year, locations, 'female'] = df$female
        rv$prep.sex[year, locations, 'male'] = df$male
        
        rv$prep.age[year, locations, 1] = df$age1
        rv$prep.age[year, locations, 2] = df$age2
        rv$prep.age[year, locations, 3] = df$age3
        rv$prep.age[year, locations, 4] = df$age4
        rv$prep.age[year, locations, 5] = df$age5
    }
    
    rv
}



read.prep.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    df[!is.na(df) & df==''] = NA
    df = df[!apply(is.na(df), 1, all),]
    
    rate.to.numeric = function(r){rv=as.numeric(r); rv[rv==-1]=NA; rv}
    
    rv = data.frame(location=df$GEO.ID,
                    total=rate.to.numeric(df$County.PrEP.Users),
                    male=rate.to.numeric(df$Male.PrEP.Users),
                    female=rate.to.numeric(df$Female.PrEP.Users),
                    age1=rate.to.numeric(df$Age.LE.24.PrEP.Users),
                    age2=rate.to.numeric(df$Age.25.34.PrEP.Users),
                    age3=rate.to.numeric(df$Age.35.44.PrEP.Users),
                    age4=rate.to.numeric(df$Age.45.54.PrEP.Users),
                    age5=rate.to.numeric(df$Age.55..PrEP.Users))
}

##------------------------------------##
##-- ACTUALLY MAKING THE PREP MODEL --##
##------------------------------------##

make.prep.model <- function(anchor.year,
                            specification.metadata,
                            max.proportion,
                            apply.age.ors.to.max=T,
                            apply.race.ors.to.max=T,
                            apply.sex.ors.to.max=T)
{
    # proportion.msm = .159/2 # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4516312/
    
    logit = function(p){log(p)-log(1-p)}
    expit = function(lo){1/(1+exp(-lo))}
    map.p = function(p){
        pmin(.9999, p/max.proportion)
    }
    
    #-- RAW DATA --#
    
    
    #   p.idu.2018 = 0.011 #https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
    #   p.msm.2017 = 0.25 #https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
    #    nhbs.p.het.2016 = c(total=0.002) #https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf
    nhbs.p.idu.2015 = c(total=0.003)
    
    prep.persistence.13mo = 1-.38 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5879003/
    
    nhbs.p.idu.2018 = c(total=0.011,
                        male=0.009,
                        female=0.015,
                        age1=0.017,
                        age2=0.011,
                        age3=0.013,
                        age4=0.014,
                        age5=0.008,
                        black=0.007,
                        hispanic=0.014,
                        white=0.012)
    nhbs.n.idu.2018 = c(total=5605,
                        male=3793,
                        female=1771,
                        age1=229,
                        age2=707,
                        age3=1730,
                        age4=1329,
                        age5=1610,
                        black=1173,
                        hispanic=1311,
                        white=2646)
    
    nhbs.p.msm.2017 = c(total=0.25,
                        age1=0.215,
                        age2=0.280,
                        age3=0.304,
                        age4=0.237,
                        age5=0.139,
                        black=0.188,
                        hispanic=0.212,
                        white=0.309)
    nhbs.n.msm.2017 = c(total=4952,
                        age1=924,
                        age2=1307,
                        age3=1430,
                        age4=669,
                        age5=622,
                        black=1183,
                        hispanic=1426,
                        white=1878)
    
    nhbs.p.het.2016 = c(total=0.002,
                        male=0.001,
                        female=0.002,
                        age1=0.001,
                        age2=0.002,
                        age3=0.001,
                        age4=0.001,
                        age5=0.002)
    nhbs.n.het.2016 = c(total=7316,
                        male=3280,
                        female=4036,
                        age1=1518,
                        age2=1067,
                        age3=1546,
                        age4=1455,
                        age5=1730)
    
    ca.msm.2017 = c(total=0.097,
                    black=19/193,
                    hispanic=16/243,
                    white=23/165
    )*.716 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5140696/
    
    cdc.total = c('2014'=13748,
                  '2015'=38879,
                  '2016'=78630)
    
    chicago.p.msm = c('2015'=.066, '2017'=.175)
    
    
    
    #-- CRUNCH IT TO SLOPES AND ORS --#
    log.or.vector = numeric()
    
    #-- MSM slope and intercept --#
    logit.slope.chicago = lm(logit(map.p(chicago.p.msm))~as.numeric(names(chicago.p.msm)))$coefficients[2]
    log.slope.cdc = lm(log(cdc.total)~as.numeric(names(cdc.total)))$coefficients[2]
    logit.slope.msm = mean(c(logit.slope.chicago, log.slope.cdc))
    
    # msm.intercept.lo = mean(c(logit(map.p(ca.msm.2017['total'])) + logit.slope.msm*(anchor.year-2017),
    #                              logit(map.p(chicago.p.msm['2015'])) + logit.slope.msm*(anchor.year-2015)#,
    #                             #logit(map.p(nhbs.p.msm.2017['total'])) + logit.slope.msm*(anchor.year-2017)
    #  ))
    msm.intercept.lo = logit(map.p(ca.msm.2017['total'])) + logit.slope.msm*(anchor.year-2017)
    
    #   nhbs.rr = expit(msm.intercept.lo) / expit(logit(map.p(nhbs.p.msm.2017['total'])) + logit.slope.msm*(anchor.year-2017))
    #   chicago.rr = expit(msm.intercept.lo) / expit(logit(map.p(chicago.p.msm['2015'])) + logit.slope.msm*(anchor.year-2015))
    nhbs.rr = expit(msm.intercept.lo + logit.slope.msm*(2017-anchor.year))*max.proportion / nhbs.p.msm.2017['total']
    
    log.or.vector['msm'] = msm.intercept.lo
    msm.slope = log.or.vector['msm_slope'] = logit.slope.msm
    
    #-- IDU slope and intercept --#
    
    IDU.HET.MULT = 1#0.25
    
    logit.slope.idu = (logit(map.p(nhbs.rr*nhbs.p.idu.2018['total'])) - logit(map.p(nhbs.rr*nhbs.p.idu.2015['total']))) / (2018-2015)
    
    idu.intercept.lo = logit(IDU.HET.MULT*map.p(nhbs.rr*nhbs.p.idu.2015['total'])) + logit.slope.idu*(anchor.year-2015)
    
    log.or.vector['idu'] = idu.intercept.lo
    idu.slope = log.or.vector['idu_slope'] = logit.slope.idu
    
    
    #-- Heterosexual slope and intercept --#
    
    logit.slope.het = logit.slope.idu
    
    het.intercept.lo = logit(IDU.HET.MULT*map.p(nhbs.rr*nhbs.p.het.2016['total'])) + logit.slope.het*(anchor.year-2016)
    
    log.or.vector['heterosexual'] = het.intercept.lo
    het.slope = log.or.vector['heterosexual_slope'] = logit.slope.het
    
    
    #-- Race ORs --#
    #combine 2017 MSM and 2018 IDU
    
    races = c('black','hispanic','white')
    if (apply.race.ors.to.max)
    {
        nhbs.msm.lors = logit(nhbs.p.msm.2017[races]) - logit(nhbs.p.msm.2017['total'])
        nhbs.idu.lors = logit(nhbs.p.idu.2018[races]) - logit(nhbs.p.idu.2018['total'])
        ca.msm.lors = logit(ca.msm.2017[races]) - logit(ca.msm.2017['total'])
    }
    else
    {
        nhbs.msm.lors = logit(map.p(nhbs.p.msm.2017[races])) - logit(map.p(nhbs.p.msm.2017['total']))
        nhbs.idu.lors = logit(map.p(nhbs.p.idu.2018[races])) - logit(map.p(nhbs.p.idu.2018['total']))
        ca.msm.lors = logit(map.p(ca.msm.2017[races])) - logit(map.p(ca.msm.2017['total']))
    }
    
    black.lor = log.or.vector['black'] = 
        mean(c(nhbs.msm.lors[1], nhbs.idu.lors[1], ca.msm.lors[1]))
    hispanic.lor = log.or.vector['hispanic'] = 
        mean(c(nhbs.msm.lors[2], nhbs.idu.lors[2], ca.msm.lors[2]))
    other.lor = log.or.vector['other'] = 
        mean(c(nhbs.msm.lors[3], nhbs.idu.lors[3], ca.msm.lors[3]))
    
    
    #-- Age ORs --#
    
    
    ages = paste0('age', 1:5)
    p.total = (nhbs.p.msm.2017['total'] * nhbs.n.msm.2017['total'] +
                   nhbs.p.idu.2018['total'] * nhbs.n.idu.2018['total']) /
        (nhbs.n.msm.2017['total'] + nhbs.n.idu.2018['total'])
    p.age = (nhbs.p.msm.2017[ages] * nhbs.n.msm.2017[ages] +
                 nhbs.p.idu.2018[ages] * nhbs.n.idu.2018[ages]) /
        (nhbs.n.msm.2017[ages] + nhbs.n.idu.2018[ages])
    
    if (apply.age.ors.to.max)
        age.lors = log.or.vector[ages] = logit(p.age) - logit(p.total)
    else
        age.lors = log.or.vector[ages] = logit(map.p(p.age)) - logit(map.p(p.total))
    
    #-- Male vs Female --#
    
    p.total = (nhbs.p.idu.2018['total'] * nhbs.n.idu.2018['total'] +
                   nhbs.p.het.2016['total'] * nhbs.n.het.2016['total']) /
        (nhbs.n.idu.2018['total'] + nhbs.n.het.2016['total'])
    p.male = (nhbs.p.idu.2018['male'] * nhbs.n.idu.2018['male'] +
                  nhbs.p.het.2016['male'] * nhbs.n.het.2016['male']) /
        (nhbs.n.idu.2018['male'] + nhbs.n.het.2016['male'])
    p.female = (nhbs.p.idu.2018['female'] * nhbs.n.idu.2018['female'] +
                    nhbs.p.het.2016['female'] * nhbs.n.het.2016['female']) /
        (nhbs.n.idu.2018['female'] + nhbs.n.het.2016['female'])
    
    if (apply.sex.ors.to.max)
    {
        male.lor = log.or.vector['male'] = logit(p.male) - logit(p.total)
        female.lor = log.or.vector['female'] = logit(p.female) - logit(p.total)
    }
    else
    {
        male.lor = log.or.vector['male'] = logit(map.p(p.male)) - logit(map.p(p.total))
        female.lor = log.or.vector['female'] = logit(map.p(p.female)) - logit(map.p(p.total))
    }
    
    #-- Save the ORs --#
    model = list(anchor.year=anchor.year,
                 max.proportion=max.proportion,
                 log.ors=log.or.vector)
    
    
    #-- Put It Together --#
    
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    model$intercept = model$slope = 
        model$max.p.lors = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Slopes - Risk
    model$slope[,,c('heterosexual_male','female'),'never_IDU'] = 
        model$slope[,,c('heterosexual_male','female'),'never_IDU'] + het.slope
    model$slope[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] =
        model$slope[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] + idu.slope
    model$slope[,,'msm',] =
        model$slope[,,'msm',] + msm.slope
    
    # Intercepts - Risk
    model$intercept[,,c('heterosexual_male','female'),'never_IDU'] = 
        model$intercept[,,c('heterosexual_male','female'),'never_IDU'] + het.intercept.lo
    model$intercept[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] =
        model$intercept[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] + idu.intercept.lo
    model$intercept[,,'msm',] =
        model$intercept[,,'msm',] + msm.intercept.lo
    
    # Race
    if (apply.race.ors.to.max)
    {
        model$max.p.lors[,'black',,] = 
            model$max.p.lors[,'black',,] + black.lor - other.lor
        model$max.p.lors[,'hispanic',,] = 
            model$max.p.lors[,'hispanic',,] + hispanic.lor - other.lor
    }
    else
    {
        model$intercept[,'black',,] = 
            model$intercept[,'black',,] + black.lor
        model$intercept[,'hispanic',,] = 
            model$intercept[,'hispanic',,] + hispanic.lor
        model$intercept[,'other',,] = 
            model$intercept[,'other',,] + other.lor
    }
    
    # Age
    for (age in 1:length(age.lors))
    {
        if (apply.age.ors.to.max)
            model$max.p.lors[age,,,] = 
                model$max.p.lors[age,,,] + age.lors[age] - age.lors[3]
        else
            model$intercept[age,,,] = 
                model$intercept[age,,,] + age.lors[age]
    }
    
    # Male/Female
    if (apply.sex.ors.to.max)
    {   
        model$max.p.lors[,,'heterosexual_male',] = 
            model$max.p.lors[,,'heterosexual_male',] + male.lor
        model$max.p.lors[,,'female',] = 
            model$max.p.lors[,,'female',] + female.lor
    }
    else
    {
        model$intercept[,,'female',] = 
            model$intercept[,,'female',] + female.lor - male.lor
    }
    
    # Return
    model$mixed.linear=F
    model$model.type = 'logistic'
    model
}


make.prep.model.logistic.tail <- function(specification.metadata,
                                         as.proportion.of.indicated=T,
                                         min.proportion=0,
                                         max.proportion=0.4,
                                         use.logistic.tail=T,
                                         logistic.after.frac.of.p.span=0.5,
                                         msm.correction.rr = 0.1298588, #from calling calculate.msm.prep.correction.rr()
                                         idu.correction.rr = 0.09869363, #from calling calculate.female.prep.correction.rr()
                                         het.correction.rr = 0.09869363, #from calling calculate.female.prep.correction.rr()
                                         age1.correction.rr = 0.612,#0.538,
                                         age2.correction.rr = 1.138,#1,
                                         age3.correction.rr = 1.312,#1.153,
                                         age4.correction.rr = 1.240,#1.090,
                                         age5.correction.rr = 0.697#0.613
                                         #x=calculate.age.prep.correction.rr(relative.to=NULL);x/mean(x)
)
{
    anchor.year=2014
    data = get.prep.lit.data(as.proportion.of.indicated=as.proportion.of.indicated, specification.metadata=specification.metadata)
  
    log.or.vector = numeric()
    
    #-- MSM --#
    
    # Slopes
    mmwr.msm.slope = (data$mmwr$msm.2017$p*msm.correction.rr - data$mmwr$msm.2014$p*msm.correction.rr) / (2017-2014)
  #  sullivan.slopes = apply(data$sullivan$msm.2013.2017$p, 1, function(x){
  #      years = 2013:2017
  #      lm(x*msm.correction.rr~years)$coefficients[2]
  #  })
    msm.slope.lo = log.or.vector['msm_slope'] = logit(mmwr.msm.slope['total'])
    
    # Intercept
    msm.slope.cv = mmwr.msm.slope['total'] / (data$mmwr$msm.2017$p['total'] * msm.correction.rr)
    msm.intercept.lo = log.or.vector['msm'] =
        mean(logit(c(data$mmwr$msm.2014$p['total']*msm.correction.rr * (1 + (anchor.year-2014) * msm.slope.cv),
                     data$nhbs$msm.2017$p['total']*msm.correction.rr * (1 + (anchor.year-2017) * msm.slope.cv),
                     data$ca$msm.2017$p['total']*msm.correction.rr * (1 + (anchor.year-2017) * msm.slope.cv)
    )))

    
    #-- IDU --#
    
    idu.slope = (data$nhbs$idu.2018$p['total']*idu.correction.rr - data$nhbs$idu.2015$p['total']*idu.correction.rr) / (2018-2015)
    idu.slope.lo = log.or.vector['idu_slope'] = logit(idu.slope)
    idu.slope.cv = idu.slope / (data$nhbs$idu.2018$p['total']*idu.correction.rr)
    
    idu.intercept.lo = log.or.vector['idu'] = logit(data$nhbs$idu.2015$p['total']*idu.correction.rr + (anchor.year-2015)*idu.slope)
    
    
    #-- Heterosexual --#
    
    het.slope.cv = idu.slope.cv
    het.slope.lo = log.or.vector['heterosexual_slope'] = logit(het.slope.cv * data$nhbs$het.2016$p['total']*het.correction.rr)
    
    het.intercept.lo = log.or.vector['heterosexual'] = logit(data$nhbs$het.2016$p['total']*het.correction.rr * (1+(anchor.year-2016)*het.slope.cv))
    
    
    #-- Race --#
    
    mmwr.msm.slope = (data$mmwr$msm.2017$p*msm.correction.rr - data$mmwr$msm.2014$p*msm.correction.rr) / (2017-2014)
    black.slope.lor = log.or.vector['black_slope'] =
        logit(mmwr.msm.slope['black']) - logit(mmwr.msm.slope['total'])
    hispanic.slope.lor = log.or.vector['hispanic_slope'] =
        logit(mmwr.msm.slope['hispanic']) - logit(mmwr.msm.slope['total'])
    other.slope.lor = log.or.vector['other_slope'] =
        logit(mmwr.msm.slope['white']) - logit(mmwr.msm.slope['total'])
    
    mmwr.msm.slope.cv = mmwr.msm.slope / (data$mmwr$msm.2017$p*msm.correction.rr)
    

    
    black.lor = log.or.vector['black'] =
        mean(c(logit(data$nhbs$msm.2017$p['black']*msm.correction.rr) - 
                   logit(data$nhbs$msm.2017$p['total']*msm.correction.rr),
               logit(data$mmwr$msm.2014$p['black']*msm.correction.rr) - 
                   logit(data$mmwr$msm.2014$p['total']*msm.correction.rr),
               logit(data$ca$msm.2017$p['black']*msm.correction.rr) - 
                   logit(data$ca$msm.2017$p['total']*msm.correction.rr),
               logit(data$shover$msm.2015.2018$p['black']*msm.correction.rr) - 
                   logit(data$shover$msm.2015.2018$p['total']*msm.correction.rr)
        ))
    
    
    hispanic.lor = log.or.vector['hispanic'] =
        mean(c(logit(data$nhbs$msm.2017$p['hispanic']*msm.correction.rr) - 
                           logit(data$nhbs$msm.2017$p['total']*msm.correction.rr),
                       logit(data$mmwr$msm.2014$p['hispanic']*msm.correction.rr) - 
                           logit(data$mmwr$msm.2014$p['total']*msm.correction.rr),
                       logit(data$ca$msm.2017$p['hispanic']*msm.correction.rr) - 
                           logit(data$ca$msm.2017$p['total']*msm.correction.rr),
               logit(data$shover$msm.2015.2018$p['hispanic']*msm.correction.rr) - 
                   logit(data$shover$msm.2015.2018$p['total']*msm.correction.rr)
    ))
    
    other.lor = log.or.vector['other'] =
        mean(c(logit(data$nhbs$msm.2017$p['white']*msm.correction.rr) - 
                   logit(data$nhbs$msm.2017$p['total']*msm.correction.rr),
               logit(data$mmwr$msm.2014$p['white']*msm.correction.rr) - 
                   logit(data$mmwr$msm.2014$p['total']*msm.correction.rr),
               logit(data$ca$msm.2017$p['white']*msm.correction.rr) - 
                   logit(data$ca$msm.2017$p['total']*msm.correction.rr),
               logit(data$shover$msm.2015.2018$p['white']*msm.correction.rr) - 
                   logit(data$shover$msm.2015.2018$p['total']*msm.correction.rr)
        ))
    
    
    #-- Age --#
    
    
    ages = paste0('age', 1:5)
    age.correction.rrs = c(age1=age1.correction.rr,
                           age2=age2.correction.rr,
                           age3=age3.correction.rr,
                           age4=age4.correction.rr,
                           age5=age5.correction.rr)
    p.total = (data$nhbs$msm.2017$p['total']*msm.correction.rr * data$nhbs$msm.2017$n['total'] +
                   data$nhbs$idu.2018$p['total']*idu.correction.rr * data$nhbs$idu.2018$n['total']) /
        (data$nhbs$msm.2017$n['total'] + data$nhbs$idu.2018$n['total'])
    p.age = (data$nhbs$msm.2017$p[ages]*msm.correction.rr*age.correction.rrs * data$nhbs$msm.2017$n[ages] +
                 data$nhbs$idu.2018$p[ages]*idu.correction.rr*age.correction.rrs * data$nhbs$idu.2018$n[ages]) /
        (data$nhbs$msm.2017$n[ages] + data$nhbs$idu.2018$n[ages])
    
    age.lors = log.or.vector[ages] = logit(p.age) - logit(p.total)
    
    #-- Male vs Female --#
    
    p.total = (data$nhbs$idu.2018$p['total']*idu.correction.rr * data$nhbs$idu.2018$n['total'] +
                   data$nhbs$het.2016$p['total']*het.correction.rr * data$nhbs$het.2016$n['total']) /
        (data$nhbs$idu.2018$n['total'] + data$nhbs$het.2016$n['total'])
    p.male = (data$nhbs$idu.2018$p['male']*idu.correction.rr * data$nhbs$idu.2018$n['male'] +
                  data$nhbs$het.2016$p['male']*het.correction.rr * data$nhbs$het.2016$n['male']) /
        (data$nhbs$idu.2018$n['male'] + data$nhbs$het.2016$n['male'])
    p.female = (data$nhbs$idu.2018$p['female']*idu.correction.rr * data$nhbs$idu.2018$n['female'] +
                    data$nhbs$het.2016$p['female']*het.correction.rr * data$nhbs$het.2016$n['female']) /
        (data$nhbs$idu.2018$n['female'] + data$nhbs$het.2016$n['female'])
    
    male.lor = log.or.vector['male'] = logit(p.male) - logit(p.total)
    female.lor = log.or.vector['female'] = logit(p.female) - logit(p.total)
    
    
    #-- Put It Together --#
    
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    intercept = slope = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    # Slopes - Risk
    slope[,,c('heterosexual_male','female'),'never_IDU'] = 
        slope[,,c('heterosexual_male','female'),'never_IDU'] + het.slope.lo
    slope[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] =
        slope[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] + idu.slope.lo
    slope[,,'msm',] =
        slope[,,'msm',] + msm.slope.lo
    
    # Intercepts - Risk
    intercept[,,c('heterosexual_male','female'),'never_IDU'] = 
        intercept[,,c('heterosexual_male','female'),'never_IDU'] + het.intercept.lo
    intercept[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] =
        intercept[,,c('heterosexual_male','female'),c('active_IDU','IDU_in_remission')] + idu.intercept.lo
    intercept[,,'msm',] =
        intercept[,,'msm',] + msm.intercept.lo
    
    # Log ORs - Race
    intercept[,'black',,] = 
        intercept[,'black',,] + black.lor
    intercept[,'hispanic',,] = 
        intercept[,'hispanic',,] + hispanic.lor
    intercept[,'other',,] = 
        intercept[,'other',,] + other.lor
    
    slope[,'black',,] = 
        slope[,'black',,] + black.slope.lor
    slope[,'hispanic',,] = 
        slope[,'hispanic',,] + hispanic.slope.lor
    slope[,'other',,] = 
        slope[,'other',,] + other.slope.lor
    
    # Log ORs - Age
    for (age in 1:length(age.lors))
    {
        intercept[age,,,] = 
            intercept[age,,,] + age.lors[age]
        
        slope[age,,,] = 
            slope[age,,,] + age.lors[age]
    }
    
    # Log ORs - Male/Female
    intercept[,,'heterosexual_male',] = 
        intercept[,,'heterosexual_male',] + male.lor
    intercept[,,'female',] = 
        intercept[,,'female',] + female.lor
    
    intercept[,,'heterosexual_male',] = 
        intercept[,,'heterosexual_male',] + male.lor
    intercept[,,'female',] = 
        intercept[,,'female',] + female.lor
    
    # Make the Model and Return
    create.logistic.tail.functional.form(intercept = intercept,
                                         slope = slope,
                                         anchor.year = anchor.year,
                                         min = min.proportion,
                                         max = max.proportion, 
                                         intercept.link = 'logistic',
                                         slope.link = 'logistic',
                                         parameters.are.on.transformed.scale = T,
                                         #  log.ors = log.or.vector, 
                                         logistic.after.frac.of.span = logistic.after.frac.of.p.span)
}

make.marginal.rr.array <- function(p, n)
{
    ages = paste0('age',1:5)
    races.to = c('black','hispanic','other')
    races.from = c('black','hispanic','white')
    
    male.female = any(names(p)=='female')
    if (male.female)
        sexes = c('heterosexual_male','female')
    else
    {
        sexes = 'msm'
        if (is.null(dim(n)))
            n = matrix(n[ages], ncol=1, dimnames=list(age=ages, sex='msm'))
    }
    
    dim.names = list(age=ages, race=races.to, sex=sexes)
    rv = array(1, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (sex in sexes)
    {
        for (age in ages)
            rv[age,races.to,sex] = p[races.from]
    }
    
    age.rrs = p[ages] / p[ages][3]
    age.n.proportion = n[ages] / sum(n[ages])
    
    for (sex in sexes)
    {
        for (race in races.to)
            rv[ages,race,sex] = rv[ages,race,sex] * age.rrs
    }
    
    if (male.female)
    {
        rr.female = p['female'] / p['male']
        rv[,,'female'] = rv[,,'female'] * rr.female
    }
    
    for (r in 1:length(races.from))
    {
        for (sex in sexes)
        {
            raw.race.p = sum(rv[,r,sex]*n[,sex])/sum(n[,sex])
            mult = sum(p[races.from[r]]) / raw.race.p
            
            rv[,r,sex] = rv[,r,sex]*mult
        }
    }
    
    if (any(rv>1))
        stop("The stratifying process produces a p>1")
    rv
}
    
get.prep.lit.data <- function(as.proportion.of.indicated=T,
                              specification.metadata)
{
    rv = list()
    
    rv$nhbs = list()
    
    
    #-- NHBS IDU 2015 --#
    
    rv$nhbs$idu.2015 = list(p=c(total=.003),
                            p.indcated=c(total=.606))
    if (as.proportion.of.indicated)
        rv$nhbs$idu.2015$p = rv$nhbs$idu.2015$p * PREP.PERSISTENCE['total'] / .606 #needle sharing, table 10
    
    
    #-- NHBS IDU 2018 --#
    
    rv$nhbs$idu.2018 = list()
    #Based off of needle sharing, table 10
    rv$nhbs$idu.2018$p.indicated = p.indicated.nhbs.idu = 
        c(total=.598,
          male=.585,
          female=.626,
          age1=.720,
          age2=.705,
          age3=.672,
          age4=.596,
          age5=.498,
          black=.498,
          hispanic=.578,
          white=.683) 
    if (!as.proportion.of.indicated)
        p.indicated.nhbs.idu[] = 1
    
    rv$nhbs$idu.2018$p = c(total=0.011,
                           male=0.009,
                           female=0.015,
                           age1=0.017,
                           age2=0.011,
                           age3=0.013,
                           age4=0.014,
                           age5=0.008,
                           black=0.007,
                           hispanic=0.014,
                           white=0.012) * PREP.PERSISTENCE['total'] / p.indicated.nhbs.idu
    
    rv$nhbs$idu.2018$n = c(total=5605,
                        male=3793,
                        female=1771,
                        age1=229,
                        age2=707,
                        age3=1730,
                        age4=1329,
                        age5=1610,
                        black=1173,
                        hispanic=1311,
                        white=2646) * p.indicated.nhbs.idu
    
    rv$nhbs$idu.2018$n.sex.age = cbind(
        heterosexual_male=c(age1=235, age2=787, age3=1967, age4=1793, age5=3044),
        female=c(age1=167, age2=410, age3=1003, age4=818, age5=1027)
    )
    
    
    #-- NHBS MSM 2017 --#
    
    rv$nhbs$msm.2017 = list()
    
    # Based on condomless sex with casual partner, table 6
    rv$nhbs$msm.2017$p.indicated = p.indicated.nhbs.msm =
        c(total=.467,
          age1=.436,
          age2=.515,
          age3=.505,
          age4=.444,
          age5=.367,
          black=.395,
          hispanic=.453,
          white=.518)
    
    rv$nhbs$msm.2017$p = c(total=0.25,
                           age1=0.215,
                           age2=0.280,
                           age3=0.304,
                           age4=0.237,
                           age5=0.139,
                           black=0.188,
                           hispanic=0.212,
                           white=0.309) * PREP.PERSISTENCE['male'] / p.indicated.nhbs.msm
    rv$nhbs$msm.2017$n = c(total=4952,
                           age1=924,
                           age2=1307,
                           age3=1430,
                           age4=669,
                           age5=622,
                           black=1183,
                           hispanic=1426,
                           white=1878) * p.indicated.nhbs.msm
    
    #-- Heterosexual 2016 --#
    
    rv$nhbs$het.2016 = list()
    
    #based off of table 10 STIs
    rv$nhbs$het.2016$p.indicated = p.indicated.nhbs.het = 
        c(total=0.069,
          male=0.054,
          female=0.082,
          age1=0.114,
          age2=0.107,
          age3=0.056,
          age4=0.047,
          age5=0.038,
          black=0.079,
          hispanic=0.037,
          white=0.036)
    if (!as.proportion.of.indicated)
        p.indicated.nhbs.het[] = 1
    
    rv$nhbs$het.2016$p = c(total=0.002,
                           male=0.001,
                           female=0.002,
                           age1=0.001,
                           age2=0.002,
                           age3=0.001,
                           age4=0.001,
                           age5=0.002,
                           black=0.002,
                           hispanic=0,
                           white=0) * PREP.PERSISTENCE['total'] / p.indicated.nhbs.het
    rv$nhbs$het.2016$n = c(total=7316,
                           male=3280,
                           female=4036,
                           age1=1518,
                           age2=1067,
                           age3=1546,
                           age4=1455,
                           age5=1730,
                           black=5245,
                           hispanic=1471,
                           white=221) * p.indicated.nhbs.het

    rv$nhbs$het.2016$n.sex.age = cbind(
        heterosexual_male=c(age1=718, age2=463, age3=644, age4=648, age5=877),
        female=c(age1=805, age2=612, age3=933, age4=850, age5=940)
    )
        
    #-- MMWR MSM 2014-27 --#
    
    # https://www.cdc.gov/mmwr/volumes/68/wr/mm6827a1.htm
    rv$mmwr = list(msm.2014=list(),
                   msm.2017=list())
    rv$mmwr$msm.2014$p = c(total=0.057,
                           black=0.038,
                           hispanic=0.038,
                           white=0.083) * PREP.PERSISTENCE['male']
    
    rv$mmwr$msm.2017$p = c(total=0.351,
                           black=0.262,
                           hispanic=0.300,
                           white=0.424) * PREP.PERSISTENCE['male']
    
    if (!as.proportion.of.indicated)
    {
        p.indicated.msm = get.prep.indications.estimate(specification.metadata=specification.metadata)['msm','never_IDU']
        rv$mmwr$msm.2014$p = rv$mmwr$msm.2014$p / p.indicated.msm
        rv$mmwr$msm.2017$p = rv$mmwr$msm.2017$p / p.indicated.msm
    }
    
    #-- Shover et al 2015-2018 --#
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6137770/
    
    rv$shover = list(
        msm.2015.2018 = list()
    )
    
    rv$shover$msm.2015.2018$p = c(
        total = 0.13,
        age1 = 0.05,
        age2 = 0.10,
        age3 = 0.16,
        age4 = 0.19,
        age5 = 0.16,
        black = 0.11,
        hispanic = 0.08,
        white = 0.17
        
    )
    
    #-- Sullivan et al 2013 - 2017 --#
    
    rv$sullivan = list(
        msm.2013.2017 = list(
            p=cbind(
            '2013' = c(
                total = 0.017,
                age1 = 0.012,
                age2 = 0.022,
                age3 = 0.028,
                age4 = 0.013,
                age5 = 0.013,
                black = 0,
                hispanic = 0.015,
                white = 0.017,
                urban = 0.029
            ),
            '2014' = c(
                total = 0.061,
                age1 = 0.029,
                age2 = 0.061,
                age3 = 0.070,
                age4 = 0.067,
                age5 = 0.067,
                black = 0.051,
                hispanic = 0.060,
                white = 0.061,
                urban = 0.091
            ),
            '2015' = c(
                total = 0.077,
                age1 = 0.047,
                age2 = 0.092,
                age3 = 0.114,
                age4 = 0.076,
                age5 = 0.076,
                black = 0.054,
                hispanic = 0.067,
                white = 0.080,
                urban = 0.106
            ),
            '2016' = c(
                total = 0.134,
                age1 = 0.064,
                age2 = 0.18,
                age3 = 0.188,
                age4 = 0.138,
                age5 = 0.138,
                black = 0.116,
                hispanic = 0.138,
                other = 0.137,
                urban = 0.207
            ),
            '2017' = c(
                total = 0.199,
                age1 = 0.094,
                age2 = 0.246,
                age3 = 0.302,
                age4 = 0.205,
                age5 = 0.205,
                black = 0.231,
                hispanic = 0.163,
                other = 0.137,
                urban = 0.270
            ),
            or = c(
                total = 1,
                age1 = 1.13,
                age2 = 1,
                age3 = 0.98,
                age4 = 0.89,
                age5 = 0.89,
                black = 1.05,
                hispanic= 1,
                other = 1,
                urban = 1
            ))
        )
    )
    
    #-- CA MSM Study young MSM 2017 --#
    
    rv$ca = list(msm.2017=list())
    rv$ca$msm.2017$p = c(total=0.097,
                    black=19/193,
                    hispanic=16/243,
                    white=23/165
    ) * .925 #percent taking 6-7d/wk
    if (as.proportion.of.indicated)
        rv$ca$msm.2017$p = rv$ca$msm.2017$p / .553 #proportion with condomless sex
    #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5140696/ 
    
    
    ## NOT USING THESE
    cdc.total = c('2014'=13748,
                  '2015'=38879,
                  '2016'=78630)
    
  #  chicago.p.msm = c('2015'=.066, '2017'=.175) / p.indicated['msm','never_IDU']
    
    
    rv
}

calculate.msm.prep.correction.rr <- function(prep.manager = ALL.DATA.MANAGERS$prep,
                                             census = ALL.DATA.MANAGERS$census.collapsed.msm,
                                             specification.metadata)
{
    msa.fips = counties.for.msa(dimnames(msa.surveillance$new.all)[['location']])
#    msa.fips = counties.for.msa(TARGET.MSAS)
    fips = intersect(msa.fips, intersect(prep.manager$locations, census$combined.fips))
    
    rx = get.prep.data(prep.manager,
                       locations=fips,
                       years = 2017,
                       sex=T,
                       collapse.locations = F)[1,,'male']
    
    pop = get.census.data(census,
                          fips=fips,
                          years=2017,
                          sexes=c('msm','heterosexual_male'),
                          aggregate.years=T,
                          aggregate.counties = F,
                          aggregate.age=F,
                          aggregate.race=F)
    
    mask = !is.na(rx) & !apply(is.na(pop), 1, any)
    
    # Convert the rx
    rx = sum(rx[mask])
    rx = convert.prep.rx.to.true.prep(rx, description='male')
    
    # Sum up the population
    pop = colSums(pop[mask,,,], dims=1)
    pop = collapse.races(pop)
    
    # Multiply pop by p indicated
    p.indicated = get.prep.indications.estimate(specification.metadata=specification.metadata)[,,c('msm','heterosexual_male'),'never_IDU']
    denominator = sum(pop[,,'msm'] * p.indicated[,,'msm'])
    
    # Compare
    rx.proportion = rx/denominator
    nhbs.proportion = get.prep.lit.data(specification.metadata=specification.metadata)$nhbs$msm.2017$p['total']
    
    rr = rx.proportion / nhbs.proportion
    rr
}


calculate.female.prep.correction.rr <- function(prep.manager = ALL.DATA.MANAGERS$prep,
                                             census = ALL.DATA.MANAGERS$census.collapsed.msm,
                                             specification.metadata)
{
    msa.fips = counties.for.msa(dimnames(msa.surveillance$new.all)[['location']])
    fips = intersect(msa.fips, intersect(prep.manager$locations, census$combined.fips))
    
    rx = get.prep.data(prep.manager,
                       locations=fips,
                       years = 2017,
                       sex=T,
                       collapse.locations = F)[1,,'female']
    
    pop = get.census.data(census,
                          fips=fips,
                          years=2017,
                          sexes='female',
                          aggregate.years=T,
                          aggregate.counties = F,
                          aggregate.age=F,
                          aggregate.race=F)[,,,1]
    
    mask = !is.na(rx) & !apply(is.na(pop), 1, any)
    
    # Convert the rx
    rx = sum(rx[mask])
    rx = convert.prep.rx.to.true.prep(rx, description='female')
    
    # Sum up the population
    pop = colSums(pop[mask,,])
    pop = collapse.races(pop)
    
    # Multiply pop by p indicated
    p.indicated = get.prep.indications.estimate(specification.metadata=specification.metadata)[,,'female','never_IDU']
    denominator = sum(pop * p.indicated)
    
    # Compare
    rx.proportion = rx/denominator
    nhbs.proportion = get.prep.lit.data(specification.metadata=specification.metadata)$nhbs$het.2016$p['female']
    
    rr = rx.proportion / nhbs.proportion
    rr
}


calculate.age.prep.correction.rr <- function(prep.manager = ALL.DATA.MANAGERS$prep,
                                                census = ALL.DATA.MANAGERS$census.collapsed.msm,
                                             relative.to.age = 3,
                                             specification.metadata)
{
    msa.fips = counties.for.msa(dimnames(msa.surveillance$new.all)[['location']])
    fips = intersect(msa.fips, intersect(prep.manager$locations, census$combined.fips))
    
    rx = get.prep.data(prep.manager,
                       locations=fips,
                       years = 2017,
                       age=T,
                       collapse.locations = F)[1,,]
    mask = !apply(is.na(rx), 1, any)
    fips = fips[mask]
    rx = rx[mask,]
    
    pop = get.census.data(census,
                          fips=fips,
                          years=2017,
                          #sexes='msm',
                          aggregate.years=T,
                          aggregate.counties = F,
                          aggregate.age=F,
                          aggregate.race=F)[,,,]
    
    
    # Convert the rx
    rx = convert.prep.rx.to.true.prep(rx)
    rx = colSums(rx)
    
    # Sum up the population
    pop = colSums(pop)
    pop = collapse.races(pop)
    
    # Multiply pop by p indicated
    p.indicated = get.prep.indications.estimate(specification.metadata=specification.metadata)[,,,'never_IDU']
    denominator = rowSums(pop * p.indicated)
    
    # Compare
    rx.proportion = rx/denominator
    nhbs.proportion = get.prep.lit.data(specification.metadata=specification.metadata)$nhbs$msm$p.indicated[paste0('age',1:5)]
    
    rr = rx.proportion / nhbs.proportion
    if (!is.null(relative.to.age))
        rr = rr/rr[relative.to.age]
    rr
}
