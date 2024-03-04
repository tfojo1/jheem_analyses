# Initiation from NSDUH: https://datatools.samhsa.gov/#/ 
    # Used weighted heroin and cocaine rates over 2018-2020: 
    #   numerator: number in each age group who initiated in the prior year (combined numerators over 3 years)
    #   denominator: total in age group (combined denominators over 3 years)
# Remission/relapse from Shah et al: https://pubmed.ncbi.nlm.nih.gov/16364568/ 

if (1==2){
  source('applications/EHE/ehe_specification.R')
  source('commoncode/age_mappings.R')
  specification.metadata = get.specification.metadata('ehe', 
                                                      location = 'c.12580')
}

##-----------------------##
##--  HELPER FUNCTIONS --##
##-----------------------##
# initiation helper functions (combining numerators, denominators over 3 years and two data types (heroin and cocaine))
generate.iniation.numerators = function(data.type,
                                        year){
  
  file.dir = paste0("../jheem_analyses/data_files/idu_initiation")
  
  if(data.type=="heroin"){
    file = list.files(file.path(file.dir,year),"HER")
  } else if (data.type=="cocaine"){
    file = list.files(file.path(file.dir,year),"COC")
  } else stop("incorrect data type (must be heroin or cocaine)")
  
  df = read.csv(file.path(file.dir,year,file))
  df = df[,c(1:3,(ncol(df)-2),(ncol(df)-1))]
  df = df[df$RC.EVER.USED.NEEDLE.TO.INJECT.DRUGS=="1 - Yes",]
  df = df[,-1]
  
  names(df) = c("age.when.used","age","weighted.count","unweighted.count")
  df = df[df$age.when.used!="Overall",]
  df = df[df$age!="Overall",]
  df = df[order(df$age.when.used),]
  df$age.when.used = suppressWarnings(as.numeric(df$age.when.used))
  df = df[!is.na(df$age.when.used),]
  
  ages = c("12","21","22-23","24-25","26-29","30-34","35-49","50-64","65+","13","14","15","16","17","18","19","20")
  
  df$age = rep(ages,(nrow(df)/17))
  
  df = df[order(df$age,df$age.when.used),]
  
  dim.names = list(age.when.used = unique(df$age.when.used),
                   age = sort(ages),
                   value = c("weighted","unweighted"))
  
  arr = array(0,
              dim = sapply(dim.names,length),
              dimnames = dim.names)
  
  arr[,,"weighted"] = df$weighted.count
  arr[,,"unweighted"] = df$unweighted.count
  
  arr = aperm(arr,c(2,1,3))
  
  initiated.prior.year = array(NA,
                               dim = sapply(dim.names[-1],length),
                               dimnames = dim.names[-1])
  
  initiated.prior.year["12",] = 0
  initiated.prior.year["13",] = arr["13","12",] # 13-year-olds who initiated when they were 12
  initiated.prior.year["14",] = arr["14","13",] # 14-year-olds who initiated when they were 13
  initiated.prior.year["15",] = arr["15","14",] # ...so on
  initiated.prior.year["16",] = arr["16","15",]
  initiated.prior.year["17",] = arr["17","16",]
  initiated.prior.year["18",] = arr["18","17",]
  initiated.prior.year["19",] = arr["19","18",]
  initiated.prior.year["20",] = arr["20","19",]
  initiated.prior.year["21",] = arr["21","20",]
  initiated.prior.year["22-23",] = arr["22-23","23",] + arr["22-23","22",] + arr["22-23","21",]*0.5
  initiated.prior.year["24-25",] = arr["24-25","25",] + arr["24-25","24",] + arr["24-25","23",]*0.5
  
  initiated.prior.year["26-29",] = arr["26-29","29",] + arr["26-29","28",] + arr["26-29","27",]*0.5 + 
    arr["26-29","26",]*(1/3) + arr["26-29","25",]*(1/4) 
  
  initiated.prior.year["30-34",] = arr["30-34","34",] + arr["30-34","33",] + arr["30-34","32",]*0.5 + 
    arr["30-34","31",]*(1/3) + arr["30-34","30",]*(1/4) + arr["30-34","29",]*(1/5) 
  
  if(year=="2020"){
    if(data.type=="heroin"){
      initiated.prior.year["35-49",] = arr["35-49","48",] + 
        #arr["35-49","47",]*0.5 + 
        arr["35-49","46",]*(1/3) + arr["35-49","45",]*(1/4) + 
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = 
        arr["50-64","63",] + arr["50-64","62",]*0.5 + 
        arr["50-64","52",]*(1/12)  + 
        arr["50-64","50",]*(1/14) 
      
      initiated.prior.year["65+",] = arr["65+","70",]*(1/32)
    } else if(data.type=="cocaine"){
      initiated.prior.year["35-49",] = arr["35-49","47",]*0.5 + 
        arr["35-49","45",]*(1/4) + arr["35-49","44",]*(1/5) +
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = arr["50-64","55",]*(1/9) + arr["50-64","54",]*(1/10) +
        arr["50-64","51",]*(1/13) + arr["50-64","50",]*(1/14)
      
      initiated.prior.year["65+",] = 0 
    }
    
  } else if(year=="2019"){
    if(data.type=="heroin"){
      initiated.prior.year["35-49",] = arr["35-49","49",] + # arr["35-49","48",] + 
        arr["35-49","47",]*0.5 + # arr["35-49","46",]*(1/3) + 
        arr["35-49","45",]*(1/4) + arr["35-49","44",]*(1/5) +
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = 
        # arr["50-64","64",] + arr["50-64","63",] + arr["50-64","62",]*0.5 + 
        # arr["50-64","61",]*(1/3) + arr["50-64","60",]*(1/4) + arr["50-64","59",]*(1/5) +
        # arr["50-64","58",]*(1/6) + 
        arr["50-64","57",]*(1/7) + 
        arr["50-64","56",]*(1/8) + arr["50-64","55",]*(1/9) + arr["50-64","54",]*(1/10) +
        # arr["50-64","53",]*(1/11) + arr["50-64","52",]*(1/12)  + arr["50-64","51",]*(1/13) 
        arr["50-64","50",]*(1/14) + arr["50-64","49",]*(1/15)
      
      initiated.prior.year["65+",] = arr["65+","68",]*(1/30) + arr["65+","70",]*(1/32)
      
    } else if(data.type=="cocaine"){
      initiated.prior.year["35-49",] = arr["35-49","49",] + arr["35-49","48",] + arr["35-49","47",]*0.5 + 
        arr["35-49","46",]*(1/3) + arr["35-49","45",]*(1/4) + arr["35-49","44",]*(1/5) +
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = 
        # arr["50-64","64",] + arr["50-64","63",] + arr["50-64","62",]*0.5 + 
        # arr["50-64","61",]*(1/3) + arr["50-64","60",]*(1/4) + arr["50-64","59",]*(1/5) +
        # arr["50-64","58",]*(1/6) + arr["50-64","57",]*(1/7) + 
        arr["50-64","56",]*(1/8) + arr["50-64","55",]*(1/9) + arr["50-64","54",]*(1/10) +
        arr["50-64","53",]*(1/11) + arr["50-64","52",]*(1/12)  + #arr["50-64","51",]*(1/13) 
        arr["50-64","50",]*(1/14) + arr["50-64","49",]*(1/15)
      
      initiated.prior.year["65+",] = arr["65+","72",]*(1/28)    
    }
  } else if(year=="2018"){
    if(data.type=="heroin"){
      initiated.prior.year["35-49",] = arr["35-49","49",] + # arr["35-49","48",] + 
        arr["35-49","46",]*(1/3) + 
        arr["35-49","45",]*(1/4) + arr["35-49","44",]*(1/5) +
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = 
        arr["50-64","56",]*(1/8) + arr["50-64","55",]*(1/9) 
      arr["50-64","51",]*(1/13) + arr["50-64","50",]*(1/14) + arr["50-64","49",]*(1/15)
      
      initiated.prior.year["65+",] = 0
      
    } else if(data.type=="cocaine"){
      initiated.prior.year["35-49",] = arr["35-49","49",] + # arr["35-49","48",] + 
        arr["35-49","47",]*0.5 + 
        arr["35-49","46",]*(1/3) + arr["35-49","45",]*(1/4) + arr["35-49","44",]*(1/5) +
        arr["35-49","43",]*(1/6) + arr["35-49","42",]*(1/7) + arr["35-49","41",]*(1/8) +
        arr["35-49","40",]*(1/9) + arr["35-49","39",]*(1/10) + arr["35-49","38",]*(1/11) +
        arr["35-49","37",]*(1/12) + arr["35-49","36",]*(1/13) + arr["35-49","35",]*(1/14) +
        arr["35-49","34",]*(1/15)
      
      initiated.prior.year["50-64",] = 
        arr["50-64","62",]*0.5 + 
        arr["50-64","60",]*(1/4) + arr["50-64","55",]*(1/9) + 
        arr["50-64","53",]*(1/11) + arr["50-64","52",]*(1/12)  
      arr["50-64","50",]*(1/14) + arr["50-64","49",]*(1/15)
      
      initiated.prior.year["65+",] = 0
    }
  }
  
  initiated.prior.year
  
}

generate.initiation.denominators = function(year){
  
  file.dir = paste0("../jheem_analyses/data_files/idu_initiation")
  
  ages = c("12","21","22-23","24-25","26-29","30-34","35-49","50-64","65+","13","14","15","16","17","18","19","20")
  
  age.totals = read.csv(file.path(file.dir,year,"AGE2.csv"))
  age.totals = age.totals[-1,c(1,ncol(age.totals)-1,ncol(age.totals)-2)]
  age.totals[,1] = ages
  age.totals = age.totals[order(age.totals$RECODE...FINAL.EDITED.AGE),]
  
  dim.names = list(age = sort(ages),
                   value = c("weighted","unweighted"))
  
  age.array = array(c(age.totals[,3],age.totals[,2]),
                    dim = sapply(dim.names,length),
                    dimnames = dim.names)
}


generate.multi.year.estimates.by.data.type = function(data.type,
                                                      years){
  
  numerators = list()
  denominators = list()
  
  for(year in years){
    numerators[[year]] = generate.iniation.numerators(data.type = data.type,
                                                      year = year)
    denominators[[year]] = generate.initiation.denominators(year = year)
  }
  
  numerators.combined = Reduce("+",numerators)
  denominators.combined = Reduce("+",denominators)

  rv = (numerators.combined/denominators.combined)
  
  rv
  
}

generate.multi.year.estimates.total = function(years){
  
  heroin.rate = generate.multi.year.estimates.by.data.type(data.type = "heroin",
                                                           years = years)
  cocaine.rate = generate.multi.year.estimates.by.data.type(data.type = "cocaine",
                                                            years = years)
  
  rv = heroin.rate + cocaine.rate
  
  rv
  
}

##-------------------##
##--  RETURN RATES --##
##-------------------##
get.idu.incidence.rates <- function(specification.metadata)
{
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  
  rates.2018.to.2020 = generate.multi.year.estimates.total(years = c("2018","2019","2020"))
  
  ## mapped estimates 
  ages.sorted = sort(c("12","21","22-23","24-25","26-29","30-34","35-49",
                       "50-64","65+","13","14","15","16","17","18","19","20"))
  
  age.info.initiation = parse.age.strata.names(ages.sorted)
  
  weighted.mapped = map.age.values(values = rates.2018.to.2020[,1],
                                   given.age.lowers = age.info.initiation$lower,
                                   given.age.uppers = age.info.initiation$upper, 
                                   desired.ages = dim.names$age)
  
  rv = array(rep(weighted.mapped,9), # this is going to fill in assuming that age is the first dimension 
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv
}

get.idu.remission.rates <- function(specification.metadata)
{
  dim.names = specification.metadata$dim.names[c('age','race','sex')]

  overall.remission = (936/5553) # 936 remission events/5553 py --> 16.8/100py, Shah et al. 
  
  remission.age = 1/(c(0.69,0.87,0.98,1)) # inverted time ratio
  names(remission.age) = c("0-29 years","30-34 years","35-39 years","40+ years")
  
  remission.race = 1/c(1.34)
  names(remission.race) = c("black")
  
  remission.sex = 1/c(0.65)
  names(remission.sex) = c("msm")
  
  age.info.remission = parse.age.strata.names(names(remission.age))
  
  remission.age.mapped = map.age.values(values = overall.remission*remission.age,
                                        given.age.lowers = age.info.remission$lower,
                                        given.age.uppers = age.info.remission$upper, 
                                        desired.ages = dim.names$age)
  
  rv = array(rep(remission.age.mapped,9), # this is going to fill in assuming that age is the first dimension 
             dim=sapply(dim.names, length),
             dimnames = dim.names)

  rv[,"black",] = rv[,"black",]*remission.race
  rv[,,"msm"] = rv[,,"msm"]*remission.sex
  
  rv 
}

get.idu.relapse.rates <- function(specification.metadata)
{
  
  dim.names = specification.metadata$dim.names[c('age','race','sex')]

  overall.relapse = (678/1727) # 678 relapse events/1727 py --> 39.2/100py, Shah et al. 

  relapse.race = 1/c(1.40)
  names(relapse.race) = c("black")
  
  relapse.sex = 1/c(1.22)
  names(relapse.sex) = c("female")
  
  rv = array(overall.relapse, # this is going to fill in assuming that age is the first dimension 
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv[,"black",] = rv[,"black",]*relapse.race
  rv[,,"female"] = rv[,,"female"]*relapse.sex
  
  rv 
}

##------------------------------##
##--  RETURN FUNCTIONAL FORMS --##
##------------------------------##
get.incident.idu.model <- function(specification.metadata,
                                   static,
                                   knot.times=c(2000,2016))
{
  rates = get.idu.incidence.rates(specification.metadata = specification.metadata)
  
  if (static)
    create.static.functional.form(value=rates, link='log')
  else
  {
    knot.values = lapply(1:length(knot.times), function(i){rates})
    names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
    create.natural.spline.functional.form(knot.times = knot.times,
                                          knot.values = knot.values,
                                          before.time = min(knot.times)-10,
                                          after.time = max(knot.times)+10,
                                          link='log',
                                          after.modifier = .025 / (1-.05-.025),
                                          before.modifier = .05 / (1-.05-.025),
                                          modifiers.apply.to.change = T)
  }
}

get.idu.remission.model <- function(specification.metadata,
                                    static,
                                    knot.times=c(2000,2016))
{
  rates = get.idu.remission.rates(specification.metadata = specification.metadata)
  
  if (static)
    create.static.functional.form(value=rates, link='log')
  else    
  {
    knot.values = lapply(1:length(knot.times), function(i){rates})
    names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
    create.natural.spline.functional.form(knot.times = knot.times,
                                          knot.values = knot.values,
                                          before.time = min(knot.times)-10,
                                          after.time = max(knot.times)+10,
                                          link='log',
                                          after.modifier = .025 / (1-.05-.025),
                                          before.modifier = .05 / (1-.05-.025),
                                          modifiers.apply.to.change = T)
  }
}

get.idu.relapse.model <- function(specification.metadata,
                                  static,
                                  knot.times=c(2000,2016))
{
  rates = get.idu.relapse.rates(specification.metadata = specification.metadata)
  if (static)
    create.static.functional.form(value=rates, link='log')
  else
  {
    knot.values = lapply(1:length(knot.times), function(i){rates})
    names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
    create.natural.spline.functional.form(knot.times = knot.times,
                                          knot.values = knot.values,
                                          before.time = min(knot.times)-10,
                                          after.time = max(knot.times)+10,
                                          link='log',
                                          after.modifier = .025 / (1-.05-.025),
                                          before.modifier = .05 / (1-.05-.025),
                                          modifiers.apply.to.change = T)
  }
}


##-------------------------##
##-- AVAILABILITY BY AGE --##
##-------------------------##


get.idu.availability.13.24 <- function(dir='../jheem_analyses/data_files/idu_age_availability', years=2015:2018)
{
    arr = get.idu.by.age.counts(dir=dir, years=years)
    
    arr = get.idu.by.age.counts(dir=dir, years=years)
    
    ages = as.character(13:24)
    dim.names = list(age=ages, use=c('active','prior','never'))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    rv[1:9,] = arr[2:10,]
    rv['22',] = arr[11,] / 2
    rv['23',] = arr[11,] / 2
    rv['24',] = arr[12,] / 2
    
    rv = rv/rowSums(rv)
    
    c('13-14'=sum(rv[as.character(13:14),1]) / sum(rv[as.character(19:24),1]),
      '15-18'=sum(rv[as.character(15:18),1]) / sum(rv[as.character(19:24),1]),
      '19-24'=1)
}

get.idu.by.age.counts <- function(dir, years=2015:2018)
{
    age.mapping = c('1 - Respondent is 12 years old'='12 yo',
                    '2 - Respondent is 13 years old'='13 yo',
                    '3 - Respondent is 14 years old'='14 yo',
                    '4 - Respondent is 15 years old'='15 yo',
                    '5 - Respondent is 16 years old'='16 yo',
                    '6 - Respondent is 17 years old'='17 yo',
                    '7 - Respondent is 18 years old'='18 yo',
                    '8 - Respondent is 19 years old'='19 yo',
                    '9 - Respondent is 20 years old'='20 yo',
                    '10 - Respondent is 21 years old'='21 yo',
                    '11 - Respondent is 22 or 23 years old'='22-23 yo',
                    '12 - Respondent is 24 or 25 years old'='24-25 yo',
                    '13 - Respondent is between 26 and 29 years old'= '26-29 yo',
                    '14 - Respondent is between 30 and 34 years old'= '30-34 yo',
                    '15 - Respondent is between 35 and 49 years old'= '35-49 yo',
                    '16 - Respondent is between 50 and 64 years old'= '50-64 yo',
                    '17 - Respondent is 65 years old or older'= '65+yo',
                    'Overall'=NA)

    use.mapping = c('1 - Within the past 30 days'= 'Within_30d',
                    '13 - More than 12 months ago LOGICALLY ASSIGNED'= 'More_than_30d',
                    '2 - More than 30 days ago but within the past 12 mos'= 'More_than_30d',
                    '3 - More than 12 months ago'= 'More_than_30d',
                    '81 - NEVER USED COC/HER/STM W/NEEDLE Log assn'= 'Never',
                    '9 - At some point in the lifetime LOG ASSN'= 'More_than_30d',
                    '91 - NEVER USED COC/HER/STM WITH A NEEDLE'= "Never",
                    '97 - REFUSED'= NA,
                    '98 - MISSING'= NA,
                    'Overall'= NA)

    ages = age.mapping[!is.na(age.mapping)]
    uses = c('Within_30d', 'More_than_30d', 'Never')
    dim.names = list(age=ages, use=uses)
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    for (year in years)
    {
        file = file.path(dir, paste0('NSDUH_', year, '_IDU_single_age.csv'))
        df = read.csv(file, stringsAsFactors = F)

        for (i in 1:dim(df)[1])
        {
            age = age.mapping[df$RECODE...FINAL.EDITED.AGE[i]]
            use = use.mapping[df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE[i]]

            if (!is.na(age) && !is.na(use))
                rv[age, use] =  rv[age, use] + df$Weighted.Count[i]
        }
    }

    rv
}
