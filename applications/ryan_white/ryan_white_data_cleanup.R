RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

melted = reshape2::melt(RW.DATA.MANAGER$data$adap.suppression$estimate$nastad.adap$ryan.white.pdfs$year__location)

melted = melted[!is.na(melted$value) & melted$value==0,]

if (nrow(melted)>0)
{
    for (i in 1:nrow(melted))
    {
      print(melted[i,])
        RW.DATA.MANAGER$put(data = as.numeric(NA),
                            outcome='adap.suppression', 
                            source = 'nastad.adap', 
                            ontology.name = 'ryan.white.pdfs', 
                            dimension.values = list(location=as.character(melted$location[i]),
                                                    year=as.character(melted$year[i])),
                            allow.na.to.overwrite = T,
                            url = "https://nastad.org/adap-monitoring-project",
                            details = "NASTAD PDF Reports", 
                            is.removal = T)
      
      RW.DATA.MANAGER$put(data = as.numeric(NA),
                          outcome='adap.suppressed.proportion.of.diagnosed', 
                          source = 'ryan.white.program', 
                          ontology.name = 'ryan.white.pdfs', 
                          dimension.values = list(location=as.character(melted$location[i]),
                                                  year=as.character(melted$year[i])),
                          allow.na.to.overwrite = T,
                          url = "https://nastad.org/adap-monitoring-project",
                          details = "NASTAD PDF Reports", 
                          is.removal = T)
    }
}

strats.to.do = names(RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs)
strats.to.do = strats.to.do[!grepl('fpl', strats.to.do)]
for (strat in strats.to.do)
{
    strat.data = RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs[[strat]]
    strat.adap = RW.DATA.MANAGER$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs[[strat]]
    
    msa.mask = get.location.type(dimnames(strat.data)$location)=='CBSA'
    msas = dimnames(strat.data)$location[msa.mask]
    prevalence = RW.DATA.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc[[strat]]
    overlapping.years = intersect(dimnames(strat.data)$year, dimnames(prevalence)$year)
    adap.overlapping.years = intersect(overlapping.years, dimnames(strat.adap)$year)
    
    print(paste0(strat, " - MSAs"))
    for (msa in msas)
    {
        counties = get.contained.locations(msa, 'COUNTY')

        if (length(setdiff(counties, dimnames(prevalence)$location))==0)
        {
            county.prevalence = array.access(prevalence, year=overlapping.years, location=counties)
          
            msa.data = array.access(strat.data, year=overlapping.years, location=msa)
            dim.names = dimnames(msa.data)[names(dimnames(msa.data))!='location']
            dim(msa.data) = sapply(dim.names, length)
            dimnames(msa.data) = dim.names
          
            mapping = get.ontology.mapping(dimnames(msa.data), dimnames(county.prevalence)[names(dim(county.prevalence))!='location'])
            if (is.null(mapping))
                stop("Cannot get mapping")
            msa.data = mapping$apply(msa.data)
            
            msa.p.non.adap = msa.data / apply(county.prevalence, setdiff(names(dim(county.prevalence)), 'location'), sum, na.rm=T)
            
            county.non.adap = county.prevalence * expand.array(msa.p.non.adap, dimnames(county.prevalence))
            
            RW.DATA.MANAGER$put(data = county.non.adap,
                                outcome='non.adap.clients', 
                                source = 'ryan.white.program', 
                                ontology.name = 'cdc', 
                                allow.na.to.overwrite = T,
                                url = "https://ryanwhite.hrsa.gov/data/reports",
                                details = "Ryan White Downloaded PDF Reports")
            
            for (state in get.overlapping.locations(msa, 'STATE'))
            {
                if (state!='PR' && length(adap.overlapping.years)>0)
                {
                    state.adap = array.access(strat.adap, location=state, year=adap.overlapping.years)
                    dim.names = dimnames(state.adap)[names(dimnames(state.adap))!='location']
                    dim(state.adap) = sapply(dim.names, length)
                    dimnames(state.adap) = dim.names
                    state.adap = mapping$apply(state.adap)
                  
                    county.non.adap = array.access(county.non.adap, year=adap.overlapping.years)
                    county.adap =  county.non.adap *
                      expand.array(state.adap, dimnames(county.non.adap))
                    
                    RW.DATA.MANAGER$put(data = county.adap,
                                        outcome='adap.clients', 
                                        source = 'ryan.white.program', 
                                        ontology.name = 'cdc', 
                                        allow.na.to.overwrite = T,
                                        url = "https://ryanwhite.hrsa.gov/data/reports",
                                        details = "Ryan White Downloaded PDF Reports")
                }
            }
        }
    }
    
    state.mask = get.location.type(dimnames(strat.data)$location)=='STATE'
    states = dimnames(strat.data)$location[state.mask]
    
    print(paste0(strat, " - States"))
    for (state in states)
    {
#      print(paste0(state, " - ", strat))
        counties = get.contained.locations(state, 'COUNTY')
        contained.msas = intersect(msas, get.overlapping.locations(state, 'CBSA'))
        
        if (length(contained.msas)==0)
            counties.in.state.out.of.msa = counties
        else
            counties.in.state.out.of.msa = setdiff(counties, get.contained.locations(contained.msas, 'COUNTY'))
        
        if (length(counties.in.state.out.of.msa)>0)
        {
            if (length(setdiff(counties, dimnames(prevalence)$location))==0)
            {
                county.prevalence = array.access(prevalence, year=overlapping.years, location=counties)
                fraction.contained.msa.prevalence.in.state = sapply(contained.msas, function(msa){
                    
                    msa.counties = get.contained.locations(msa, 'county')
                    msa.prevalence = array.access(prevalence, year=overlapping.years, location=msa.counties)
                    
                    msa.counties.in.state = intersect(msa.counties, counties)
                    msa.prevalence.in.state = array.access(county.prevalence, location=msa.counties.in.state)
                    
                    sum(msa.prevalence.in.state, na.rm=T) / sum(msa.prevalence, na.rm=T)
                  
                })
                
                contained.msas.to.consider = contained.msas[fraction.contained.msa.prevalence.in.state > 0.4]
                if (length(contained.msas.to.consider)==0)
                    counties.in.state.out.of.msas.to.consider = counties
                else
                    counties.in.state.out.of.msas.to.consider = setdiff(counties, get.contained.locations(contained.msas.to.consider, 'COUNTY'))
                
                in.state.out.of.msa.n.non.adap = array.access(strat.data, year=overlapping.years, location=state)
                dim.names = dimnames(in.state.out.of.msa.n.non.adap)[names(dimnames(in.state.out.of.msa.n.non.adap))!='location']
                dim(in.state.out.of.msa.n.non.adap) = sapply(dim.names, length)
                dimnames(in.state.out.of.msa.n.non.adap) = dim.names
                
                
                use.na.rm = strat == 'year__location'
                
                in.state.out.of.msas.to.consider.prevalence = apply(array.access(county.prevalence, location=counties.in.state.out.of.msas.to.consider), setdiff(names(dim(county.prevalence)), 'location'), sum,
                                                       na.rm = use.na.rm) 

                mapping = get.ontology.mapping(dimnames(in.state.out.of.msa.n.non.adap), dimnames(county.prevalence)[names(dim(county.prevalence))!='location'])
                if (is.null(mapping))
                    stop("Cannot get mapping")
                in.state.out.of.msa.n.non.adap = mapping$apply(in.state.out.of.msa.n.non.adap)
                in.state.n.non.adap = in.state.out.of.msa.n.non.adap
                
                
                for (msa in contained.msas.to.consider)
                {
                    msa.n.non.adap = array.access(strat.data, year=overlapping.years, location=msa)
                    dim.names = dimnames(msa.n.non.adap)[names(dimnames(msa.n.non.adap))!='location']
                    dim(msa.n.non.adap) = sapply(dim.names, length)
                    dimnames(msa.n.non.adap) = dim.names
                  
                    msa.n.non.adap = mapping$apply(msa.n.non.adap)
                
                    counties.in.msa = get.contained.locations(msa, 'county')
                    prevalence.in.msa = apply(array.access(prevalence, year=overlapping.years, location=counties.in.msa), 
                                              setdiff(names(dim(prevalence)), 'location'),
                                              sum, na.rm=use.na.rm)    
                    
                    counties.in.state.and.msa = intersect(counties.in.msa, counties)
                    prevalence.in.state.and.msa = apply(array.access(prevalence, year=overlapping.years, location=counties.in.state.and.msa), 
                                              setdiff(names(dim(prevalence)), 'location'),
                                              sum, na.rm=use.na.rm)    
                    
                    msa.n.non.adap = msa.n.non.adap * prevalence.in.state.and.msa/prevalence.in.msa
                    
                    in.state.out.of.msa.n.non.adap = in.state.out.of.msa.n.non.adap - msa.n.non.adap
                }
                
                if (any(!is.na(in.state.out.of.msa.n.non.adap) & in.state.out.of.msa.n.non.adap<0))
                {
                    print(paste0(strat, " - ", state, " - est in.state.out.of.msa.n.non.adap < 0 - using state totals and ignoring MSA effects"))
                    in.state.prevalence = apply(county.prevalence, setdiff(names(dim(county.prevalence)), 'location'), 
                                                sum, na.rm = use.na.rm) 
                    
                    in.state.out.of.msa.p.non.adap = in.state.n.non.adap / in.state.out.of.msas.to.consider.prevalence
                }
                else
                    in.state.out.of.msa.p.non.adap = in.state.out.of.msa.n.non.adap / in.state.out.of.msas.to.consider.prevalence

                counties.in.state.out.of.msa.prevalence = array.access(county.prevalence, location=counties.in.state.out.of.msa)
                county.non.adap = counties.in.state.out.of.msa.prevalence *
                                                 expand.array(in.state.out.of.msa.p.non.adap, dimnames(counties.in.state.out.of.msa.prevalence))

#                county.non.adap[!is.na(county.non.adap) & county.non.adap<0] = NA
                if (length(county.non.adap)==0 || any(!is.na(county.non.adap) & county.non.adap<0))
                    browser()
                RW.DATA.MANAGER$put(data = county.non.adap,
                                    outcome='non.adap.clients', 
                                    source = 'ryan.white.program', 
                                    ontology.name = 'cdc', 
                                    allow.na.to.overwrite = T,
                                    url = "https://ryanwhite.hrsa.gov/data/reports",
                                    details = "Ryan White Downloaded PDF Reports")
                
                if (state!='PR' && length(adap.overlapping.years)>0)
                {
                    state.adap = array.access(strat.adap, location=state, year=adap.overlapping.years)
                    dim.names = dimnames(state.adap)[names(dimnames(state.adap))!='location']
                    dim(state.adap) = sapply(dim.names, length)
                    dimnames(state.adap) = dim.names
                    state.adap = mapping$apply(state.adap)
                    
                    county.non.adap = array.access(county.non.adap, year=adap.overlapping.years)
                    county.adap = county.non.adap *
                      expand.array(state.adap, dimnames(county.non.adap))
                    
                    RW.DATA.MANAGER$put(data = county.adap,
                                        outcome='adap.clients', 
                                        source = 'ryan.white.program', 
                                        ontology.name = 'cdc', 
                                        allow.na.to.overwrite = T,
                                        url = "https://ryanwhite.hrsa.gov/data/reports",
                                        details = "Ryan White Downloaded PDF Reports")
                }
            }
        }
    }
}

RW.DATA.MANAGER$put(data = as.numeric(NA),
                    dimension.values = list(
                        year = '2022',
                        location = 'C.19100',
                        race = RW.DATA.MANAGER$ontologies$ryan.white.pdfs$race
                    ),
                    outcome='oahs.clients', 
                    source = 'ryan.white.program', 
                    ontology.name = 'ryan.white.pdfs', 
                    allow.na.to.overwrite = T,
                    url = "https://ryanwhite.hrsa.gov/data/reports",
                    details = "Ryan White Downloaded PDF Reports")

RW.DATA.MANAGER$put(data = as.numeric(NA),
                    dimension.values = list(
                      year = '2022',
                      location = 'C.19100',
                      race = RW.DATA.MANAGER$ontologies$ryan.white.pdfs$race
                    ),
                    outcome='oahs.suppression', 
                    source = 'ryan.white.program', 
                    ontology.name = 'ryan.white.pdfs', 
                    allow.na.to.overwrite = T,
                    url = "https://ryanwhite.hrsa.gov/data/reports",
                    details = "Ryan White Downloaded PDF Reports")

RW.DATA.MANAGER$put(data = as.numeric(NA),
                    dimension.values = list(
                      year = '2022',
                      location = 'C.12580',
                      race = RW.DATA.MANAGER$ontologies$ryan.white.pdfs$race
                    ),
                    outcome='oahs.clients', 
                    source = 'ryan.white.program', 
                    ontology.name = 'ryan.white.pdfs', 
                    allow.na.to.overwrite = T,
                    url = "https://ryanwhite.hrsa.gov/data/reports",
                    details = "Ryan White Downloaded PDF Reports")

save(RW.DATA.MANAGER, file='../../cached/ryan.white.data.manager.rdata')
