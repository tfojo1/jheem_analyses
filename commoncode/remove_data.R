remove.data(data.manager = SURVEILLANCE.MANAGER,
            outcome = "diagnosed.prevalence",
            source = "cdc.aggregated.county",
            ontology.name = "cdc",
            dimension.values = list(year=as.character(2008:2014),
                                    location="C.40140"))

# need to add in all other stratifications here - is there an easier way to do this? 

# this call didn't do anything
remove.data(data.manager = SURVEILLANCE.MANAGER,
            outcome = "diagnosed.prevalence",
            source = "cdc.aggregated.county",
            ontology.name = "cdc",
            dimension.values = list(year=as.character(2008:2014),
                                    location="C.40140",
                                    age=c("13-24 years","25-34 years","35-44 years","45-54 years","55+ years")))


remove.data(data.manager = SURVEILLANCE.MANAGER,
            outcome = "diagnosed.prevalence",
            source = "cdc.surveillance.reports",
            ontology.name = "cdc.msa.reports",
            dimension.values = list(year=as.character(2014),
                                    location="C.40140"))


sapply(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc, function(x) {
  RIVERSIDE.MSA %in% dimnames(x)$location})

