#GONORRHEA RATIOS
#Pull aggregated gc data from surveillance manager.

gonorrhea.total = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location, stringsAsFactors = FALSE)
gonorrhea.sex = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location__sex, stringsAsFactors = FALSE)
gonorrhea.age = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location__age, stringsAsFactors = FALSE)
gonorrhea.race = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location__race, stringsAsFactors = FALSE)

gonorrhea.data.list = list(
  "gc.total" = gonorrhea.total, 
  "gc.sex" = gonorrhea.sex,
  "gc.race" =gonorrhea.race, 
  "gc.age" = gonorrhea.age)

gc.data <- lapply(gonorrhea.data.list, function(x){
  list(data = x)
})

gc.data.names = list(
  "gc.total", 
  "gc.sex",
  "gc.race", 
  "gc.age")

gc.data.for.ratios = mapply(c, gc.data, gc.data.names, SIMPLIFY=FALSE)

################################################################################
#Create Ratios- Gonorrhea
################################################################################
gonorrhea.ratios = lapply(gc.data.for.ratios, function(file){
  
  data=file[["data"]]
  filename = file[[2]]
  
  data= as.data.frame(data)
  
  data$year = as.numeric(data$year)
  data$value = data$Freq
  
  if(grepl("total", filename)) { 
    data<- data %>%
      group_by(location)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% #Remove rows without a year over year value (so 2018 bc it's the first year)
      select(location, year, year.over.year)%>%
      rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
  }
  
  if(grepl("sex", filename)) {
    data<- data%>%
      filter(!is.na(value))%>% #because this is so stratified there are certain rows where value = NA bc the 'data is not available'
      group_by(location, sex)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, sex, year.over.year)%>%
      rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
  }
  
  if(grepl("race", filename)) { 
    data<- data %>%
      filter(!is.na(value))%>% 
      group_by(location, race)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, race, year.over.year)%>%
      rename(value = year.over.year) 
  }
  
  if(grepl("age", filename)) { 
    data<-data %>%
      filter(!is.na(value))%>% 
      group_by(location, age)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, age, year.over.year)%>%
      rename(value = year.over.year) 
  }
  
  data$value = ifelse(data$value == "Inf", NA, data$value)
  data$outcome = "gonorrhea.ratio"
  data$year = as.character(data$year)
  data= as.data.frame(data)
  
  list(filename, data) 
  
})
################################################################################
#PUT Data- Gonorrhea
################################################################################
gc.ratio.put = lapply(gonorrhea.ratios, `[[`, 2)

for (data in gc.ratio.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}
################################################################################
################################################################################
#syphilis RATIOS
#Pull aggregated syph data from surveillance manager.

syphilis.total = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location, stringsAsFactors = FALSE)
syphilis.sex = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location__sex, stringsAsFactors = FALSE)
syphilis.age = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location__age, stringsAsFactors = FALSE)
syphilis.race = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location__race, stringsAsFactors = FALSE)

syphilis.data.list = list(
  "syph.total" = syphilis.total, 
  "syph.sex" = syphilis.sex,
  "syph.race" =syphilis.race, 
  "syph.age" = syphilis.age)

syph.data <- lapply(syphilis.data.list, function(x){
  list(data = x)
})

syph.data.names = list(
  "syph.total", 
  "syph.sex",
  "syph.race", 
  "syph.age")

syph.data.for.ratios = mapply(c, syph.data, syph.data.names, SIMPLIFY=FALSE)

################################################################################
#Create Ratios- syphilis
################################################################################
syphilis.ratios = lapply(syph.data.for.ratios, function(file){
  
  data=file[["data"]]
  filename = file[[2]]
  
  data= as.data.frame(data)
  
  data$year = as.numeric(data$year)
  data$value = data$Freq
  
  if(grepl("total", filename)) { 
    data<- data %>%
      group_by(location)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% #Remove rows without a year over year value (so 2018 bc it's the first year)
      select(location, year, year.over.year)%>%
      rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
  }
  
  if(grepl("sex", filename)) {
    data<- data%>%
      filter(!is.na(value))%>% #because this is so stratified there are certain rows where value = NA bc the 'data is not available'
      group_by(location, sex)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, sex, year.over.year)%>%
      rename(value = year.over.year) #Need to figure out how to handle years that have zero cases bc calculation is 10/0 which returns 'inf'
  }
  
  if(grepl("race", filename)) { 
    data<- data %>%
      filter(!is.na(value))%>% 
      group_by(location, race)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, race, year.over.year)%>%
      rename(value = year.over.year) 
  }
  
  if(grepl("age", filename)) { 
    data<-data %>%
      filter(!is.na(value))%>% 
      group_by(location, age)%>%
      arrange(year, .by_group = T)%>%
      mutate(year.over.year=value/lag(value,1)) %>%
      filter(!is.na(year.over.year))%>% 
      select(location, year, age, year.over.year)%>%
      rename(value = year.over.year) 
  }
  
  data$value = ifelse(data$value == "Inf", NA, data$value)
  data$outcome = "ps.syphilis.ratio"
  data$year = as.character(data$year)
  data= as.data.frame(data)
  
  list(filename, data) 
  
})
################################################################################
#PUT Data- syphilis
################################################################################
syph.ratio.put = lapply(syphilis.ratios, `[[`, 2)

for (data in syph.ratio.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'cdc.sti',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}