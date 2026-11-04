oakland <- c('TGA.OAKLAND')
oakland.counties <- c('06001', '06013')
#Oakland TGA = Alameda county (06001) + Contra Costa County (06013)

#===============================================================================

#This Code calculates STI ratios for the Oakland TGA

#===============================================================================

#GONORRHEA RATIOS

gonorrhea.oakland.total = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
gonorrhea.oakland.total <- gonorrhea.oakland.total %>%
    group_by(year)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

gonorrhea.oakland.age = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location__age, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
gonorrhea.oakland.age <- gonorrhea.oakland.age %>%
    group_by(year, age)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

gonorrhea.oakland.sex = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location__sex, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
gonorrhea.oakland.sex <- gonorrhea.oakland.sex %>%
    group_by(year, sex)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

gonorrhea.oakland.race = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location__race, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
gonorrhea.oakland.race <- gonorrhea.oakland.race %>%
    group_by(year, race)%>%
    mutate(value = sum(Freq))%>%
    ungroup()

gonorrhea.data.list = list(
    "gc.total" = gonorrhea.oakland.total, 
    "gc.sex" = gonorrhea.oakland.sex,
    "gc.race" =gonorrhea.oakland.race, 
    "gc.age" = gonorrhea.oakland.age)

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
    
    data = file[["data"]]
    filename = file[[2]]
    
    data = as.data.frame(data)
    
    data <- data %>%
        select(-location, -Freq) %>%
        mutate(location = "TGA.OAKLAND",
               year = as.numeric(year)) %>%
        distinct()
    
    if(grepl("total", filename)) { 
        
        data <- data %>%
            group_by(location) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, year.over.year) %>%
            rename(value = year.over.year)
        
    } else if(grepl("sex", filename)) {
        
        data <- data %>%
            group_by(location, sex) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, sex, year.over.year) %>%
            rename(value = year.over.year)
        
    } else if(grepl("race", filename)) { 
        
        data <- data %>%
            group_by(location, race) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, race, year.over.year) %>%
            rename(value = year.over.year) %>%
            mutate(race = tolower(race))
        
    } else if(grepl("age", filename)) { 
        
        data <- data %>%
            group_by(location, age) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, age, year.over.year) %>%
            rename(value = year.over.year)
    }
    
    data$outcome = "gonorrhea.ratio"
    data$year = as.character(data$year)
    data = as.data.frame(data)
    
    list(filename, data) 
    
})

################################################################################
#PUT Data- Gonorrhea
################################################################################
gc.ratio.put = lapply(gonorrhea.ratios, `[[`, 2)

for (data in gc.ratio.put) {
    
    surveillance.manager$put.long.form(
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

syphilis.oakland = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
syphilis.oakland <- syphilis.oakland %>%
    group_by(year)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

syphilis.oakland.age = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location__age, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
syphilis.oakland.age <- syphilis.oakland.age %>%
    group_by(year, age)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

syphilis.oakland.sex = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location__sex, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
syphilis.oakland.sex <- syphilis.oakland.sex %>%
    group_by(year, sex)%>%
    mutate(value = sum(Freq))%>%
    ungroup()
##

syphilis.oakland.race = as.data.frame.table(surveillance.manager$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location__race, stringsAsFactors = FALSE)%>% filter(location %in% oakland.counties)
syphilis.oakland.race <- syphilis.oakland.race %>%
    group_by(year, race)%>%
    mutate(value = sum(Freq))%>%
    ungroup()


syphilis.data.list = list(
    "syph.total" = syphilis.oakland, 
    "syph.sex" = syphilis.oakland.sex,
    "syph.race" =syphilis.oakland.race, 
    "syph.age" = syphilis.oakland.age)

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
    
    data = file[["data"]]
    filename = file[[2]]
    
    data = as.data.frame(data)
    
    data <- data %>%
        select(-location, -Freq) %>%
        mutate(location = "TGA.OAKLAND",
               year = as.numeric(year)) %>%
        distinct()
    
    if(grepl("total", filename)) { 
        
        data <- data %>%
            group_by(location) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, year.over.year) %>%
            rename(value = year.over.year)
        
    } else if(grepl("sex", filename)) {
        
        data <- data %>%
            group_by(location, sex) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, sex, year.over.year) %>%
            rename(value = year.over.year)
        
    } else if(grepl("race", filename)) { 
        
        data <- data %>%
            group_by(location, race) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, race, year.over.year) %>%
            rename(value = year.over.year) %>%
            mutate(race = tolower(race))
        
    } else if(grepl("age", filename)) { 
        
        data <- data %>%
            group_by(location, age) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(year.over.year = value / lag(value, 1)) %>%
            mutate(year.over.year = ifelse(is.finite(year.over.year), 
                                           year.over.year, NA)) %>%
            select(location, year, age, year.over.year) %>%
            rename(value = year.over.year)
    }
    
    data$outcome = "ps.syphilis.ratio"
    data$year = as.character(data$year)
    data = as.data.frame(data)
    
    list(filename, data) 
    
})
################################################################################
#PUT Data- syphilis
################################################################################
syph.ratio.put = lapply(syphilis.ratios, `[[`, 2)

for (data in syph.ratio.put) {
    
    surveillance.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.sti',
        source = 'cdc.sti',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Reporting')
}