#This code processes data that was manually pulled from health department websites:

#I'm removing any LHDs that did not report what counties make up their data points
#I'm combining primary and secondary syphilis into ps.syphilis

DATA.DIR.LHD=file.path(Q_ROOT, "data_raw/syphilis.manager/lhd")

lhd.files <- Sys.glob(paste0(DATA.DIR.LHD, '/*.xlsx'))

lhd.data <- lapply(lhd.files, function(x){
  list(filename=x, data=read_excel(x))
})

# Clean -------------------------------------------------------------------

lhd.data.clean = lapply(lhd.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    mutate(year = as.character(year))%>%
    filter(LHD.Value != "Not Available")%>%
      filter(`Counties Included in MSA (per the data)` != 'Not stated')%>% #remove counties that don't report what counties their data represents
    mutate(value = as.numeric(LHD.Value))%>%
    filter(outcome != "cns.syphilis.diagnoses")%>%  #removing neurosyphilis for now
    select(outcome, location, year, value)%>%
    mutate(outcome = ifelse(outcome == "primary.syphilis.diagnoses" | outcome == "secondary.syphilis.diagnoses", "ps.syphilis.diagnoses", outcome))%>% #combining primary+secondary diagnoses into ps.syphilis
    group_by(year, location, outcome)%>%
     mutate(regrouped.value = sum(value))
      
   data = as.data.frame(data)
  
   list(filename, data) 
  
})

# Put ---------------------------------------------------------------------

lhd.data.clean.put = lapply(lhd.data.clean, `[[`, 2)  

for (data in lhd.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.sti',
    source = 'lhd',
    dimension.values = list(),
    url = 'na',
    details = 'Data pulled from Local Health Department websites')
}

#============================================================================
#Adding in Updated Data for 2024 (and a few years prior)
#Parastu wants this for doxy calibration (April 2026)

#I'm going to put this several ways:
#1: data by county per outcome
#2: data summed to MSA per outcome
#3: MSA summed to total diagnoses
#============================================================================
DATA.DIR.LHD.NEW=file.path(Q_ROOT, "data_raw/syphilis.manager/lhd/lhd.updated")

lhd.files.new <- Sys.glob(paste0(DATA.DIR.LHD.NEW, '/*.xlsx'))

lhd.data.new <- lapply(lhd.files.new, function(x){
    list(filename=x, data=read_excel(x))
})

# Put ---------------------------------------------------------------------
#1: Put data by county per outcome:
county.data <- lhd.data.new[[1]][[2]]
county.data <- county.data%>%
    select(outcome, year, county.code, value)%>%
    rename(location = county.code)
county.data <- as.data.frame(county.data)
    
    data.manager$put.long.form(
        data = county.data,
        ontology.name = 'cdc.sti',
        source = 'lhd',
        dimension.values = list(),
        url = 'na',
        details = 'Data pulled from Local Health Department websites')
    
    #2:data summed to MSA per outcome
    by.msa <- lhd.data.new[[1]][[2]]
    by.msa <- by.msa%>%
        select(outcome, year, associated.msa.code, value)%>%
        rename(location = associated.msa.code)%>%
        group_by(outcome, year, location)%>%
        mutate(new.value = sum(value))%>%
        select(-value)%>%
        rename(value = new.value)
    by.msa <- as.data.frame(by.msa)
    
    data.manager$put.long.form(
        data = by.msa,
        ontology.name = 'cdc.sti',
        source = 'lhd',
        dimension.values = list(),
        url = 'na',
        details = 'Data pulled from Local Health Department websites')
    
    #3: Sum to total.diagnoses (LA only)
    la.total <- lhd.data.new[[1]][[2]]
    la.total <- la.total%>%
        filter(associated.msa.name == "Los Angeles")%>%
        select(year, associated.msa.code, value)%>%
        rename(location = associated.msa.code)%>%
        group_by(year, location)%>%
        mutate(new.value = sum(value))%>%
        select(-value)%>%
        rename(value = new.value)%>%
        mutate(outcome = "total.syphilis.diagnoses")
    la.total <- as.data.frame(unique(la.total))
    
    data.manager$put.long.form(
        data = la.total,
        ontology.name = 'cdc.sti',
        source = 'lhd',
        dimension.values = list(),
        url = 'na',
        details = 'Data pulled from Local Health Department websites')
