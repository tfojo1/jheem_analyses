#This code processes data that was manually pulled from health department websites:

#I'm removing any LHDs that did not report what counties make up their data points
#I'm combining primary and secondary syphilis into ps.syphilis

DATA.DIR.LHD="../../data_raw/syphilis.manager/lhd"

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

