#This code processes data that was manually pulled from health department websites:

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
    mutate(value = as.numeric(LHD.Value))%>%
    filter(outcome != "cns.syphilis.diagnoses")%>%  #removing these for now
    filter(outcome != "primary.syphilis.diagnoses")%>%
    filter(outcome != "secondary.syphilis.diagnoses")
 
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