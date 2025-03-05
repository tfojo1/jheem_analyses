#This pulls Table 1 from the 2022 report which reports syphilis cases from 1941-2022
#Pulling from 2022 so we have the most recent report of the historic data


# Read in -----------------------------------------------------------------

DATA.DIR.2022.TABLE="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.older/2022.table"

table.2022 <- Sys.glob(paste0(DATA.DIR.2022.TABLE, '/*.xlsx'))

table.2022.raw <- lapply(table.2022, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})


# Clean -------------------------------------------------------------------

syphilis.1941.2022 = lapply(table.2022.raw, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

  data$year = as.character(data$Year)
  
data <- data %>%
  select(year, `Total Syphilis Cases`, `Primary and Secondary Cases`, `Early Non-Primary Non-Secondary Cases`, `Unknown Duration or Late Cases`, `Congenital Cases` )%>%
  pivot_longer(cols = contains("cases"),
               names_to = "outcome",
               values_to = "value")%>%
  mutate(outcome = case_when(outcome == "Total Syphilis Cases" ~ "total.syphilis.diagnoses",
                             outcome == "Primary and Secondary Cases" ~ "ps.syphilis.diagnoses",
                             outcome == "Early Non-Primary Non-Secondary Cases" ~ "early.syphilis.diagnoses",
                             outcome == "Unknown Duration or Late Cases" ~ "unknown.duration.or.late.syphilis.diagnoses",
                             outcome == "Congenital Cases" ~ "congenital.syphilis.diagnoses"))

data$location = "US"

data= as.data.frame(data)

list(filename, data)
})

# Put ---------------------------------------------------------------------

syphilis.1941.2022.put = lapply(syphilis.1941.2022, `[[`, 2)

for (data in syphilis.1941.2022.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.pdf.report',
    source = 'cdc.sti.surveillance.reports',
    url = 'https://www.cdc.gov/sti-statistics/media/pdfs/2024/11/2022-STI-Surveillance-Report-PDF.pdf',
    details = 'CDC STI Surveillance Reports')
}
