#This code pulls older data from the PDF CDC STI Surveillance Reports
#Note: I am pulling the most recent report of each year of data- so data for 1993 comes
#from the 1997 report.  


DATA.DIR.PDF.REPORTS="../../data_raw/syphilis.manager/cdc.syphilis.reports/syphilis.tables.1941.1999"

pdf.reports <- Sys.glob(paste0(DATA.DIR.PDF.REPORTS, '/*.csv'))

cdc.pdf.reports <- lapply(pdf.reports, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})