library(stringr)

## This is the only line you need to change before sourcing this file to create an R Markdown summary 
#file = "prelim_results/full.with.covid_simset_2024-07-15_C.12580.Rdata"
 file = "prelim_results/init.transmission.ehe_simset_2024-07-12_C.12580.Rdata"

load(file)
if(grepl("transmission",simset$calibration.code)){
  calibration = "transmission"
} else if(grepl("full",simset$calibration.code)){
  calibration = "full"
} else 
  stop("missing calibration code")

file.date = str_sub(file, -24, -15)

render_subject <- function(calibration,
                           file,
                           file.date) {
  rmarkdown::render(
    input = "applications/EHE/calibration_runs/markdown_summaries/MCMC_summary.Rmd",
    params = list(set_title = calibration,
                  file = file,
                  file.date = file.date),
    output_file = paste0(calibration, '_output_', Sys.Date(), '.html')
  )
}

render_subject(calibration = calibration,
               file = file,
               file.date = file.date)
