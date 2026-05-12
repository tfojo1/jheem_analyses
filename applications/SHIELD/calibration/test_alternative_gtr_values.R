# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/calibration/shield_calibration_inspection_helpers.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")


# Reads completed runs
for (rate in seq(20, 22, by=2)) {
    calibname=(paste0("calib.5.11.stg0.gtr.", rate))
    assign_simset_vars(SHIELD.TEN.MSAS,calibration.codes = calibname,n.sim = 300,sim.id = rate)
}
chars=substring(names(SHIELD.TEN.MSAS),first = 1,1);chars

results <- data.frame(
    msa = character(),
    rate = numeric(),
    mixing_stat = numeric(),
    success = logical(),
    stringsAsFactors = FALSE
)

for (rate in seq(20,22, by = 2)) {
    
    for (msa in c("A","B","C","H","L","M","N","P","PH","S")) {
        
        obj_name <- paste0("simset", msa, rate)
        
        # get the object
        sim_obj <- get(obj_name)
        
        # extract mixing statistic
        mixing_stat <- sim_obj$get.mcmc.mixing.statistic()
        
        # if vector, use first value
        mixing_stat <- mixing_stat[1]
        
        # define success criterion
        success <- mixing_stat < 100000
        
        # store result
        results <- rbind(
            results,
            data.frame(
                msa = msa,
                rate = rate,
                mixing_stat = mixing_stat,
                success = success
            )
        )
    }
}

results

source("../jheem_analyses/applications/SHIELD/calibration/shield_calibration_plots.R")
if (1==1) {
    
    stage=0
    calibname="calib.5.11.stg0.gtr.20"
    
    # Define style managers to use.
    source.style.manager = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
    
    # Retrieve and/or assemble simsets. Only need to run once per session.
    simset_data <- prepare_simsets_for_plots(calibration.code = calibname, 
                                             locations =    SHIELD.TEN.MSAS, 
                                             assemble.incomplete = F)
    
    # Create and save the plots.
    x <- create_plots_for_calibration(stage, calibname, simset_data, create.dirs = T)
}

# 
# simplot(
#     simsetB20$last.sim(),  simsetB20$last.sim(),
#     # simsetH1$last.sim(),  simsetH2$last.sim(),
#     # simsetA1$last.sim(),  simsetA2$last.sim(),
#     # simsetL1$last.sim(),  simsetL2$last.sim(),
#     simsetP0$last.sim(),
#     # outcomes=c("immigration")# facet.by = "age",split.by = "race"
#     outcomes=c("population","diagnosis.ps")
#     # outcomes = c("diagnosis.primary.symptomatic","diagnosis.secondary.symptomatic","diagnosis.primary.asymptomatic","diagnosis.secondary.asymptomatic")
#     # outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing")
#     # ,split.by="sex",plot.which="sim.only"
#     # ,split.by="race"
#     # 
#     # outcomes = c("diagnosis.ps"),
#     # outcomes = c("diagnosis.el.misclassified")
#     # facet.by = "age"
#     # ,split.by="race"
#     # ,split.by="sex"
#     # dimension.values = list(year=c(1970:2025)),
# )    
