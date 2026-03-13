# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")




source('../jheem_analyses/applications/SHIELD/calibration/calibration_inspection_helpers.R')

# READ STATUS of ongoing runs
for(x in names(msa_var_names)[msa_var_names %in% c("PH","B","M","A","H")] ){    
    print(MSAS.OF.INTEREST[MSAS.OF.INTEREST==x])
    status<-get.calibration.progress('shield', locations = x, calibration.code = "calib.3.12.stage1.az")
    print(status)
}

# Reads complete runs 
assign_simset_vars(MSAS.OF.INTEREST,
                   calibration.codes = "calib.3.10.stage0.az")
# read older stage0 calib for Atlanta
# assign_simset_vars("C.12060",sim.id = 1,
#                    calibration.codes = "calib.2.27.stage0.az")

stg0simplot(
    lastA
    # lastAU #wide understimation- not following fetility u turn
    # lastB
     # lastBO  #underestimate among whites
    # lastBR # no PS syphilis
    # lastC #eponential PS syphilis
    # lastCD #no syphilis
    # lastCI #no syphilis
    # lastCO
    # lastCT
    # lastD #understimate pop & no syphilis
        # lastH #underestimate 

)    
