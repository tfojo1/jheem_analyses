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

# READ STATUS of ongoing runs
# for(x in names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")] ){
#     print(MSAS.OF.INTEREST[MSAS.OF.INTEREST==x])
#     status<-get.calibration.progress('shield', locations = x, calibration.code = "calib.3.30.stage1.pk")
#     print(status)
# }
# (msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")]
#    

# # Reads complete runs from andrew
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")],
                   calibration.codes = "calib.3.16.stage1.az",sim.id = 1)

simM2<-extract.last.simulation.from.calibration('shield', location = "C.33100", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simH2<-extract.last.simulation.from.calibration('shield', location = "C.26420", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simC2<-extract.last.simulation.from.calibration('shield', location = "C.16980", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simA2<-extract.last.simulation.from.calibration('shield', location = "C.12060", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simP2<-extract.last.simulation.from.calibration('shield', location = "C.38060", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simN2<-extract.last.simulation.from.calibration('shield', location = "C.35620", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)
simL2<-extract.last.simulation.from.calibration('shield', location = "C.31080", calibration.code = "calib.3.30.stage1.pk",allow.incomplete = T,include.first.sim = T)

simN1<-extract.last.simulation.from.calibration('shield', location = "C.35620", calibration.code = "calib.3.16.stage1.az",allow.incomplete = T,include.first.sim = T)

# # Reads complete runs Sim1
assign_simset_vars(names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N")],
                   calibration.codes = "calib.3.16.stage1.az")
# new sims: Sim2
sim2=simA2;sim1=simsetA1
sim2=simM2;sim1=simsetM1
# sim2=simB2;sim1=simsetB1
sim2=simL2;sim1=simsetL1
# sim2=simN2;sim1=simsetN1
sim2=simP2;sim1=simsetP1
sim2=simH2;sim1=simsetH1
sim2=simC2;sim1=simsetC1

simplot(
    sim1$last.sim(), #sim1$first.sim(),
    sim2$last.sim(), #sim2$first.sim(),
    # outcomes = c("diagnosis.primary.symptomatic","diagnosis.secondary.symptomatic","diagnosis.primary.asymptomatic","diagnosis.secondary.asymptomatic")
    outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing")
)    
