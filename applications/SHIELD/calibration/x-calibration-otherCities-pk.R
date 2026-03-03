
# Configuration ----
MSAS.OF.INTEREST

# LOCATION="C.26420" #houston
simsetH1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.26420",  version = 'shield', n.sim = 400)
# get.calibration.progress('shield', locations = "C.26420", calibration.code = "calib.3.2.stage1.az")
# simsetH0 <- assemble.simulations.from.calibration(calibration.code = "calib.3.2.stage1.az", location = "C.26420",  version = 'shield', allow.incomplete = T)


# LOCATION="C.31080" #LA
simsetLA1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.31080",  version = 'shield', n.sim = 400)

# LOCATION="C.38060" #Phoenix
simsetPH1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.38060",  version = 'shield', n.sim = 400)

# LOCATION="C.16980" #Chicago
simsetC1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.16980",  version = 'shield', n.sim = 400)

# LOCATION="C.35620" #NYC
simsetNY1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.35620",  version = 'shield', n.sim = 400)

# LOCATION="C.12060 #Atlanta
simsetA1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.12060",  version = 'shield', n.sim = 400)

# LOCATION="C.12580" #Baltimore
simsetB1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.12580",  version = 'shield', n.sim = 400)

# LOCATION="C.33100" #Miami
simsetM1 <- retrieve.simulation.set(calibration.code = "calib.3.2.stage1.pk", location = "C.33100",  version = 'shield', n.sim = 400)




# PLOT -----
simplot(
    # simsetH1$last.sim(), 
    simsetH1[350:400], 
    # simsetB1$last.sim(),
    # simsetC1$last.sim(),
    # simsetLA1$last.sim(),
    # simsetPH1$last.sim(),
    # simsetNY1$last.sim(),
    # simsetM1$last.sim(),
    
    # outcomes=c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing","sti.screening")
    # outcomes = c("diagnosis.ps") , split.by = "race" ,facet.by = "sex"
    outcomes = c("hiv.testing") , split.by = "race" #,facet.by = "sex"
    
    # ,dimension.values = list(year = 2010:2030)
    ,style.manager = source.style.manager
)
 
