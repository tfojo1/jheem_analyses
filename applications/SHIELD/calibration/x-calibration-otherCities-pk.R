
# Configuration ----
MSAS.OF.INTEREST
VERSION='shield'

LOCATION="C.26420" #houston
#repeating stage0 & 1 with the new model using the 2X larger weight for HIV testing and transmission knot in 2017
simsetHouston19 <- retrieve.simulation.set("shield",
                                         location = LOCATION,calibration.code = "calib.2.19.stage1.az", n.sim = 400)

LOCATION="C.31080" #LA
simsetLA190 <- retrieve.simulation.set("shield",
                                      location = LOCATION,calibration.code = "calib.2.19.stage0.az", n.sim = 400)
simsetLA19 <- retrieve.simulation.set("shield",
                                           location = LOCATION,calibration.code = "calib.2.19.stage1.az", n.sim = 400)


LOCATION="C.38060" #Phoenix
simsetPhoenix19 <- retrieve.simulation.set("shield",
                                      location = LOCATION,calibration.code = "calib.2.19.stage1.az", n.sim = 400)

LOCATION="C.16980" #Chicago
simsetChicago19 <- retrieve.simulation.set("shield",
                                           location = LOCATION,calibration.code = "calib.2.19.stage1.az", n.sim = 400)


# DATA doesnt show for population 

# PLOT -----
simplot(
    # simsetHouston19$last.sim(),
    simsetLA190$last.sim(),
    # simsetPhoenix19$last.sim(),
    # simsetChicago19$last.sim(),
    
    outcomes = c("population") 
    , split.by = "race"
    ,facet.by = "age"
    # outcomes = c("fertility.rate"),    split.by = "race", facet.by = "age"
    # outcomes = c("deaths")
    # outcomes = c("immigration","emigration")    #split.by = "race"
    # outcomes = c("emigration") #split.by = "race", facet.by = "age"
    # outcomes=c("diagnosis.ps")
    # outcomes=c("diagnosis.total")
    # outcomes=c("diagnosis.total","diagnosis.ps",
    #            "diagnosis.el.misclassified","diagnosis.ll.misclassified",
    #            "hiv.testing","sti.screening")
    # ,split.by = "race"
    # ,facet.by = "sex"
    
    # outcomes=c("hiv.testing")
    # ,dimension.values = list(year = 2010:2030)
    ,style.manager = source.style.manager
)

