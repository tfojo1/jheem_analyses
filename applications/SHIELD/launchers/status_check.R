source("../jheem_analyses/applications/SHIELD/shield_specification.R")
# root.dir = "../jheem_analyses/applications/SHIELD/logs/"
# JHEEM.ANALYSES.PATH="../jheem_analyses/applications/SHIELD/logs/"
calib="calib.9.23.stage2"
root.dir = get.jheem.root.directory();root.dir

get.calibration.progress("shield","C.12060",calib,root.dir = root.dir)
get.calibration.progress("shield","C.12580",calib,root.dir = root.dir)
get.calibration.progress("shield","C.16980",calib,root.dir = root.dir)
get.calibration.progress("shield","C.26420",calib,root.dir = root.dir)
get.calibration.progress("shield","C.31080",calib,root.dir = root.dir)

get.calibration.progress("shield","C.33100",calib,root.dir = root.dir)
get.calibration.progress("shield","C.37980",calib,root.dir = root.dir)
get.calibration.progress("shield","C.38060",calib,root.dir = root.dir)
get.calibration.progress("shield","C.42660",calib,root.dir = root.dir)


calib="calib.9.23.stage2"
get.calibration.progress("shield","C.35620",calib,root.dir = root.dir)

assemble.mcmc.from.calibration()
