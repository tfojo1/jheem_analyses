# 
# 
# shield2_cities=(
#     C.12060 C.12580 C.16980 C.26420 C.31080
# )
# shield1_cities=(
#     C.33100 C.35620 C.37980
# )
# shield3_cities=(
#     C.38060 C.42660
# )
calib="calib.9.19.stage0"
JHEEM.ANALYSES.PATH="../jheem_analyses/applications/SHIELD/logs/"
calib="calib.9.19.stage2"
root.dir = "../jheem_analyses/applications/SHIELD/logs/"
# SHIELD 2
get.calibration.progress("shield","C.12060",calib,root.dir = root.dir)
get.calibration.progress("shield","C.12580",calib,root.dir = root.dir)
get.calibration.progress("shield","C.16980",calib,root.dir = root.dir)
get.calibration.progress("shield","C.26420",calib,root.dir = root.dir)
get.calibration.progress("shield","C.31080",calib,root.dir = root.dir)

# SHIELD 1
get.calibration.progress("shield","C.33100",calib,root.dir = root.dir)
get.calibration.progress("shield","C.35620",calib,root.dir = root.dir)
get.calibration.progress("shield","C.37980",calib,root.dir = root.dir)

# SHIELD 3
get.calibration.progress("shield","C.38060",calib,root.dir = root.dir)
get.calibration.progress("shield","C.42660",calib,root.dir = root.dir)

