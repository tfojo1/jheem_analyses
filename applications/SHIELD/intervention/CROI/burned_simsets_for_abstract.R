

simset.MIA      <-  run_calibration(
    upload_method  = "root",
    date           = "2025-09-20",
    calibration_code = "calib.MIA.09.19.2way.2",
    location       = "C.33100"
)
simset.NYC     <-  run_calibration(
    upload_method  = "root",
    date           = "2025-09-20",
    calibration_code = "calib.NYC.09.19.2way.2",
    location       = "C.35620"
)
simset.ATL     <-  run_calibration(
    upload_method  = "root",
    date           = "2025-09-20",
    calibration_code = "calib.ATL.09.19.2way.2",
    location       = "C.12060"
)
simset.BLT     <-  run_calibration(
    upload_method  = "root",
    date           = "2025-09-20",
    calibration_code = "calib.BLT.09.19.2way.2",
    location       = "C.12580"
)

simset.NYC = simset.NYC.32$burn(keep = 100)
simset.MIA = simset.MIA$burn(keep = 100)
simset.ATL = simset.ATL$burn(keep = 100)
simset.BLT = simset.BLT$burn(keep = 100)

save(simset.NYC, file= "prelim_results/NYC.09.19.2025.rdata")
save(simset.MIA, file= "prelim_results/MIA.09.19.2025.rdata")
save(simset.ATL, file= "prelim_results/ATL.09.19.2025.rdata")
save(simset.BLT, file= "prelim_results/ATL.09.19.2025.rdata")
    