
#LOCATION = "C.32820"  
LOCATION = 'C.18140'
print(paste0("Location = ", LOCATION))
source('applications/ryan_white/ryan_white_main.R')


set.up.fit.rw.simset(location = LOCATION,
                     n.sim = N.SIM,
                     calibration.code = CALIBRATION.CODE,
                     n.chunks = N.CHUNKS,
# n.iter.first.sim = 20,
                     verbose=T)
