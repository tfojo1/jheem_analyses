

LOCATION = 'C.32820' 
#LOCATION = 'C.18140'
CHUNKS = 1:4

print(paste0("Location = ", LOCATION, " chunks = ", paste0(CHUNKS, collapse=', ')))
source('applications/ryan_white/ryan_white_main.R')



print(ggplot2::qplot(1,1) + 
          ggplot2::ggtitle(paste0(LOCATION, " - ", paste0("chunks ", paste0(CHUNKS, collapse=',')))))


fit.rw.simset.chunk(location = LOCATION,
                    chunks = CHUNKS,
                    n.sim = N.SIM,
                    calibration.code = CALIBRATION.CODE,
                    verbose = T)

simset = assemble.rw.simset(location = LOCATION,
                            chunks = 1:N.CHUNKS,
                            n.sim = N.SIM,
                            calibration.code = CALIBRATION.CODE,
                            allow.incomplete = F,
                            verbose = T)

simset$save()