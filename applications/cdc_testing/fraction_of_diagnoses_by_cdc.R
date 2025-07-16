

n.cdc.dx = SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location *
    SURVEILLANCE.MANAGER$data$cdc.hiv.test.positivity$estimate$cdc.testing$cdc$year__location

n.dx = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location

locs = intersect(dimnames(n.cdc.dx)$location,
                 dimnames(n.dx)$location)


frac.dx.by.cdc.2021 = round(n.cdc.dx['2021',locs]) / n.dx['2021',locs]
    
sort(frac.dx.by.cdc.2021)
