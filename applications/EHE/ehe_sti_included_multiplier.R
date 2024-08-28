source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')


STI.sds = c(rep(0,length(MSAS.OF.INTEREST)))
names(STI.sds) = c(unname(MSAS.OF.INTEREST))

STI.weights = STI.sds
  
for(location in MSAS.OF.INTEREST){
  x = SURVEILLANCE.MANAGER$data$gonorrhea.ratio$estimate$cdc.sti$cdc.sti$year__location[,location]
  x2 = x[-1]/x[-length(x)]
  y = SURVEILLANCE.MANAGER$data$ps.syphilis.ratio$estimate$cdc.sti$cdc.sti$year__location[,location]
  y2 = y[-1]/y[-length(y)]
  #x2/y2
  #sd(x2/y2)
  
  weight = mean(SURVEILLANCE.MANAGER$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location[,location],na.rm = T)
  
  STI.sds[location] = sd(c(x2/y2,y2/x2),na.rm = T)
  STI.weights[location] = weight
}

STI.included.multiplier = weighted.mean(STI.sds,STI.weights)
# divide by 2 to account for measurement error - assume that half of this SD is due to measurement error
STI.included.multiplier = STI.included.multiplier/2 

STI.included.multiplier # 0.1232901
