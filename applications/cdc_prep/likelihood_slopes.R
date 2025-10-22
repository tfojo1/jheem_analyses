

#Load simset

load("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/test.AL.sim.Rdata")

source('applications/cdc_prep/cdc_prep_specification.R')
source('applications/cdc_prep/Surveillance_Manager_Updates.R')



transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2010, to.year = 2035)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

sim.cdcp = transmuter$transmute(1, parameters = params)


print("Sourcing initial code")
source('applications/cdc_prep/register_cdc_prep_calibration.R')

print("Doing setup calibration")

results = set.up.transmute.calibration('cdcp', 
                                       from.calibration.code = 'final.ehe.state', 
                                       location = 'AL', 
                                       n.chunks = 1,
                                       n.sim = 10,
                                       allow.overwrite.cache = TRUE,
                                       return.simulations = T)

results = results$burn(500)

sim = results$last.sim()
x =sim$get("cdc.funded.tests")
sim.ratio = x["2028",,]/x["2023",,]

sim.ratio2 = 1.2

dlnorm(sim.ratio2, -.25, .69)/dlnorm(sim.ratio, -.25, .69)

z <- seq(.25,4,length = 1000)

fn <- function(ratio){
 log.mean = -.25 # to change
 log.sd = 0.44
 
 if((log(ratio) < (log.mean + log.sd)) &&  (log(ratio) > (log.mean - log.sd)) ){
  dnorm((log.mean + log.sd),log.mean, log.sd) * 1/ratio
 } else {
   dnorm(log(ratio),log.mean,log.sd) * 1/ratio
 }
 
}

qplot(z,sapply(z,fn))

states = dimnames(SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location)[[2]]

z<- sapply(states,function(st){y = SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location[,st]
slope = coefficients(lm(log(y)~as.numeric(names(y))))[2]
return(exp(slope*5))})

logR5 <- log(unlist(z))
sd_logR5 <- sd(logR5, na.rm = TRUE)


      