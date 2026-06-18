source("../jheem_analyses/applications/SHIELD/shield_specification.R")
VERSION='shield'
LOCATION="C.12580"
if (1==2) {
    engine1 <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    param.manual1=get.means(SHIELD.FULL.PARAMETERS.PRIOR)
    print("no wrror so far")
    sim.manual1 <- engine1$run(param.manual1)
    simplot(sim.manual1,
            c("diagnosis.ps","hiv.testing"))
}

if (1==1){
    engine2 <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    param.manual2=get.means(SHIELD.FULL.PARAMETERS.PRIOR)
    sim.manual2 <- engine2$run(param.manual2)
    simplot(sim.manual2,
            c("diagnosis.ps","hiv.testing"))
    
}

simplot(sim.manual1,sim.manual2,
        c("diagnosis.ps","hiv.testing","sti.screening"))

q2=engine2$extract.quantity.values() #returns the input values to the model
q2$rate.sti.screening
apply(q2$rate.sti.screening[[1]],c("race","age"),mean)

q1=engine1$extract.quantity.values() #returns the input values to the model
# q1$rate.sti.screening
apply(q1$rate.sti.screening[[1]],c("year"),mean)

