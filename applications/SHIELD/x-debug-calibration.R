 
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)

params.manual <- params.last
sim.manual0 <- engine$run(params.manual)


# params.manual["global.transmission.rate"] <- 3.7870952 * 1
# params.manual["hiv.testing.or"] <- params.manual["hiv.testing.or    "] *1.8
# params.manual["syphilis.screening.multiplier.1990"] <- params.manual["syphilis.screening.multiplier.1990"] *0
# params.manual["syphilis.screening.multiplier.2000"] <- params.manual["syphilis.screening.multiplier.2000"] *0
# params.manual["syphilis.screening.multiplier.2010"] <- params.manual["syphilis.screening.multiplier.2010"] *0
# params.manual["syphilis.screening.multiplier.2020"] <- params.manual["syphilis.screening.multiplier.2020"] *0
# # 
# params.manual["prp.symptomatic.primary.msm"] <- params.manual["prp.symptomatic.primary.msm"] *0
# params.manual["prp.symptomatic.primary.heterosexual_male"] <- params.manual["prp.symptomatic.primary.heterosexual_male"] *0
# params.manual["prp.symptomatic.primary.female"] <- params.manual["prp.symptomatic.primary.female"] *0
# params.manual["prp.symptomatic.secondary.msm"] <- params.manual["prp.symptomatic.secondary.msm"] *0
# params.manual["prp.symptomatic.secondary.heterosexual_male"] <- params.manual["prp.symptomatic.secondary.heterosexual_male"] *0
# params.manual["prp.symptomatic.secondary.female"] <- params.manual["prp.symptomatic.secondary.female"] *0


# sim.manual1 <- engine$run(params.manual)


simplot(
    # sim.first,
    sim.last,
    sim.manual0,
    # sim.manual1,
    # outcomes = c("population"),
    outcomes = c("incidence",
                 "diagnosis.total",  "diagnosis.ps","diagnosis.el.misclassified",
                 # "diagnosis.total1",  "diagnosis.ps1","diagnosis.el.misclassified1",
                 "diagnosis.ll.misclassified",
                 # "diagnosis.delayed.treatment","diagnosis.immediate.treatment",
                 # "diagnosis.tertiary","diagnosis.cns",
                 "diagnosis.congenital","hiv.testing"),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)

simplot(
    # sim.first,
    # sim.last,
    sim.manual0,
    sim.manual,
    # outcomes = c("diagnosis.total"),
    outcomes = c("incidence",
                 "diagnosis.total",  "diagnosis.total1",
                 "diagnosis.ps", "diagnosis.ps1",
                 "diagnosis.el.misclassified","diagnosis.el.misclassified1",
                 # "diagnosis.el.misclassified","diagnosis.ll.misclassified",
                 # "diagnosis.el.true","diagnosis.ll.true",
                 "diagnosis.congenital","hiv.testing"),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)

cbind(sim.manual$diagnosis.total,sim.manual$diagnosis.total1) #they are close but not the same
hist(unlist(sim.manual$diagnosis.total-sim.manual$diagnosis.total1))

simplot(
    # sim.first,
    sim.manual0,
    sim.manual1,
    split.by = "profile",# facet.by = "age",
    outcomes = c("incidence"),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)
#looking at local variables 
q=engine$extract.quantity.values() #returns the input values to the model

sapply(q$rate.infected.contacts.diagnosed.treated,sum)

q$sti.screening.by.stage
sum(unlist(q$rate.sti.screening))
sum(unlist(q$rate.testing.hiv))

q$rate.diagnosis.immediate.treatment
q$rate.diagnosis.delayed.treatment

q$prop.index.cases.reached.for.contact.tracing #contact tracing
sum(unlist(q$rate.infected.contacts.empirically.treated))

q$prp.received.prenatal.care #prenatal care
q$el.rel.secondary.transmissibility #EL infectiousness
q$prop.early.latent.to.secondary#relapse
q$prob.vertical.transmission.by.stage #vertical transmission

dimnames(sim.manual$immigration)


rowSums(sim.manual1$infected)
sapply(q$rate.sexual.transmission,mean)
