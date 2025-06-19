 
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)

params.manual <- params.last
# params.manual["global.transmission.rate"] <- 3.7870952 * 1
# params.manual["hiv.testing.or"] <- params.manual["hiv.testing.or    "] *1.8
params.manual["syphilis.screening.multiplier.1990"] <- params.manual["syphilis.screening.multiplier.1990"] *1.8
params.manual["syphilis.screening.multiplier.2000"] <- params.manual["syphilis.screening.multiplier.2000"] *1.8
params.manual["syphilis.screening.multiplier.2010"] <- params.manual["syphilis.screening.multiplier.2010"] *1.8
params.manual["syphilis.screening.multiplier.2020"] <- params.manual["syphilis.screening.multiplier.2020"] *1.5


sim.manual <- engine$run(params.manual)
# sim.manual0<-sim.manual
simplot(
    # sim.first,
    # sim.last,
    # sim.manual0,
    sim.manual,
    # outcomes = c("diagnosis.total"),
    outcomes = c("incidence",
                 "diagnosis.total",  "diagnosis.total1",
                 "diagnosis.ps", "diagnosis.ps1",
                 # "diagnosis.el.misclassified","diagnosis.ll.misclassified",
                 # "diagnosis.el.true","diagnosis.ll.true",
                 "diagnosis.congenital","hiv.testing"),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)

cbind(sim.manual$diagnosis.total,sim.manual$diagnosis.total1) #they are close but not the same
 
simplot(
    # sim.first,
    sim.last,
    sim.manual,
    split.by = "race",# facet.by = "age",
    outcomes = c("incidence"),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)
#looking at local variables 
q=engine$extract.quantity.values() #returns the input values to the model

q$sti.screening.by.stage
stiscreening=q$rate.sti.screening
dim(stiscreening)
q$prop.index.cases.reached.for.contact.tracing #contact tracing
q$prp.received.prenatal.care #prenatal care
q$el.rel.secondary.transmissibility #EL infectiousness
q$prop.early.latent.to.secondary#relapse
q$prob.vertical.transmission.by.stage #vertical transmission

dimnames(sim.manual$immigration)