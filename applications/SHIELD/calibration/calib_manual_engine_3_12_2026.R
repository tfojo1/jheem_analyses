if(1==2) {
engineMANUAL = create.jheem.engine( "shield",  "C.16740", end.year = 2030)
engineCALIB = create.jheem.engine( "shield",  "C.16740", end.year = 2030)
}
paramsCALIB = lastCT$params
paramsMANUAL = paramsCALIB
# paramsMANUAL["other.immigration.rate.multiplier.2"] =
#     1.5 * paramsMANUAL["other.immigration.rate.multiplier.2"]

paramsMANUAL["age14.other.aging.rate.multiplier.2"] =
    0.5 * paramsMANUAL["age14.other.aging.rate.multiplier.2"]


simMANUAL = engineMANUAL$run(paramsMANUAL)
# simCALIB = engineCALIB$run(paramsCALIB)

simplot(simCALIB, simMANUAL, "population")
simplot(simCALIB, simMANUAL, "population", split.by="race", facet.by="age")
simplot(simCALIB, simMANUAL, c("immigration", "emigration"), split.by="race", facet.by="age")
simplot(simCALIB, simMANUAL, "fertility.rate", split.by="race", facet.by="age")
simplot(simCALIB, simMANUAL, "births.from", split.by="race", facet.by="age")
