
LOCATION = 'NV'


print(paste0("Assmeble for location = ", LOCATION))

simset.full = assemble.simulations.from.calibration('ehe', LOCATION,'final.ehe.state')

simset = simset.full$burn(keep=1000)

print(simplot(simset, c('new','diagnosed.prevalence')))
simplot(simset, c('new','incidence'))

simset$n.sim
simset$save()

# new = simset$get('new', keep.dimensions = character(), year=2030)
# range(new)
# mean(new>625)
