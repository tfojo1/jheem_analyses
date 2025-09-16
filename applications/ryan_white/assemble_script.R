loc = 'MS'

full.simset = assemble.simulations.from.calibration('ehe',loc,'final.ehe.state')

simplot(full.simset, 'new')

new.2030 = full.simset$get('new', year=2030)
mean(new.2030<250)

# simset = full.simset[new.2030<300]
# simset$n.sim

simset = full.simset$burn(1000)

simplot(simset, 'new')



simset$save()

