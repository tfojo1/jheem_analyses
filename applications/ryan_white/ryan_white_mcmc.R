

get.fitted.rw.simulation <- function(sim.transmuter,
                                     sim.index)
{
    params = generate.random.samples(RYAN.WHITE.PARAMETERS.PRIOR, 1)
    sim.transmuter$transmute(sim.index = sim.index,
                             parameters = params)
}