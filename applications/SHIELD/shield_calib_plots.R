
#reviewing calibration plots
# Demographic outcome:

simplot(
    sim.first,
    sim.last,
    # split.by = "sex",
    # split.by = "age",
    # split.by = "race",
    # split.by = "race", facet.by = "sex",
    split.by = "race", facet.by = "age",
    outcomes = c("population"),
    style.manager = source.style.manager
)
simplot(
    # sim.first,
    sim.last,
    outcomes = c("deaths"),
    style.manager = source.style.manager
)
simplot(
    # sim.first,
    sim.last,
    split.by = "race", facet.by = "age",
    outcomes = c("fertility.rate"),
    style.manager = source.style.manager
)
simplot(
    # sim.first,
    sim.last,
    split.by = "race",
    outcomes = c("immigration","emigration"),
    style.manager = source.style.manager
)

