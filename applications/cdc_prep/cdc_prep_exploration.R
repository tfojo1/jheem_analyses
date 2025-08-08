
#-- Settings --#
loc = 'NY'
region = 'Northeast'

#-- DATA --#
n.test.non.healthcare = c(
    LA = 17882,
    GA = 31675,
    CA = 18278+58+5547,
    MO = 4842,
    WI = 4619,
    MS = 2167,
    SC = 8423,
    TN = 4868,
    AL = 59886,
    TX = 6449 + 27272,
    KY = 2628,
    FL = 92176,
    OK = 4118,
    MD = 319+2441,
    NY = 2088 + 8552
)

p.eligible = c(
    all = 0.565,
    South = 0.554,
    West = 0.523,
    Midwest = 0.659,
    Northeast = 0.658
)

p.referred.of.eligible = c(
    South = 0.543,
    West = 0.321,
    Midwest = 0.465,
    Northeast = 0.437
)

p.linkage.assistance.of.referred = c(
    South = 0.87,
    West = 0.79,
    Midwest = 0.598,
    Northeast = 0.807
)

p.started.of.referred = mean(c(0.14,0.29))

#-- Pull Simset --#

simset = retrieve.simulation.set('ehe',loc,'final.ehe.state',1000)
simplot(simset, 'prep.uptake')

simset.sub = simset$thin(keep = 50)

prep.uptake = simset$get("prep.uptake", year=2020:2030, keep.dimensions = 'year')
rowMeans(prep.uptake)

SURVEILLANCE.MANAGER$data$prep$estimate$aidsvu$aidsvu$year__location[,simset$location]
SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location[,simset$location]

#-- Calculations --#

n.referred = n.test.non.healthcare[loc] *
    p.eligible[region] * p.referred.of.eligible[region]

n.started = n.referred * p.started.of.referred

frac.prep.from.cdc = n.started / rowMeans(prep.uptake)

test.int.effect = create.intervention.effect(
    'oral.prep.uptake',
    start.time = 2025.75,
    times = c(2026,2030),
    effect.values = 1-frac.prep.from.cdc[c('2025','2030')],
    apply.effects.as = 'multiplier',
    scale = 'proportion',
    allow.values.less.than.otherwise = T,
    allow.values.greater.than.otherwise = F
)

test.int = create.intervention(
    WHOLE.POPULATION,
    test.int.effect,
    code = paste0('cdc-prep-end.', loc)
)

simset.int = test.int$run(simset.sub, end.year = 2030, start.year = 2025)
simset.noint = get.null.intervention()$run(simset.sub, end.year = 2030, start.year = 2025)

inc.baseline = simset.noint$get('incidence', year=2025:2030, keep.dimensions = character())
inc.end = simset.int$get('incidence', year=2025:2030, keep.dimensions = character())

rel.increase = (inc.end-inc.baseline) / inc.baseline
print(frac.prep.from.cdc)
print(mean(rel.increase))

results[[loc]] = list(
    frac.prep.from.cdc = frac.prep.from.cdc,
    rel.increase = rel.increase,
    simset.int = simset.int,
    simset.sub = simset.sub
)

summ = sapply(results, function(sub.results){
    mean(sub.results$rel.increase)
})

print(sort(summ, decreasing = T))
