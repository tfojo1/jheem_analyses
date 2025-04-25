

new.dx = c(
    '2024' = 24392,
    '2023' = 38539,
    '2022' = 37601,
    '2021' = 35671,
    '2020' = 30317,
    '2019' = 36350,
    '2018' = 37132,
    '2017' = 38048,
    '2016' = 39163,
    '2015' = 39543,
    '2014' = 39653,
    '2013' = 38931,
    '2012' = 40183,
    '2011' = 40945,
    '2010' = 42775
)

total.prev = c(
    '2022' = 1238000,
    '2021' = 1215900,
    '2020' = 1199700,
    '2019' = 1188300,
    '2018' = 1167500,
    '2017' = 1145800,
    '2016' = 1124000
)

dx.prev = c(
    '2022' = 1092023,
    '2021' = 1068119,
    '2020' = 1049709,
    '2019' = 1036801,
    '2018' = 1014481,
    '2017' = 991448,
    '2016' = 967557,
    '2015' = 942988,
    '2014' = 917870,
    '2013' = 893271,
    '2012' = 869358,
    '2011' = 844203,
    '2010' = 818561
)

awareness = c(
    '2022' = .872,
    '2021' = .868,
    '2020' = .865,
    '2019' = .863,
    '2018' = .859,
    '2017' = .856,
    '2016' = .851,
    '2015' = .846,
    '2014' = .841,
    '2013' = .835,
    '2012' = .830,
    '2011' = .825,
    '2010' = .818
)

suppression = c(
    '2022' = .651,
    '2021' = .659,
    '2020' = .646,
    '2019' = .655,
    '2018' = .647,
    '2017' = .631
)

years = intersect(
    intersect(names(new.dx), names(dx.prev)),
    intersect(names(awareness), names(suppression))
)

new.dx[years] / ( dx.prev[years] * (1-suppression[years]) + dx.prev[years] * (1/awareness[years] - 1) )



calc.trate.est <- function(loc)
{
    new.dx = rowMeans(SURVEILLANCE.MANAGER$pull('diagnoses', location=loc), na.rm=T)
    dx.prev = rowMeans(SURVEILLANCE.MANAGER$pull('diagnosed.prevalence', location=loc), na.rm=T)
    
    awareness = rowMeans(SURVEILLANCE.MANAGER$pull('awareness', location=loc), na.rm=T)
    suppression = rowMeans(SURVEILLANCE.MANAGER$pull('suppression', location=loc), na.rm=T)
    
    years = intersect(
        intersect(names(new.dx)[!is.na(new.dx)], names(dx.prev))[!is.na(dx.prev)],
        intersect(names(awareness)[!is.na(awareness)], names(suppression)[!is.na(suppression)])
    )
    
    new.dx[years] / ( dx.prev[years] * (1-suppression[years]) + dx.prev[years] * (1/awareness[years] - 1) )
}

calc.trate.est('TX')
calc.trate.est('CA')

loc='TX';z=calc.trate.est(loc); qplot(names(z), z, group=rep('all', length(z))) + geom_line() + ylim(0,NA) + geom_line() + ggtitle(get.location.name(loc))
loc='NY';z=calc.trate.est(loc); qplot(names(z), z, group=rep('all', length(z))) + geom_line() + ylim(0,NA) + geom_line() + ggtitle(get.location.name(loc))
loc='TX';z=calc.trate.est(loc); qplot(names(z), z, group=rep('all', length(z))) + geom_line() + ylim(0,NA) + geom_line() + ggtitle(get.location.name(loc))
loc='TX';z=calc.trate.est(loc); qplot(names(z), z, group=rep('all', length(z))) + geom_line() + ylim(0,NA) + geom_line() + ggtitle(get.location.name(loc))
