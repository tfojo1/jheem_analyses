#https://ryanwhite.hrsa.gov/about/budget

rw.budget = c(
    a = 680.75,
    b = 1364.88,
    c = 208.97,
    d = 77.94,
    adap = 900.31
)

non.adap.budget = as.numeric(sum(rw.budget) - 2 *rw.budget['adap'])
frac.cd.of.non.adap = (rw.budget['c'] + rw.budget['d']) / non.adap.budget





(208+77)/(208+77+680+1364-900)
