

# assuming you already have a fit
# and a specification.metadata


dim.names = specification.metadata$dim.names[c('age','race','sex')]
iterated.values = as.data.frame(get.every.combination(dim.names))

year0.data = cbind(iterated.values, year=0)
year1.data = cbind(iterated.values, year=1)

intercept = predict(fit, year0.data, type = 'link')
slope = predict(fit, year1.data, type='link') - intercept

dim(intercept) = dim(slope) = sapply(dim.names, length)
dimnames(intercept) = dimnames(slope) = dim.names
