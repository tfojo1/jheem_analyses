

all.values1 = numeric()
all.values2 = numeric()


# For total
data1 = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.aggregated.proportion$cdc
data2 = SURVEILLANCE.MANAGER$data$suppression$estimate$lhd$lhd

years.in.both.total = intersect(dimnames(data1$year__location)$year,
                                dimnames(data2$year__location)$year)
locations.in.both.total = intersect(dimnames(data1$year__location)$location,
                                    dimnames(data2$year__location)$location)

values1 = data1$year__location[years.in.both.total, locations.in.both.total]
values2 = data2$year__location[years.in.both.total, locations.in.both.total]

all.values1 = c(all.values1, values1)
all.values2 = c(all.values2, values2)

# For other stratificiations
#@melissa

# Calculate it

errors = all.values1 - all.values2

sqrt(sum(errors^2, na.rm=T)/sum(!is.na(errors)))
