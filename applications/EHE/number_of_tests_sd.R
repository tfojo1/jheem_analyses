
library(ggplot2)
source('source_code.R')

data = SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location


df = reshape2::melt(data)
df$year = as.numeric(df$year)

locations.to.plot = c('CA','MD', 'C.12580','C.35620','FL','TX')
to.plot = df[sapply(df$location, function(loc){any(loc==locations.to.plot)}),]

ggplot(to.plot, aes(x=year, y=log(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()
ggplot(to.plot, aes(x=year, y=(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()


df = df[df$year<2020,]

fit = lm(log(value) ~ year * location, data=df)
unified.sigma = summary(fit)$sigma

location.sigmas = sapply(unique(df$location), function(loc){
    sub.df = df[df$location==loc,]
    fit = lm(log(value) ~ year, data=sub.df)
    summary(fit)$sigma
})
simple.mean.sigma = mean(location.sigmas, na.rm=T)
exp(simple.mean.sigma * qnorm(.975))

location.weights = sapply(unique(df$location), function(loc){
    mean(df$value[df$location==loc], na.rm=T)
})
weighted.mean.sigma = sum(location.sigmas * location.weights, na.rm=T) / sum(location.weights, na.rm=T)
exp(weighted.mean.sigma * qnorm(.975))

