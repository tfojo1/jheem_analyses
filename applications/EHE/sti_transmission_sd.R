
library(ggplot2)
source('../jheem_analyses/source_code.R')

data = SURVEILLANCE.MANAGER$data$gonorrhea$estimate$cdc.aggregated.county$cdc.sti$year__location
ratios = data[-1,] / data[-nrow(data),]
rowMeans(ratios, na.rm=T)

# On the number of tests
df = reshape2::melt(data)
df$year = as.numeric(df$year)

locations.to.plot = c('C.12580','C.35620','C.12060','C.16980')
to.plot = df[sapply(df$location, function(loc){any(loc==locations.to.plot)}),]

ggplot(to.plot, aes(x=year, y=log(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()
ggplot(to.plot, aes(x=year, y=(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()


df = df[df$year<2020,]
df$year.sp1 = pmax(df$year-2012, 0)

fit = lm(log(value) ~ year * location + year.sp1 * location, data=df)
unified.sigma = summary(fit)$sigma; unified.sigma

location.sigmas = sapply(unique(df$location), function(loc){
    sub.df = df[df$location==loc,]
    fit = lm(log(value) ~ year +year.sp1, data=sub.df)
    summary(fit)$sigma
})
simple.mean.sigma = mean(location.sigmas, na.rm=T); simple.mean.sigma
exp(simple.mean.sigma * qnorm(.975))

location.weights = sapply(unique(df$location), function(loc){
    mean(df$value[df$location==loc], na.rm=T)
})
weighted.mean.sigma = sum(location.sigmas * location.weights, na.rm=T) / sum(location.weights, na.rm=T)
exp(weighted.mean.sigma * qnorm(.975))

# On the year-to-year ratio

df.ratio = reshape2::melt(ratios)
df.ratio$year = as.numeric(df.ratio$year)
df.ratio$year.sp1 = pmax(df.ratio$year-2012, 0)

to.plot = df.ratio[sapply(df.ratio$location, function(loc){any(loc==locations.to.plot)}),]

ggplot(to.plot, aes(x=year, y=log(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()
ggplot(to.plot, aes(x=year, y=(value))) + geom_line() + facet_wrap(~location, scales='free_y') + ylim(0,NA) + geom_smooth()


df.ratio = df.ratio[df.ratio$year<2020,]

fit = lm(log(value) ~ year * location + year.sp1 * location, data=df.ratio)
unified.sigma.ratio = summary(fit)$sigma; unified.sigma.ratio

location.sigmas.ratio = sapply(unique(df$location), function(loc){
  sub.df = df.ratio[df.ratio$location==loc,]
  fit = lm(log(value) ~ year + year.sp1, data=sub.df)
  summary(fit)$sigma
})
simple.mean.sigma.ratio = mean(location.sigmas.ratio, na.rm=T)
exp(simple.mean.sigma.ratio * qnorm(.975))

weighted.mean.sigma.ratio = sum(location.sigmas.ratio * location.weights, na.rm=T) / sum(location.weights, na.rm=T)
exp(weighted.mean.sigma.ratio * qnorm(.975))


rowMeans(ratios, na.rm=T)

dlnorm(1, 0, weighted.mean.sigma.ratio) / dlnorm(rowMeans(ratios, na.rm=T), 0, weighted.mean.sigma.ratio)
dnorm(0, 0, weighted.mean.sigma.ratio) / dnorm(log(rowMeans(ratios, na.rm=T)), 0, weighted.mean.sigma.ratio)

dlnorm(1, 0, weighted.mean.sigma.ratio/2) / dlnorm(rowMeans(ratios, na.rm=T), 0, weighted.mean.sigma.ratio/2)
dnorm(0, 0, weighted.mean.sigma.ratio/2) / dnorm(log(rowMeans(ratios, na.rm=T)), 0, weighted.mean.sigma.ratio/2)

dlnorm(1, 0, weighted.mean.sigma.ratio/sqrt(2)) / dlnorm(rowMeans(ratios, na.rm=T), 0, weighted.mean.sigma.ratio/sqrt(2))
dnorm(0, 0, weighted.mean.sigma.ratio/sqrt(2)) / dnorm(log(rowMeans(ratios, na.rm=T)), 0, weighted.mean.sigma.ratio/sqrt(2))


dlnorm(1, 0, weighted.mean.sigma) / dlnorm(rowMeans(ratios, na.rm=T), 0, weighted.mean.sigma)
dnorm(0, 0, weighted.mean.sigma) / dnorm(log(rowMeans(ratios, na.rm=T)), 0, weighted.mean.sigma)
