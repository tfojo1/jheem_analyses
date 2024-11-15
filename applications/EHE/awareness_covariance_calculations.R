

msa.awareness1 = SURVEILLANCE.MANAGER$data$awareness$estimate$cdc.aggregated.proportion$cdc$year__location
#msa.awareness2 = SURVEILLANCE.MANAGER$data$awareness$estimate$lhd$lhd$year__location


msa.awareness = reshape2::melt(msa.awareness1)

awareness.data = SURVEILLANCE.MANAGER$data$awareness$estimate$cdc.hiv$cdc$year__location
n.msa = SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location
n.state = SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location

county.state.delta.list = lapply(1:dim(msa.awareness)[1], function(i){
    
    msa = msa.awareness$location[i]
    year = as.character(msa.awareness$year[i])
    msa.value = msa.awareness$value[i]
    
    counties = locations::get.contained.locations(msa, 'COUNTY')
    if (length(counties)==1)
    {
        print(paste0("aborting ", msa, " - only one county"))
        return (NULL)
    }
    
    counties = intersect(counties, dimnames(awareness.data)$location)
    
    states = get.containing.locations(counties, 'STATE')
    
    county.values = awareness.data[year, counties]
    
    state.values = awareness.data[year, states]
    
    msa.n = n.msa[year, msa]
    state.n = n.state[year, states]
    
    out.of.msa.values = (state.values*state.n - msa.value*msa.n) / (state.n-msa.n)
    
    array(c(county.values-msa.value, 
            out.of.msa.values-msa.value),
            #state.values-msa.value),
           dim=c(length(counties), 2),
           dimnames=list(county=paste0(counties, "_", year), type=c('county.delta','out.of.msa.delta')))
})


county.state.deltas = NULL
for (to.add in county.state.delta.list)
{
    if (!is.null(to.add))
        county.state.deltas = rbind(county.state.deltas, to.add)
}

cor(county.state.deltas)


out.of.msa.deltas = unlist(lapply(1:dim(msa.awareness)[1], function(i){
    
    msa = msa.awareness$location[i]
    year = as.character(msa.awareness$year[i])
    msa.value = msa.awareness$value[i]
    
    states = get.containing.locations(msa, 'STATE')
    
    state.values = awareness.data[year, states]
    
    msa.n = n.msa[year, msa]
    state.n = n.state[year, states]
    
    out.of.msa.values = (state.values*state.n - msa.value*msa.n) / (state.n-msa.n)
    
    out.of.msa.values-msa.value
}))

mean(out.of.msa.deltas)
sd(out.of.msa.deltas)


mean(county.state.deltas[,1])
sd(county.state.deltas[,1])



county.deltas = unlist(lapply(1:dim(msa.awareness)[1], function(i){
    
    msa = msa.awareness$location[i]
    year = as.character(msa.awareness$year[i])
    msa.value = msa.awareness$value[i]
    
    counties = locations::get.contained.locations(msa, 'COUNTY')
    
    
    county.values = awareness.data[year, counties]
    
    county.values-msa.value
}))

mean(county.deltas)
sd(county.deltas)


county.msa.summary.list = lapply(1:dim(msa.awareness)[1], function(i){
    
    msa = msa.awareness$location[i]
    year = as.character(msa.awareness$year[i])
    msa.value = msa.awareness$value[i]
    
    counties = locations::get.contained.locations(msa, 'COUNTY')
    
    
    county.values = awareness.data[year, counties]
    
    data.frame(
        county = counties,
        msa = msa,
        county.p = county.values,
        msa.p = msa.value
    )
})


county.msa.summary = NULL
for (to.add in county.msa.summary.list)
{
    if (!is.null(to.add))
        county.msa.summary = rbind(county.msa.summary, to.add)
}

ggplot(county.msa.summary, aes(msa.p, county.p, color=msa)) + geom_point() + geom_abline(slope=1, intercept=0)
msa

awareness.bias.estimates = list(
    in.mean = mean(county.deltas),
    out.mean = mean(out.of.msa.deltas),
    in.sd = sd(county.deltas),
    out.sd = sd(out.of.msa.deltas),
    n.in = length(county.deltas),
    n.out = length(out.of.msa.deltas)
)
