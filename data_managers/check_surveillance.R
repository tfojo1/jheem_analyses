
library(reshape2)
library(ggplot2)

msa='12580' #Baltimore

new.health.dep=c('2010'=922,
                 '2011'=711,
                 '2012'=676,
                 '2013'=615,
                 '2014'=583,
                 '2015'=539,
                 '2016'=523,
                 '2017'=432,
                 '2018'=445)

load('../code/cached_surveillance/msa.surveillance.estimated.Rdata')

msa.surveillance.estimated = msa.surveillance
new.msa.estimated = get.surveillance.data(msa.surveillance.estimated, msa, data.type='new')
prevalence.msa.estimated = get.surveillance.data(msa.surveillance.estimated, msa, data.type='prevalence')

load('../code/cached_surveillance/msa.surveillance.not.estimated.Rdata')

msa.surveillance.not.estimated = msa.surveillance
new.msa.not.estimated = get.surveillance.data(msa.surveillance.not.estimated, msa, data.type='new')
prevalence.msa.not.estimated = get.surveillance.data(msa.surveillance.not.estimated, msa, data.type='prevalence')

load('../code/cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')

msa.surveillance.estimated.corrected = msa.surveillance

load('../code/cached_surveillance/msa.surveillance.not.estimated.correct.to.county.Rdata')

msa.surveillance.not.estimated.corrected = msa.surveillance


load('../code/cached_surveillance/msa.by.county.Rdata')

new.by.county = msa.by.county$new.all[,msa]
prev.by.county = msa.by.county$prevalence.all[,msa]


df = rbind(data.frame(Year=as.integer(names(new.health.dep)),
                      Cases=new.health.dep,
                      Data_Type='New Diagnoses',
                      Source="Health Department"),
           data.frame(Year=attr(new.msa.estimated, 'year'),
                      Cases=new.msa.estimated,
                      Data_Type='New Diagnoses',
                      Source='CDC MSA Reports (Estimated)'),
           data.frame(Year=attr(new.msa.not.estimated, 'year'),
                      Cases=new.msa.not.estimated,
                      Data_Type='New Diagnoses',
                      Source='CDC MSA Reports (Not Estimated)'),
           data.frame(Year=as.integer(names(new.by.county)),
                      Cases=new.by.county,
                      Data_Type='New Diagnoses',
                      Source="Aggregated CDC County-Level Data"),

           data.frame(Year=attr(prevalence.msa.estimated, 'year'),
                      Cases=prevalence.msa.estimated,
                      Data_Type='Prevalence (Diagnosed)',
                      Source='CDC MSA Reports (Estimated)'),
           data.frame(Year=attr(prevalence.msa.not.estimated, 'year'),
                      Cases=prevalence.msa.not.estimated,
                      Data_Type='Prevalence (Diagnosed)',
                      Source='CDC MSA Reports (Not Estimated)'),
           data.frame(Year=as.integer(names(prev.by.county)),
                      Cases=prev.by.county,
                      Data_Type='Prevalence (Diagnosed)',
                      Source="Aggregated CDC County-Level Data")
)

print(ggplot(df, aes(Year, Cases, color=Source, shape=Source)) +
          geom_line() + geom_point() + facet_wrap(~Data_Type, scales='free') +
          ylim(0,NA))
asdf
load('../code/cached_surveillance/msa.surveillances.before.collapsing.Rdata')

data.type.labels = c(new='New Diagnoses (Heterosexual + Other merged)', prevalence='Prevalence (Diagnosed, Heterosexual + Other merged)')
for (data.type in c('new','prevalence'))
{
    by.risk.estimated.uncorrected = get.surveillance.data(msa.surveillance.estimated, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.uncorrected = get.surveillance.data(msa.surveillance.not.estimated, msa, data.type=data.type, risk=T)
    by.risk.estimated.corrected = get.surveillance.data(msa.surveillance.estimated.corrected, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.corrected = get.surveillance.data(msa.surveillance.not.estimated.corrected, msa, data.type=data.type, risk=T)

    df.by.risk.estimated.uncorrected = melt(by.risk.estimated.uncorrected, value.name='Cases')
    df.by.risk.estimated.uncorrected$Source = 'CDC MSA Reports (Estimated)'
    df.by.risk.not.estimated.uncorrected = melt(by.risk.not.estimated.uncorrected, value.name='Cases')
    df.by.risk.not.estimated.uncorrected$Source = 'CDC MSA Reports (Not Estimated)'

    df.by.risk.estimated.corrected = melt(by.risk.estimated.corrected, value.name='Cases')
    df.by.risk.estimated.corrected$Source = 'CDC MSA Reports (Estimated, Corrected to County Sums)'
    df.by.risk.not.estimated.corrected = melt(by.risk.not.estimated.corrected, value.name='Cases')
    df.by.risk.not.estimated.corrected$Source = 'CDC MSA Reports (Not Estimated, Corrected to County Sums)'

    df.by.risk = rbind(df.by.risk.estimated.uncorrected, df.by.risk.not.estimated.uncorrected,
                        df.by.risk.estimated.corrected, df.by.risk.not.estimated.corrected)
    risk.labels = c(msm="MSM", msm_idu='MSM+IDU', heterosexual='Heterosexual+Other', idu='IDU')
    df.by.risk$Risk_Factor = risk.labels[as.character(df.by.risk$risk)]
    df.by.risk$Year = df.by.risk$year

    print(ggplot(df.by.risk, aes(Year, Cases, color=Source, shape=Source)) +
              geom_line() + geom_point() + facet_wrap(~Risk_Factor, scales='free') +
              ylim(0,NA) + ggtitle(data.type.labels[data.type]) +
              theme(legend.position = 'bottom'))
}

data.type.labels = c(new='New Diagnoses', prevalence='Prevalence (Diagnosed)')
for (data.type in c('new','prevalence'))
{
    by.risk.estimated.uncorrected = get.surveillance.data(orig.estimated, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.uncorrected = get.surveillance.data(orig.not.estimated, msa, data.type=data.type, risk=T)
    by.risk.estimated.corrected = get.surveillance.data(orig.estimated.corrected.to.county, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.corrected = get.surveillance.data(orig.not.estimated.corrected.to.county, msa, data.type=data.type, risk=T)

    df.by.risk.estimated.uncorrected = melt(by.risk.estimated.uncorrected, value.name='Cases')
    df.by.risk.estimated.uncorrected$Source = 'CDC MSA Reports (Estimated)'
    df.by.risk.not.estimated.uncorrected = melt(by.risk.not.estimated.uncorrected, value.name='Cases')
    df.by.risk.not.estimated.uncorrected$Source = 'CDC MSA Reports (Not Estimated)'

    df.by.risk.estimated.corrected = melt(by.risk.estimated.corrected, value.name='Cases')
    df.by.risk.estimated.corrected$Source = 'CDC MSA Reports (Estimated, Corrected to County Sums)'
    df.by.risk.not.estimated.corrected = melt(by.risk.not.estimated.corrected, value.name='Cases')
    df.by.risk.not.estimated.corrected$Source = 'CDC MSA Reports (Not Estimated, Corrected to County Sums)'

    df.by.risk = rbind(df.by.risk.estimated.uncorrected, df.by.risk.not.estimated.uncorrected,
                       df.by.risk.estimated.corrected, df.by.risk.not.estimated.corrected)
    risk.labels = c(msm="MSM", msm_idu='MSM+IDU', heterosexual='Heterosexual', other='Other', idu='IDU')
    df.by.risk$Risk_Factor = risk.labels[as.character(df.by.risk$risk)]
    df.by.risk$Year = df.by.risk$year

    print(ggplot(df.by.risk, aes(Year, Cases, color=Source, shape=Source)) +
              geom_line() + geom_point() + facet_wrap(~Risk_Factor, scales='free') +
              ylim(0,NA) + ggtitle(data.type.labels[data.type]) +
              theme(legend.position = 'bottom'))
}

data.type.labels = c(new='New Diagnoses', prevalence='Prevalence (Diagnosed)')
for (data.type in c('new','prevalence'))
{
    by.risk.estimated.with.other = get.surveillance.data(msa.surveillance.estimated.corrected, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.with.other = get.surveillance.data(msa.surveillance.not.estimated.corrected, msa, data.type=data.type, risk=T)
    by.risk.estimated.sans.other = get.surveillance.data(orig.estimated.corrected.to.county, msa, data.type=data.type, risk=T)
    by.risk.not.estimated.sans.other = get.surveillance.data(orig.not.estimated.corrected.to.county, msa, data.type=data.type, risk=T)

    df.by.risk.estimated.with.other = melt(by.risk.estimated.with.other, value.name='Cases')
    df.by.risk.estimated.with.other$Source = 'CDC MSA Reports (Estimated, INCLUDES Other)'
    df.by.risk.not.estimated.with.other = melt(by.risk.not.estimated.with.other, value.name='Cases')
    df.by.risk.not.estimated.with.other$Source = 'CDC MSA Reports (Not Estimate, INCLUDES Other)'

    df.by.risk.estimated.sans.other = melt(by.risk.estimated.sans.other, value.name='Cases')
    df.by.risk.estimated.sans.other$Source = 'CDC MSA Reports (Estimated, WITHOUT Other)'
    df.by.risk.not.estimated.sans.other = melt(by.risk.not.estimated.sans.other, value.name='Cases')
    df.by.risk.not.estimated.sans.other$Source = 'CDC MSA Reports (Not Estimated, WITHOUT Other)'

    df.by.risk = rbind(df.by.risk.estimated.with.other, df.by.risk.not.estimated.with.other,
                       df.by.risk.estimated.sans.other, df.by.risk.not.estimated.sans.other)
    risk.labels = c(msm="MSM", msm_idu='MSM+IDU', heterosexual='Heterosexual +/- Other', idu='IDU')
    df.by.risk$Risk_Factor = risk.labels[as.character(df.by.risk$risk)]
    df.by.risk$Year = df.by.risk$year

    print(ggplot(df.by.risk, aes(Year, Cases, color=Source, shape=Source)) +
              geom_line() + geom_point() + facet_wrap(~Risk_Factor, scales='free') +
              ylim(0,NA) + ggtitle(data.type.labels[data.type]) +
              theme(legend.position = 'bottom'))
}
