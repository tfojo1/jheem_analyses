library(jheem)
library(ggplot2)


##-----------------------------##
##-- PARAMETER-BASED GETTERS --##
##-----------------------------##


# returns a matrix indexed <age.from, age.to>
get.age.mixing.proportions <- function(age.delta.intercept.mean,
                                       age.delta.slope.mean,
                                       age.delta.intercept.sd,
                                       age.delta.slope.sd,
                                       age.cutoffs,
                                       age.labels,
                                       single.year.age.counts,
                                       sd.multiplier)
{
    if (any(is.infinite(age.cutoffs)))
        stop("age.cutoffs cannot be infinite")
    
    n.age.brackets = length(age.cutoffs)-1
    age.lowers = age.cutoffs[-length(age.cutoffs)]
    age.uppers = age.cutoffs[-1]
    
    last.age.in.bracket = age.uppers
    last.age.in.bracket[-n.age.brackets] = last.age.in.bracket[-n.age.brackets]-1
    
    min.age = age.cutoffs[1]
    max.age = age.cutoffs[length(age.cutoffs)]
    
    rv = sapply(1:n.age.brackets, function(age.to){
        
        ages.in.bracket = age.lowers[age.to]:last.age.in.bracket[age.to]
        age.proportions.within.bracket = single.year.age.counts[as.character(ages.in.bracket)] /
            sum(single.year.age.counts[as.character(ages.in.bracket)])
        ages.in.bracket = ages.in.bracket + 0.5 #make the midpoint of the year the age for each
        
        age.means = ages.in.bracket + age.delta.intercept.mean + age.delta.slope.mean*ages.in.bracket
        age.sds = (age.delta.intercept.sd + age.delta.slope.sd*ages.in.bracket) * sd.multiplier
        
        sapply(1:n.age.brackets, function(age.from){
            sum(age.proportions.within.bracket *
                    (pnorm(age.uppers[age.from], mean = age.means, sd = age.sds) -
                         pnorm(age.lowers[age.from], mean = age.means, sd = age.sds)) /
                    (pnorm(max.age, mean = age.means, sd = age.sds) -
                         pnorm(min.age, mean = age.means, sd = age.sds))
            )
        })
    })
    
    dim.names = list(age.from = age.labels, age.to = age.labels)
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

#Assumes that
# (partner.age - reference.age) ~ Normal(beta.0 + beta.1 * reference.age, gamma.0 + gamma.1 * reference.age)
get.age.mixing.proportions.from.parameters <- function(age.delta.intercept.mean,
                                                       age.delta.slope.mean,
                                                       age.delta.intercept.sd,
                                                       age.delta.slope.sd,
                                                       ages,
                                                       age.counts,
                                                       nsim.per=4000,
                                                       cutoffs=c(13,25,35,45,55,Inf))
{
    model = c(mean.intercept = age.delta.intercept.mean,
               mean.slope = age.delta.slope.mean,
               sd.intercept = age.delta.intercept.sd,
               sd.slope = age.delta.slope.sd)

    simulate.from.fitted.age.model(model,
                                   ages=ages,
                                   age.counts=age.counts,
                                   nsim.per=nsim.per,
                                   cutoffs=cutoffs)
}

get.bho.race.mixing.proportions.from.parameters <- function(oe.black.black,
                                                            oe.hispanic.hispanic,
                                                            oe.other.other,
                                                            race.proportions)
{
    rv = array(race.proportions, dim=c(race.from=length(race.proportions), race.to=length(race.proportions)),
               dimnames=list(race.from=names(race.proportions), race.to=names(race.proportions)))

    rv['black','black'] = oe.black.black * rv['black','black']
    rv['hispanic','hispanic'] = oe.hispanic.hispanic * rv['hispanic','hispanic']
    rv['other','other'] = oe.other.other * rv['other','other']

    rv = rv / rep(colSums(rv), each=length(race.proportions))
}

ORIG.get.bho.race.mixing.proportions.from.parameters <- function(log.rr.black.black,
                                                            log.rr.black.hispanic,
                                                            log.rr.black.other,
                                                            log.rr.hispanic.hispanic,
                                                            log.rr.hispanic.other,
                                                            race.order=c('black','hispanic','other'))
{
    raw.black.black = exp(log.rr.black.black)
    raw.black.hispanic = exp(log.rr.black.hispanic)
    raw.black.other = exp(log.rr.black.other)
    raw.hispanic.hispanic = exp(log.rr.hispanic.hispanic)
    raw.hispanic.other = exp(log.rr.hispanic.other)
    raw.other.other = 1

    denominator.black = raw.black.black + raw.black.hispanic + raw.black.other
    denominator.hispanic = raw.black.hispanic + raw.hispanic.hispanic + raw.hispanic.other
    denominator.other = raw.black.other + raw.hispanic.other + raw.other.other

    rv = array(NA, dim=c(race.from=length(race.order), race.to=length(race.order)),
               dimnames=list(race.from=race.order, race.to=race.order))

    rv['black','black'] = raw.black.black / denominator.black
    rv['hispanic','black'] = raw.black.hispanic / denominator.black
    rv['other','black'] = raw.black.other / denominator.black

    rv['black','hispanic'] = raw.black.hispanic / denominator.hispanic
    rv['hispanic','hispanic'] = raw.hispanic.hispanic / denominator.hispanic
    rv['other','hispanic'] = raw.hispanic.other / denominator.hispanic

    rv['black','other'] = raw.black.other / denominator.other
    rv['hispanic','other'] = raw.hispanic.other / denominator.other
    rv['other','other'] = raw.other.other / denominator.other

    rv
}

get.parameters.from.bho.mixing.proportions <- function(proportions)
{

}

##-------------------------##
##-- OTHER MAJOR HELPERS --##
##-------------------------##

#counts is a matrix indexed to-from
#Assumes that the columns are the selected denominators,
# and the rows represent the distribution of partners over those columns
calculate.oe.ratios <- function(pairing.counts)
{
    marginals = rowSums(pairing.counts) / sum(pairing.counts)
    col.counts = colSums(pairing.counts)
    expected = outer(marginals, col.counts)
    pairing.counts / expected
}

calculate.column.proportions <- function(mat)
{
    mat / rep(colSums(mat), each=dim(mat)[1])
}

get.pairing.proportions <- function(oe.ratios, marginal.counts)
{
    if (dim(oe.ratios)[1] != dim(oe.ratios)[2])
        stop("The 'oe.ratios' argument must be a square matrix")
    if (dim(oe.ratios)[1] != length(marginal.counts))
        stop("The length of 'marginal.counts' must be the same as the dimension of 'oe.ratios")

    marginals = marginal.counts / sum(marginal.counts)
    rv = oe.ratios * marginals
    rv = rv / rep(colSums(rv), each=length(marginal.counts))
    rv
}

scatter.to.matrix <- function(from.points, to.points, cutoffs)
{
    ages = make.age.strata(cutoffs)
    rv = array(0, dim=c(age.from=length(ages$labels), age.to=length(ages$labels)),
               dimnames=list(age.from=ages$labels, age.to=ages$labels))

    for (i in 1:length(from.points))
    {
        from.mask = from.points[i] >= ages$lowers & from.points[i] < ages$uppers
        to.mask = to.points[i] >= ages$lowers & to.points[i] < ages$uppers

        rv[from.mask,to.mask] = rv[from.mask,to.mask] + 1
    }

    rv
}

matrix.to.scatter <- function(mat,
                              age.cutoffs)
{
    rv = list(age.from=numeric(), age.to=numeric())
    for (i.from in 1:dim(mat)[1])
    {
        for (j.to in 1:dim(mat)[2])
        {
            n = mat[i.from,j.to]
            rv$age.from = c(rv$age.from, runif(n, age.cutoffs[i.from], age.cutoffs[i.from+1]))
#            rv$age.from = c(rv$age.from, rep((age.cutoffs[i.from]+age.cutoffs[i.from+1])/2, n))
            rv$age.to = c(rv$age.to, runif(n, age.cutoffs[j.to], age.cutoffs[j.to+1]))
#            rv$age.to = c(rv$age.from, rep((age.cutoffs[j.to]+age.cutoffs[j.to+1])/2, n))
        }
    }

    rv
}

##---------------------##
##-- SETTING UP AGES --##
##---------------------##

#fits a model where age.of.partner-age.of.reference ~ N(beta*age.of.reference, sd=gamma*age.of.reference)
fit.age.model <- function(age.of.reference, age.of.partner)
{
    diffs = age.of.partner - age.of.reference
    fit.for.mean = lm(diffs~age.of.reference)

    n.quantiles = 8
    cutoffs = quantile(age.of.reference, probs = seq(from=0,to=1,length=n.quantiles+1))
    midpoints = cutoffs[-1][-n.quantiles]
    cutoffs[n.quantiles] = Inf
    sds.for.fit = sapply(1:(n.quantiles-1), function(i){
        sd(diffs[age.of.reference>=cutoffs[i] & age.of.reference<cutoffs[i+2]])
    })
    fit.for.sd = lm(sds.for.fit~midpoints)

    rv=c(mean.intercept=as.numeric(fit.for.mean$coefficients[1]),
         mean.slope=as.numeric(fit.for.mean$coefficients[2]),
         sd.intercept=as.numeric(fit.for.sd$coefficients[1]),
         sd.slope=as.numeric(fit.for.sd$coefficients[2]))

    rv
}

plot.fitted.age.model <- function(fitted.model, age.lower=13, age.upper=65, interval.z=1.96)
{
    ages=c(age.lower,age.upper)
    df = data.frame(age=ages,
                    mean=ages + fitted.model['mean.intercept'] + ages * fitted.model['mean.slope'],
                    sd=fitted.model['sd.intercept'] + ages * fitted.model['sd.slope'])
    df$lower = df$mean + df$sd * interval.z
    df$upper = df$mean - df$sd * interval.z

    ggplot(df, aes(x=age)) +
        geom_abline(intercept = 0, slope=1, linetype='dashed', size=1) +
        geom_ribbon(aes(ymin=lower, ymax=upper), size=1, alpha=0.5) +
        geom_line(aes(y=mean), size=1) +
        xlab('Age of Reference') + ylab('Age of Partner') +
        xlim(0, NA) + ylim(0, NA)
}

simulate.from.fitted.age.model <- function(fitted.model,
                                           ages,
                                           age.counts,
                                           nsim.per=4000,
                                           cutoffs=c(13,25,35,45,55,Inf))
{
    rv = list(ages.from=numeric(), ages.to=numeric())

    for (i in 1:(length(cutoffs)-1))
    {
        ages.in.range = ages >= cutoffs[i] & ages < cutoffs[i+1]
        cum.prob = cumsum(age.counts*ages.in.range) / sum(age.counts*ages.in.range)
        rands = runif(nsim.per)
        ages.to = sapply(rands, function(rand){
            ages[rand<cum.prob][1]
        })

        means = ages.to + fitted.model['mean.intercept'] + ages.to * fitted.model['mean.slope']
        sds = fitted.model['sd.intercept'] + ages.to * fitted.model['sd.slope']

        ages.from = rnorm(nsim.per, mean=means, sd=sds)

        rv$ages.from = c(rv$ages.from, ages.from)
        rv$ages.to = c(rv$ages.to, ages.to)
    }

    rv
}

##-----------------------------##
##-- SET-UP WITH ACTUAL DATA --##
##-----------------------------##

read.pairing.manager <- function(dir='../data2/Pairing')
{

    rv = list()

##-- IDU PAIRINGS by AGE --##
    #From Smith 2018 Paper

    smith.age.cutoffs = c(18,25,30,35,40,45,50,55,65)
    raw.idu.partners = t(matrix(c(21,17,10,2,3,0,1,0,
                              16,29,20,5,3,4,9,1,
                              15,29,37,51,45,18,20,6,
                              7,22,45,104,83,53,35,20,
                              15,17,33,101,175,144,83,12,
                              1,13,35,73,132,198,110,34,
                              3,8,13,58,68,136,151,76,
                              3,3,7,28,15,55,60,74),
                            ncol=8))

    deconstructed = matrix.to.scatter(raw.idu.partners,
                                      age.cutoffs=smith.age.cutoffs)
    rv$idu.age.model = fit.age.model(deconstructed$age.to, deconstructed$age.from)


##-- IDU PAIRINGS by RACE --##
    nonblack.with.nonblack.oe = 1.05
    nonblack.with.black.oe = 0.12
    black.with.black.oe = 9.12
    black.with.nonblack.oe = 0.76

    dim.names = list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other'))
    rv$idu.oe.race = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$idu.oe.race['black','black'] = black.with.black.oe
    rv$idu.oe.race['hispanic','black'] = black.with.nonblack.oe
    rv$idu.oe.race['other','black'] = black.with.nonblack.oe
    rv$idu.oe.race['black','hispanic'] = nonblack.with.black.oe
    rv$idu.oe.race['hispanic','hispanic'] = nonblack.with.nonblack.oe
    rv$idu.oe.race['other','hispanic'] = nonblack.with.black.oe
    rv$idu.oe.race['black','other'] = nonblack.with.black.oe
    rv$idu.oe.race['hispanic','other'] = nonblack.with.black.oe
    rv$idu.oe.race['other','other'] = nonblack.with.nonblack.oe

##-- IDU PAIRINGS by SEX --##

    #older
#    male.with.male = 1.05
#    male.with.female = 0.65
#    female.with.male = 1.21
#    female.with.female = 1.27

#    dim.names = list(sex.from=c('heterosexual_male','msm','female'), sex.to=c('heterosexual_male','msm','female'))
#    rv$idu.oe.sex = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
#    rv$idu.oe.sex['heterosexual_male','heterosexual_male'] = male.with.male
#    rv$idu.oe.sex['msm','heterosexual_male'] = male.with.female
#    rv$idu.oe.sex['female','heterosexual_male'] = male.with.female
#    rv$idu.oe.sex['heterosexual_male','msm'] = male.with.female
#    rv$idu.oe.sex['msm','msm'] = male.with.male
#    rv$idu.oe.sex['female','msm'] = male.with.female
#    rv$idu.oe.sex['heterosexual_male','female'] = female.with.male
#    rv$idu.oe.sex['msm','female'] = female.with.male
#    rv$idu.oe.sex['female','female'] = female.with.female

    #get MSM with female from: https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
    #heterosexual mixing


    #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6457905/
    
    dim.names = list(sex.from=c('heterosexual_male','msm','female'), sex.to=c('heterosexual_male','msm','female'))
    sharing = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    sharing['female','msm'] = 0.30
    sharing['heterosexual_male','msm'] = 0.13
    sharing['msm','msm'] = 0.57
    
    sharing['female','heterosexual_male'] = 0.47
    sharing['heterosexual_male','heterosexual_male'] = 0.44
    sharing['msm','heterosexual_male'] = 0.08
    
    sharing['female','female'] = 0.18
    sharing['heterosexual_male','female'] = 0.67
    sharing['msm','female'] = 0.15
    
    counts = c(heterosexual_male=(1103-184),
               msm=184,
               female=606)
    proportions = counts/sum(counts)
    
    rv$idu.oe.sex = sharing/proportions
    
##-- SEXUAL PAIRINGS by SEX --##
##     (msm with females)     ##

    #from CDC NHBS: https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
    msm.sex.with.female.1 = (885+353)/(885+353+6422)
    #From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
    msm.sex.with.female.2 = (39+5+3+16) / (39+5+3+16 + 394+190+112+71)

    rv$msm.sex.with.female.estimates = c(msm.sex.with.female.1, msm.sex.with.female.2)


##-- SEXUAL PAIRINGS by RACE --##

    msm.sex.by.race.1 = msm.sex.by.race.2 = msm.sex.by.race.3 = msm.sex.by.race.4 =
        array(0, dim=c(race.from=3, race.to=3),
              dimnames=list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other')))

    #-- MSM #1 - From Bohl 2011 --#
    raw.msm.sex.by.race = t(matrix(c(77,26,98,295,10,72,56,110,35,33,130,266,333,150,464,1318), nrow=4))

    raw.to.race.strata = c(3,1,2,3)
    for (r1 in 1:dim(raw.msm.sex.by.race)[1])
    {
        for (r2 in 1:dim(raw.msm.sex.by.race)[2])
            msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
                raw.msm.sex.by.race[r1,r2]
    }

    #-- MSM #2 - Mustanski 2014 --#
    # From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
    # PMID = 25430501
    raw.msm.sex.by.race = matrix(c(94*2.07*c(82,5.7,4.3,7.8)/100,
                                   37*2.27*c(7.7,56.1,30.6,5.6)/100,
                                   24*1.79*c(2.6,14.7,75.0,7.8)/100,
                                   19*2.32*c(37.9,27.6,26.4,8.0)/100),
                                 nrow=4)

    raw.to.race.strata = c(1,2,3,3)
    for (r1 in 1:dim(raw.msm.sex.by.race)[1])
    {
        for (r2 in 1:dim(raw.msm.sex.by.race)[2])
            msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
                raw.msm.sex.by.race[r1,r2]
    }


    #-- MSM #3 - Fujimoto 2015 --#
    #From https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4312750/
    msm.sex.by.race.3['black','black'] = 610
    msm.sex.by.race.3['hispanic','black'] = 104
    msm.sex.by.race.3['other','black'] = 330
    msm.sex.by.race.3['black','hispanic'] = 8
    msm.sex.by.race.3['hispanic','hispanic'] = 49
    msm.sex.by.race.3['other','hispanic'] = 226
    msm.sex.by.race.3['black','other'] = 221
    msm.sex.by.race.3['hispanic','other'] = 191
    msm.sex.by.race.3['other','other'] = 1333

    #-- MSM #4 - Raymond 2009 --#
    raw.msm.sex.by.race = matrix(c(79,10,333,35,
                                   28,76,151,33,
                                   303,115,1332,271,
                                   103,57,472,134),
                                 nrow=4)
    #indexed Asian, Black, White, Latino

    raw.to.race.strata = c(3,1,3,2)
    for (r1 in 1:dim(raw.msm.sex.by.race)[1])
    {
        for (r2 in 1:dim(raw.msm.sex.by.race)[2])
            msm.sex.by.race.4[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.4[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
                raw.msm.sex.by.race[r1,r2]
    }

    #-- Young Heterosexuals - Hamilton 2015 --#
    # https://www.sciencedirect.com/science/article/pii/S1755436515000080?via%3Dihub
    het.sex.by.race.1 = array(0, dim=c(race.from=3, race.to=3),
          dimnames=list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other')))
    het.sex.by.race.1['black','black'] = 1078 * (.89 + .74) / 2
    het.sex.by.race.1['hispanic','black'] = 1078 * (.08 + .14) / 2
    het.sex.by.race.1['other','black'] = 1078 * (.03 + .12) / 2
    het.sex.by.race.1['black','hispanic'] = 745 * (.10 + .03) / 2
    het.sex.by.race.1['hispanic','hispanic'] = 745 * (.56 + .64) / 2
    het.sex.by.race.1['other','hispanic'] = 745 * (.34 + .33) / 2
    het.sex.by.race.1['black','other'] = 3656 * (.05 + .02) / 2
    het.sex.by.race.1['hispanic','other'] = 3656 * (.16 + .17) / 2
    het.sex.by.race.1['other','other'] = 3656 * (.79 + .82) / 2


    rv$msm.sex.by.race.oe = list(calculate.oe.ratios(msm.sex.by.race.1),
                                 calculate.oe.ratios(msm.sex.by.race.2),
                                 calculate.oe.ratios(msm.sex.by.race.3),
                                 calculate.oe.ratios(msm.sex.by.race.4))
    rv$het.sex.by.race.oe = list(calculate.oe.ratios(het.sex.by.race.1))

##-- SEXUAL PAIRINGS by AGE --##

    #From Chow 2016
    #http://www.publish.csiro.au/sh/Fulltext/SH16055

    raw.heterosexual.by.age = read.csv(paste0(dir, '/heterosexual age.csv'))
    raw.msm.by.age = read.csv(paste0(dir, '/msm age.csv'))

    fit.female.with.male = fit.age.model(raw.heterosexual.by.age[,1], raw.heterosexual.by.age[,2])
    fit.male.with.female = fit.age.model(raw.heterosexual.by.age[,2], raw.heterosexual.by.age[,1])
    fit.msm = fit.age.model(raw.msm.by.age[,1], raw.msm.by.age[,2])

    rv$sex.age.models = list(heterosexual_male = fit.male.with.female,
                             female = fit.female.with.male,
                             msm = fit.msm)

    # test MSM from Chow against Grey
#    simmed.msm = simulate.from.fitted.age.model(rv$fit.msm.sex.by.age,
#                                                ages=18:29,
#                                                age.counts=rep(1, 29-18+1),
#                                                cutoffs=c(18,20,25,30))

#    diffs = simmed.msm$ages.from - simmed.msm$ages.to
#    quantile(diffs[simmed.msm$ages.to>=18 & simmed.msm$ages.to<20], probs=c(.25,.5,.75))
#    quantile(diffs[simmed.msm$ages.to>=20 & simmed.msm$ages.to<25], probs=c(.25,.5,.75))
#    quantile(diffs[simmed.msm$ages.to>=25 & simmed.msm$ages.to<30], probs=c(.25,.5,.75))


##-- RETURN IT --##
    rv
}

##-----------------------##
##-- MID-LEVEL GETTERS --##
##-----------------------##

get.age.proportions.from.age.model <- function(age.model,
                                               ages.to.use,
                                               age.counts,
                                               age.cutoffs,
                                               nsim.per=1000)
{
    simmed = simulate.from.fitted.age.model(age.model,
                                            ages=ages.to.use,
                                            age.counts=age.counts,
                                            nsim.per=nsim.per,
                                            cutoffs=age.cutoffs)

    calculate.column.proportions(scatter.to.matrix(simmed$ages.from, simmed$ages.to, cutoffs=age.cutoffs))
}

##-------------##
##-- GETTERS --##
##-------------##

get.idu.by.age.proportions <- function(pairing.manager,
                                       ages.to.use,
                                       age.counts,
                                       age.cutoffs)
{
    simmed = simulate.from.fitted.age.model(pairing.manager$idu.age.model,
                                            ages=ages.to.use,
                                            age.counts=age.counts,
                                            nsim.per=2000,
                                            cutoffs=age.cutoffs)

    calculate.column.proportions(scatter.to.matrix(simmed$ages.from, simmed$ages.to, cutoffs=age.cutoffs))
}

get.idu.by.race.proportions <- function(pairing.manager,
                                     race.counts,
                                     races)
{
    get.pairing.proportions(pairing.manager$idu.oe.race[races, races], race.counts)
}

get.idu.by.sex.proportions <- function(pairing.manager,
                                    sex.counts,
                                    sexes)
{
    get.pairing.proportions(pairing.manager$idu.oe.sex[sexes, sexes], sex.counts)
}



get.sex.by.age.proportions <- function(pairing.manager,
                                       ego.sex=c('heterosexual_male','msm','female')[1],
                                       ages.to.use,
                                       age.counts,
                                       age.cutoffs)
{
    age.model = pairing.manager$sex.age.models[[ego.sex]]
    simmed = simulate.from.fitted.age.model(age.model,
                                            ages=ages.to.use,
                                            age.counts=age.counts,
                                            nsim.per=2000,
                                            cutoffs=age.cutoffs)

    calculate.column.proportions(scatter.to.matrix(simmed$ages.from, simmed$ages.to, cutoffs=age.cutoffs))
}

get.sex.by.race.proportions <- function(pairing.manager,
                                        race.counts,
                                        races)
{
    sex.by.race.oes = c(pairing.manager$msm.sex.by.race.oe, pairing.manager$het.sex.by.race.oe)
    mean.oe = sex.by.race.oes[[1]]
    for (i in 1:length(sex.by.race.oes))
        mean.oe = sex.by.race.oes[[i]]
    mean.oe = mean.oe / length(sex.by.race.oes)

    get.pairing.proportions(mean.oe[races,races], race.counts)
}

get.sex.by.sex.proportions <- function(pairing.manager,
                                       sex.counts,
                                       sexes)
{
    mat = array(0, dim=c(sex.from=length(sexes), sex.to=length(sexes)),
               dimnames=list(sex.from=sexes, sex.to=sexes))

    msm.with.female = mean(pairing.manager$msm.sex.with.female.estimates)
    mat['heterosexual_male','female'] = sex.counts['heterosexual_male']
    mat['msm','female'] = sex.counts['msm']
    mat['female','heterosexual_male'] = 1
    mat['female','msm'] = msm.with.female
    mat['msm','msm'] = 1-msm.with.female

    calculate.column.proportions(mat)
}

##-- OLD - saved just in case I want to get some of the code back
OLD.read.pairing.manager <- function(dir='../data2/Pairing',
                                 target.age.cutoffs = c(13,25,35,45,55,Inf))
{
    smith.age.cutoffs = c(18,25,30,35,40,45,50,55,65)

    rv = list()
    #-- IDU AGE PAIRINGS --#
    #From Smith 2018 Paper
    raw.idu.partners = t(matrix(c(21,17,10,2,3,0,1,0,
                                  16,29,20,5,3,4,9,1,
                                  15,29,37,51,45,18,20,6,
                                  7,22,45,104,83,53,35,20,
                                  15,17,33,101,175,144,83,12,
                                  1,13,35,73,132,198,110,34,
                                  3,8,13,58,68,136,151,76,
                                  3,3,7,28,15,55,60,74),
                                ncol=8))
    ages = make.age.strata(AGE.CUTOFFS)
    idu.partners = array(0, dim=c(age.from=length(ages$labels), age.to=length(ages$labels)),
                         dimnames=list(age.from=ages$labels, age.to=ages$labels))
    raw.to.age.strata = c(1,2,2,3,3,4,4,5)
    for (a1 in 1:dim(idu.raw.partners)[1])
    {
        for (a2 in 1:dim(idu.raw.partners)[2])
            idu.partners[raw.to.age.strata[a1], raw.to.age.strata[a2]] = idu.partners[raw.to.age.strata[a1], raw.to.age.strata[a2]] +
                idu.raw.partners[a1,a2]
    }

    rv$idu.oe.age <- calculate.oe.ratios(idu.partners)

    #testing if simulating works ok
    deconstructed = matrix.to.scatter(idu.raw.partners,
                                      age.cutoffs=smith.cutoffs)
    fitted.from.deconstructed = fit.age.model(deconstructed$age.to, deconstructed$age.from)

    tabbed.sim.ages = table(round(deconstructed$age.to))
    simmed.from.deconstructed = simulate.from.fitted.age.model(fitted.from.deconstructed,
                                                               ages=18:65,#as.numeric(names(tabbed.sim.ages)),#16:65,
                                                               age.counts=rep(1,65-18+1),#tabbed.sim.ages,#rep(1,65-16+1),
                                                               cutoffs = smith.cutoffs
    )

    summed.from.simmed = scatter.to.matrix(simmed.from.deconstructed$ages.from,
                                           simmed.from.deconstructed$ages.to,
                                           cutoffs = smith.cutoffs)
    round(calculate.column.proportions(summed.from.simmed),2)
    round(calculate.column.proportions(idu.raw.partners),2)

    #-- IDU RACE PAIRINGS --#
    nonblack.with.nonblack.oe = 1.05
    nonblack.with.black.oe = 0.12
    black.with.black.oe = 9.12
    black.with.nonblack.oe = 0.76

    dim.names = list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other'))
    rv$idu.oe.race = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$idu.oe.race['black','black'] = black.with.black.oe
    rv$idu.oe.race['hispanic','black'] = black.with.nonblack.oe
    rv$idu.oe.race['other','black'] = black.with.nonblack.oe
    rv$idu.oe.race['black','hispanic'] = nonblack.with.black.oe
    rv$idu.oe.race['hispanic','hispanic'] = nonblack.with.nonblack.oe
    rv$idu.oe.race['other','hispanic'] = nonblack.with.black.oe
    rv$idu.oe.race['black','other'] = nonblack.with.black.oe
    rv$idu.oe.race['hispanic','other'] = nonblack.with.black.oe
    rv$idu.oe.race['other','other'] = nonblack.with.nonblack.oe

    #-- IDU SEX PAIRINGS --#
    male.with.male = 1.05
    male.with.female = 0.65
    female.with.male = 1.21
    female.with.female = 1.27

    dim.names = list(sex.from=c('heterosexual_male','msm','female'), sex.to=c('heterosexual_male','msm','female'))
    rv$idu.oe.sex = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$idu.oe.sex['heterosexual_male','heterosexual_male'] = male.with.male
    rv$idu.oe.sex['msm','heterosexual_male'] = male.with.female
    rv$idu.oe.sex['female','heterosexual_male'] = male.with.female
    rv$idu.oe.sex['heterosexual_male','msm'] = male.with.female
    rv$idu.oe.sex['msm','msm'] = male.with.male
    rv$idu.oe.sex['female','msm'] = male.with.female
    rv$idu.oe.sex['heterosexual_male','female'] = female.with.male
    rv$idu.oe.sex['msm','female'] = female.with.male
    rv$idu.oe.sex['female','female'] = female.with.female

    #get MSM with female from: https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
    #heterosexual mixing

    #from CDC NHBS: https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
    msm.sex.with.female.1 = (885+353)/(885+353+6422)

    #From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
    msm.sex.with.female.2 = (39+5+3+16) / (39+5+3+16 + 394+190+112+71)



    msm.sex.by.race.1 = msm.sex.by.race.2 = array(0, dim=c(race.from=3, race.to=3),
                                                  dimnames=list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other')))

    #From Bohl 2011
    raw.msm.sex.by.race = t(matrix(c(77,26,98,295,10,72,56,110,35,33,130,266,333,150,464,1318), nrow=4))

    raw.to.race.strata = c(3,1,2,3)
    for (r1 in 1:dim(raw.msm.sex.by.race)[1])
    {
        for (r2 in 1:dim(raw.msm.sex.by.race)[2])
            msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
                raw.msm.sex.by.race[r1,r2]
    }

    #From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
    #PMID = 25430501
    raw.msm.sex.by.race = matrix(c(94*2.07*c(82,5.7,4.3,7.8)/100,
                                   37*2.27*c(7.7,56.1,30.6,5.6)/100,
                                   24*1.79*c(2.6,14.7,75.0,7.8)/100,
                                   19*2.32*c(37.9,27.6,26.4,8.0)/100),
                                 nrow=4)

    raw.to.race.strata = c(1,2,3,3)
    for (r1 in 1:dim(raw.msm.sex.by.race)[1])
    {
        for (r2 in 1:dim(raw.msm.sex.by.race)[2])
            msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
                raw.msm.sex.by.race[r1,r2]
    }


    #Ages
    #From Chow 2016
    #http://www.publish.csiro.au/sh/Fulltext/SH16055

    raw.heterosexual.by.age = read.csv(paste0(dir, '/heterosexual age.csv'))
    female.with.male.by.age = scatter.to.matrix(raw.heterosexual.by.age[,2], raw.heterosexual.by.age[,1],
                                                AGE.CUTOFFS)
    male.with.female.by.age = scatter.to.matrix(raw.heterosexual.by.age[,1], raw.heterosexual.by.age[,2],
                                                AGE.CUTOFFS)


    raw.msm.by.age = read.csv(paste0(dir, '/msm age.csv'))
    msm.sex.by.age = scatter.to.matrix(raw.msm.by.age[,1], raw.msm.by.age[,2], AGE.CUTOFFS)

    calculate.oe.ratios(msm.sex.by.age+het.sex.by.age)

    #for now
    df = data.frame(a1=c(raw.heterosexual.by.age[,1],raw.msm.by.age[,1]),
                    a2=c(raw.heterosexual.by.age[,2],raw.msm.by.age[,2]),
                    sex1 = c(rep('female',dim(raw.heterosexual.by.age)[1]), rep('msm',dim(raw.msm.by.age)[1])),
                    sex2 = c(rep('male',dim(raw.heterosexual.by.age)[1]), rep('msm',dim(raw.msm.by.age)[1])))
    df$diff = df$a2-df$a1

    ggplot(df, aes(a1, a2, color=sex1)) + geom_point() + geom_smooth()

    df.het = df[df$sex1=='female',]
    #based on above plot, can probably treat msm and het the same

    #testing
    fit.female.with.male = fit.age.model(raw.heterosexual.by.age[,1], raw.heterosexual.by.age[,2])
    fit.male.with.female = fit.age.model(raw.heterosexual.by.age[,2], raw.heterosexual.by.age[,1])

    female.counts = table(raw.heterosexual.by.age[,1])
    sim.female.with.male = simulate.from.fitted.age.model(fit.female.with.male,
                                                          ages=as.numeric(names(female.counts)),
                                                          age.counts=female.counts)
    simmed.female.with.male.by.age = scatter.to.matrix(sim.female.with.male$ages.from,
                                                       sim.female.with.male$ages.to,
                                                       AGE.CUTOFFS)
    rv
}
