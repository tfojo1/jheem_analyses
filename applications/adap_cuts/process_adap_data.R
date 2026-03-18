source("applications/ryan_white/ryan_white_main.R")
source("applications/adap_cuts/fitting_income_distributions.R")
source("applications/ryan_white/process_rw_data_for_priors.R")
ROOT.DIR = "../jheem_analyses/applications/ryan_white/ryan_white_data/adap_clients"

income.race = read.adap.income.data(ROOT.DIR,
                                    dimension = "race")

income.age = read.adap.income.data(ROOT.DIR,
                                    dimension = "age")

income.sex.1 = read.adap.income.data(ROOT.DIR,
                                     dimension = "sex",
                                     years=2020:2022)

income.sex.2 = read.adap.income.data(ROOT.DIR,
                                     dimension = "sex",
                                     years=2023)

income.service = read.adap.income.data(ROOT.DIR,
                                   dimension = "service")

income.total = read.adap.income.data(ROOT.DIR,
                                     dimension = "total")


# Visualize/fit distributions
betas.and.cvs.race = list()
betas.and.cvs.age = list()
betas.and.cvs.sex = list()
betas.and.cvs.service = list()
YEARS = as.character(2020:2023)

for(YEAR in YEARS){
    
    total.dist = income.total[YEAR,]/sum(income.total[YEAR,])
    
    params.total = fit.tobit.normal.distribution.to.quantiles(p = total.dist,
                                                              upper.limit = 500)
    
    ## RACE ## 
    black.dist = income.race[YEAR,,"black"]/sum(income.race[YEAR,,"black"])
    hispanic.dist = income.race[YEAR,,"hispanic"]/sum(income.race[YEAR,,"hispanic"])
    white.dist = income.race[YEAR,,"white"]/sum(income.race[YEAR,,"white"])
    
    params.black = fit.tobit.normal.distribution.to.quantiles(p = black.dist,
                                                              upper.limit = 500)
    
    params.hispanic = fit.tobit.normal.distribution.to.quantiles(p = hispanic.dist,
                                                                 upper.limit = 500)
    
    params.white = fit.tobit.normal.distribution.to.quantiles(p = white.dist,
                                                              upper.limit = 500)
    
    beta.black = log(params.black$mean/params.total$mean)
    cv.black = log((params.black$sd/params.black$mean) / (params.total$sd/params.total$mean))
    
    beta.hispanic = log(params.hispanic$mean/params.total$mean)
    cv.hispanic = log((params.hispanic$sd/params.hispanic$mean) / (params.total$sd/params.total$mean))
    
    beta.white = log(params.white$mean/params.total$mean)
    cv.white = log((params.white$sd/params.white$mean) / (params.total$sd/params.total$mean))
    
    betas.and.cvs.race[[YEAR]] = rbind(beta.black,beta.hispanic,beta.white,
                                       cv.black,cv.hispanic,cv.white)
    
    
    
    ## AGE ## 
    age1.dist = apply(income.age[YEAR,,c(2:4)],"income",sum)/sum(income.age[YEAR,,c(2:4)]) # age 13-24
    age2.dist = apply(income.age[YEAR,,c(5:6)],"income",sum)/sum(income.age[YEAR,,c(5:6)]) # age 25-34
    age3.dist = apply(income.age[YEAR,,c(7:8)],"income",sum)/sum(income.age[YEAR,,c(7:8)]) # age 35-44
    age4.dist = apply(income.age[YEAR,,c(9:10)],"income",sum)/sum(income.age[YEAR,,c(9:10)]) # age 45-54
    age5.dist = apply(income.age[YEAR,,c(11:13)],"income",sum)/sum(income.age[YEAR,,c(11:13)]) # age 45-54

    params.age1 = fit.tobit.normal.distribution.to.quantiles(p = age1.dist,
                                                              upper.limit = 500)
    params.age2 = fit.tobit.normal.distribution.to.quantiles(p = age2.dist,
                                                             upper.limit = 500)
    params.age3 = fit.tobit.normal.distribution.to.quantiles(p = age3.dist,
                                                             upper.limit = 500)
    params.age4 = fit.tobit.normal.distribution.to.quantiles(p = age4.dist,
                                                             upper.limit = 500)
    params.age5 = fit.tobit.normal.distribution.to.quantiles(p = age5.dist,
                                                             upper.limit = 500)

    beta.age1 = log(params.age1$mean/params.total$mean)
    cv.age1 = log((params.age1$sd/params.age1$mean) / (params.total$sd/params.total$mean))
    
    beta.age2 = log(params.age2$mean/params.total$mean)
    cv.age2 = log((params.age2$sd/params.age2$mean) / (params.total$sd/params.total$mean))
    
    beta.age3 = log(params.age3$mean/params.total$mean)
    cv.age3 = log((params.age3$sd/params.age3$mean) / (params.total$sd/params.total$mean))
    
    beta.age4 = log(params.age4$mean/params.total$mean)
    cv.age4 = log((params.age4$sd/params.age4$mean) / (params.total$sd/params.total$mean))
    
    beta.age5 = log(params.age5$mean/params.total$mean)
    cv.age5 = log((params.age5$sd/params.age5$mean) / (params.total$sd/params.total$mean))
    
    betas.and.cvs.age[[YEAR]] = rbind(beta.age1,beta.age2,beta.age3,beta.age4,beta.age5,
                                      cv.age1,cv.age2,cv.age3,cv.age4,cv.age5)
    
    ## Sex ##
    if(YEAR %in% as.character(2020:2022)){
        male.dist = income.sex.1[YEAR,,"male"]/sum(income.sex.1[YEAR,,"male"])
        female.dist = income.sex.1[YEAR,,"female"]/sum(income.sex.1[YEAR,,"female"])
    } else if (YEAR == "2023"){
        male.dist = income.sex.2[YEAR,,"male"]/sum(income.sex.2[YEAR,,"male"])
        female.dist = income.sex.2[YEAR,,"female"]/sum(income.sex.2[YEAR,,"female"])
    }
    
    params.male = fit.tobit.normal.distribution.to.quantiles(p = male.dist,
                                                             upper.limit = 500)
    params.female = fit.tobit.normal.distribution.to.quantiles(p = female.dist,
                                                               upper.limit = 500)
    
    beta.male = log(params.male$mean/params.total$mean)
    cv.male = log((params.male$sd/params.male$mean) / (params.total$sd/params.total$mean))
    
    beta.female = log(params.female$mean/params.total$mean)
    cv.female = log((params.female$sd/params.female$mean) / (params.total$sd/params.total$mean))
    
    betas.and.cvs.sex[[YEAR]] = rbind(beta.male,beta.female,
                                      cv.male,cv.female)
    
    ## Service ## 
    full.pay.dist = income.service[YEAR,,"full.pay.n"]/sum(income.service[YEAR,,"full.pay.n"])
    premium.dist = income.service[YEAR,,"premium.n"]/sum(income.service[YEAR,,"premium.n"])
    copay.dist = income.service[YEAR,,"copay.n"]/sum(income.service[YEAR,,"copay.n"])
    multiple.dist = income.service[YEAR,,"multiple.n"]/sum(income.service[YEAR,,"multiple.n"])
    
    params.full.pay = fit.tobit.normal.distribution.to.quantiles(p = full.pay.dist,
                                                                 upper.limit = 500)
    params.premium = fit.tobit.normal.distribution.to.quantiles(p = premium.dist,
                                                                 upper.limit = 500)
    params.copay = fit.tobit.normal.distribution.to.quantiles(p = copay.dist,
                                                                 upper.limit = 500)
    params.multiple = fit.tobit.normal.distribution.to.quantiles(p = multiple.dist,
                                                                 upper.limit = 500)
    
    beta.full.pay = log(params.full.pay$mean/params.total$mean)
    cv.full.pay = log((params.full.pay$sd/params.full.pay$mean) / (params.total$sd/params.total$mean))
    
    beta.premium = log(params.premium$mean/params.total$mean)
    cv.premium = log((params.premium$sd/params.premium$mean) / (params.total$sd/params.total$mean))
    
    beta.copay = log(params.copay$mean/params.total$mean)
    cv.copay = log((params.copay$sd/params.copay$mean) / (params.total$sd/params.total$mean))
    
    beta.multiple = log(params.multiple$mean/params.total$mean)
    cv.multiple = log((params.multiple$sd/params.multiple$mean) / (params.total$sd/params.total$mean))
    
    betas.and.cvs.service[[YEAR]] = rbind(beta.full.pay,beta.premium,beta.copay,beta.multiple,
                                          cv.full.pay,cv.premium,cv.copay,cv.multiple)
}

year.comparison.race = cbind(betas.and.cvs.race[["2020"]],
                             betas.and.cvs.race[["2021"]],
                             betas.and.cvs.race[["2022"]],
                             betas.and.cvs.race[["2023"]])
year.comparison.age = cbind(betas.and.cvs.age[["2020"]],
                            betas.and.cvs.age[["2021"]],
                            betas.and.cvs.age[["2022"]],
                            betas.and.cvs.age[["2023"]])
year.comparison.sex = cbind(betas.and.cvs.sex[["2020"]],
                            betas.and.cvs.sex[["2021"]],
                            betas.and.cvs.sex[["2022"]],
                            betas.and.cvs.sex[["2023"]])
year.comparison.service = cbind(betas.and.cvs.service[["2020"]],
                                betas.and.cvs.service[["2021"]],
                                betas.and.cvs.service[["2022"]],
                                betas.and.cvs.service[["2023"]])

colnames(year.comparison.race) = colnames(year.comparison.age) = 
    colnames(year.comparison.sex) = colnames(year.comparison.service) = c("2020","2021","2022","2023")



## FINAL BETAS AND CVS ## 
exp(year.comparison.race)
exp(year.comparison.age)
exp(year.comparison.sex)
exp(year.comparison.service)


income.dist = income.total/rowSums(income.total)

params.total.2020 = fit.tobit.normal.distribution.to.quantiles(p = income.dist["2020",],
                                                               upper.limit = 500)

params.total.2021 = fit.tobit.normal.distribution.to.quantiles(p = income.dist["2021",],
                                                               upper.limit = 500)

params.total.2022 = fit.tobit.normal.distribution.to.quantiles(p = income.dist["2022",],
                                                               upper.limit = 500)

params.total.2023 = fit.tobit.normal.distribution.to.quantiles(p = income.dist["2023",],
                                                               upper.limit = 500)
rbind(params.total.2020,
      params.total.2021,
      params.total.2022,
      params.total.2023
)

params.total.all.years = fit.tobit.normal.distribution.to.quantiles(p = c(income.dist["2020",],
                                                                          income.dist["2021",],
                                                                          income.dist["2022",],
                                                                          income.dist["2023",]),
                                                                    upper.limit = 500)




# Plots
if(1==2){
# Race
YEAR = "2023"
{
    black.dist = income.race[YEAR,,"black"]/sum(income.race[YEAR,,"black"])
    hispanic.dist = income.race[YEAR,,"hispanic"]/sum(income.race[YEAR,,"hispanic"])
    white.dist = income.race[YEAR,,"white"]/sum(income.race[YEAR,,"white"])
    
    sim.df.inc.race = data.frame(
        value = c(black.dist,
                  hispanic.dist,
                  white.dist),
        income = factor(rep(names(total.dist), 3), levels=names(total.dist)),
        race = rep(c("Black", "Hispanic", "White"), each=length(total.dist))
    )
    ggplot(sim.df.inc.race) + geom_bar(stat='identity', aes(x=income, y=value, fill=race), position = 'dodge') + facet_wrap(~race) +
        scale_y_continuous(limits = c(0,1), labels = scales::percent) +
        ylab("Proportion of Clients") + xlab("Income") + ggtitle(paste0("Income Distribution Among ADAP Clients, By Race: ",YEAR))
}

# Age 
YEAR = "2023"
{
    age1.dist = apply(income.age[YEAR,,c(2:4)],"income",sum)/sum(income.age[YEAR,,c(2:4)]) # age 13-24
    age2.dist = apply(income.age[YEAR,,c(5:6)],"income",sum)/sum(income.age[YEAR,,c(5:6)]) # age 25-34
    age3.dist = apply(income.age[YEAR,,c(7:8)],"income",sum)/sum(income.age[YEAR,,c(7:8)]) # age 35-44
    age4.dist = apply(income.age[YEAR,,c(9:10)],"income",sum)/sum(income.age[YEAR,,c(9:10)]) # age 45-54
    age5.dist = apply(income.age[YEAR,,c(11:13)],"income",sum)/sum(income.age[YEAR,,c(11:13)]) # age 45-54 
    
    sim.df.inc.age = data.frame(
        value = c(age1.dist,
                  age2.dist,
                  age3.dist,
                  age4.dist,
                  age5.dist),
        income = factor(rep(names(total.dist), 5), levels=names(total.dist)),
        age = rep(c("13-24 years", "25-34 years", "35-44 years","45-54 years","55+ years"), each=length(total.dist))
    )
    ggplot(sim.df.inc.age) + geom_bar(stat='identity', aes(x=income, y=value, fill=age), position = 'dodge') + facet_wrap(~age) +
        scale_y_continuous(limits = c(0,1), labels = scales::percent) +
        ylab("Proportion of Clients") + xlab("Income") + ggtitle(paste0("Income Distribution Among ADAP Clients, By Age: ",YEAR))
}

# Sex 
YEAR = "2023"
{
    if(YEAR %in% as.character(2020:2022)){
        male.dist = income.sex.1[YEAR,,"male"]/sum(income.sex.1[YEAR,,"male"])
        female.dist = income.sex.1[YEAR,,"female"]/sum(income.sex.1[YEAR,,"female"])
    } else if (YEAR == "2023"){
        male.dist = income.sex.2[YEAR,,"male"]/sum(income.sex.2[YEAR,,"male"])
        female.dist = income.sex.2[YEAR,,"female"]/sum(income.sex.2[YEAR,,"female"])
    }
    
    sim.df.inc.sex = data.frame(
        value = c(male.dist,
                  female.dist),
        income = factor(rep(names(total.dist), 2), levels=names(total.dist)),
        sex = rep(c("Male", "Female"), each=length(total.dist))
    )
    ggplot(sim.df.inc.sex) + geom_bar(stat='identity', aes(x=income, y=value, fill=sex), position = 'dodge') + facet_wrap(~sex) +
        scale_y_continuous(limits = c(0,1), labels = scales::percent) +
        ylab("Proportion of Clients") + xlab("Income") + ggtitle(paste0("Income Distribution Among ADAP Clients, By Sex: ",YEAR))

}

# Service
YEAR = "2023"
{
    full.pay.dist = income.service[YEAR,,"full.pay.n"]/sum(income.service[YEAR,,"full.pay.n"])
    premium.dist = income.service[YEAR,,"premium.n"]/sum(income.service[YEAR,,"premium.n"])
    copay.dist = income.service[YEAR,,"copay.n"]/sum(income.service[YEAR,,"copay.n"])
    multiple.dist = income.service[YEAR,,"multiple.n"]/sum(income.service[YEAR,,"multiple.n"])
    
    sim.df.inc.service = data.frame(
        value = c(full.pay.dist,
                  premium.dist,
                  copay.dist,
                  multiple.dist),
        income = factor(rep(names(total.dist), 4), levels=names(total.dist)),
        service = rep(c("Full pay", "Premium", "Co-pay","Multiple"), each=length(total.dist))
    )
    ggplot(sim.df.inc.service) + geom_bar(stat='identity', aes(x=income, y=value, fill=service), position = 'dodge') + facet_wrap(~service) +
        scale_y_continuous(limits = c(0,1), labels = scales::percent) +
        ylab("Proportion of Clients") + xlab("Income") + ggtitle(paste0("Income Distribution Among ADAP Clients, By Service: ",YEAR))
}

}


