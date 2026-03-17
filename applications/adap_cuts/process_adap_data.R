source("applications/ryan_white/ryan_white_main.R")
source("applications/adap_cuts/fitting_income_distributions.R")
source("applications/ryan_white/process_rw_data_for_priors.R")
ROOT.DIR = "../jheem_analyses/applications/ryan_white/ryan_white_data/adap_clients"

# CHECK ALL OF THIS TOMORROW 

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
YEAR = "2020"

total.dist = income.total[YEAR,]/sum(income.total[YEAR,])
black.dist = income.race[YEAR,,"black"]/sum(income.race[YEAR,,"black"])
hispanic.dist = income.race[YEAR,,"hispanic"]/sum(income.race[YEAR,,"hispanic"])
white.dist = income.race[YEAR,,"white"]/sum(income.race[YEAR,,"white"])

params.total = fit.tobit.normal.distribution.to.quantiles(p = total.dist,
                                                          upper.limit = 500)

params.black = fit.tobit.normal.distribution.to.quantiles(p = black.dist,
                                                          upper.limit = 500)

params.hispanic = fit.tobit.normal.distribution.to.quantiles(p = hispanic.dist,
                                                             upper.limit = 500)

params.white = fit.tobit.normal.distribution.to.quantiles(p = white.dist,
                                                          upper.limit = 500)

# Betas and CVs 
beta.black = log(params.black$mean/params.total$mean)
cv.black = (params.black$sd/params.black$mean) / (params.total$sd/params.total$mean)

beta.hispanic = log(params.hispanic$mean/params.total$mean)
cv.hispanic = (params.hispanic$sd/params.hispanic$mean) / (params.total$sd/params.total$mean)

beta.white = log(params.white$mean/params.total$mean)
cv.white = (params.white$sd/params.white$mean) / (params.total$sd/params.total$mean)

# save to compare - rename with each year
betas.and.cvs.2020 = rbind(beta.black,cv.black,
                           beta.hispanic,cv.hispanic,
                           beta.white,cv.white)

year.comparison = cbind(betas.and.cvs.2020,
                        betas.and.cvs.2021,
                        betas.and.cvs.2022,
                        betas.and.cvs.2023)
colnames(year.comparison) = c("2020","2021","2022","2023")

year.comparison

if(1==2){
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


