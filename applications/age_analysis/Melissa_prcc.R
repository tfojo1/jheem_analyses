##---------------------------------------------##
##-- NEW SENSITIVITY ANALYSIS FOR MANUSCRIPT --##
##---------------------------------------------##

library(epiR)
library(scales)
cols = hue_pal()(2)

source("model/run_systematic.R")
source("interventions/extract_intervention_results.R")
load("cached/all.results_2023-05-06.Rdata")
simset.no.int = simset.list.full$no.int    
simset.all.max = simset.list.full$all.max

# CHOOSE WHETHER I'M USING INTERVENTION OR BASELINE
simset = simset.all.max # simset.no.int
INT.SCENARIO = "all.max" # "no.int"

# simset = simset.no.int
# INT.SCENARIO = "no.int"

# need: 
# outcome: change in proportion over 50 (one value per simulation)
# parameters/covariates (9): incidence, non-HIV mortality,  HIV-specific mortality; x3 age groups (one value of each per simulation)

##----------------------##
##-- CREATE DATAFRAME --##
##----------------------##

## OUTCOME ##
prp.over.50.2025 = sapply(simset@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = "2025",
                                       sexes = c("female","male"))
})

prp.over.50.2040 = sapply(simset@simulations, function(sim){
    calculate.percent.over.age.for.sim(sim, 
                                       age.point = 50,
                                       data.type = "prevalence",
                                       years = "2040",
                                       sexes = c("female","male"))
})

outcome = prp.over.50.2040 - prp.over.50.2025

## PARAMETERS/COVARIATES ## 
create.covariate.df = function(covariates = c("incidence","non.hiv.mortality","hiv.mortality"),
                               ages = list("under.30" = c(1:6),
                                           "age.30.to.50" = c(7:10),
                                           "age.50.and.over" = c(11:17)),
                               int.scenario = INT.SCENARIO){
    results.array.new = full.results.array[c("2025","2040"),,,,,INT.SCENARIO]

    rv = list()
    for(cov in covariates){
        
        if(cov=="incidence") # denominator for hiv incidence is hiv negative 
            denominator=(results.array.new[,,,"population",]-results.array.new[,,,"prevalence",]) # hiv negative 
        else # denominator for non-hiv mortality and hiv mortailty is hiv population 
            denominator=results.array.new[,,,"prevalence",]
        
        cov.array = results.array.new[,,,cov,]
        
        rv.age = list()
        for(age in names(ages)){
            
            cov.age = cov.array[,ages[[age]],,]
            denominator.age = denominator[,ages[[age]],,]
            
            cov.age.sum = apply(cov.age,c(1,4),sum)
            denominator.sum = apply(denominator.age,c(1,4),sum)
            
            cov.age.rate = cov.age.sum/denominator.sum
            
            ## USING ABSOLUTE DELTA IN PARAMETER ## 
            # browser()
            #cov.age.rate.diff = ((cov.age.rate["2040",]-cov.age.rate["2025",])/cov.age.rate["2025",])
             cov.age.rate.diff = (cov.age.rate["2040",]-cov.age.rate["2025",])
            # cov.age.rate.diff = (cov.age.rate["2025",]-cov.age.rate["2040",])/cov.age.rate["2025",]
            
            rv.age[[age]] = cov.age.rate.diff
        }
        
        rv[[cov]] = rv.age
        
        rv
        
    }
    
    rv = as.data.frame(rv)
    
}

covariate.data.frame = create.covariate.df()

##---------------------##
##-- CALCULATE PRCCS --##
##---------------------##

calculate.prcc = function(covariates, # covariates is a named matrix with 1 column for each covariate
                          covariate.names = dimnames(covariates)[[2]],
                          outcome,
                          return.ci=F){
    
    df = data.frame(covariates=covariates[,covariate.names,drop=F], # apply rank function for every column in covariates
                    outcome=outcome) 
    
    dimnames(df)[[2]]=c(covariate.names, "outcome")
    
    #  tryCatch({    # if this code triggers an error, it will evaluate to NA 
    prccs = epi.prcc(df)
    
    if(return.ci){
        rv = data.frame(prccs$est,prccs$lower,prccs$upper)
        dimnames(rv) = list(parameter = prccs$var,
                            stat = c("est","lower","upper"))
    } else{
        rv = prccs$est
        names(rv) = prccs$var
    }
    rv
    #   }, error=function(e){NA})
    
    
    
}

prccs = calculate.prcc(covariates = covariate.data.frame,
                       outcome=outcome,
                       return.ci = T)

o = order(abs(prccs$est),decreasing = T)
prccs = prccs[o,]

##------------------------##
##-- LOW/HIGH QUANTILES --##
##------------------------##

calculate.low.high.quantiles = function(parameters,
                                        parameter.name,
                                        outcome,
                                        probs=c(0.025,.25,.5,.75,.975),
                                        n=250){
    
    o=order(parameters[,parameter.name])
    low = quantile(outcome[o[1:n]],probs)
    high = quantile(outcome[o[(length(outcome)+1)-1:n]],probs)
    
    return(c(low,high))
    
}

low.high = sapply(dimnames(covariate.data.frame)[[2]],
                  calculate.low.high.quantiles, 
                  parameters=covariate.data.frame,
                  outcome=outcome,
                  n=250)

dim.names = list(quantile=c("lower.2","lower.1","median","upper.1","upper.2"),
                 subset = c("low","high"), # these are the low sims and high sims
                 parameter=dimnames(covariate.data.frame)[[2]])

dim(low.high) = sapply(dim.names,length)
dimnames(low.high) = dim.names

diff = low.high["median","high",]-low.high["median","low",]
parameters.to.plot = dimnames(low.high)$parameter[order(abs(diff), decreasing = T)]

##------------------##
##-- PLOT RESULTS --##
##------------------##

plot.low.high = function(low.high,
                         parameters.to.plot,
                         parameter.names.for.labels
                         ){
    
    mat = low.high[,,parameters.to.plot, drop=F]
    dimnames(mat)[[3]] = parameter.names.for.labels
    diff = mat["median","high",]-mat["median","low",]
    
    df = cbind(reshape2::melt(mat["median",,]),
               data.frame(lower.2=as.numeric(mat["lower.2",,]),
                          lower.1=as.numeric(mat["lower.1",,]),
                          upper.1=as.numeric(mat["upper.1",,]),
                          upper.2=as.numeric(mat["upper.2",,])))
    
    df$parameter=factor(df$parameter,levels=parameter.names.for.labels[order(abs(diff))])
    
    ggplot(df) + geom_boxplot(aes(y=parameter,xmiddle=value,xlower = lower.1,xupper = upper.1, xmin = lower.2, xmax = upper.2, 
                                  fill=subset), stat="identity", position="dodge")
    
}

# parameter.names.for.labels must be in the same order as parameters.to.plot
parameter.names.for.labels = c("General mortality, over age 50\n (PRCC: -0.92)","Incidence, under age 30\n (PRCC: 0.31)",
                               "General mortality, under age 30\n (PRCC: 0.72)","General mortality, age 30-50\n (PRCC: 0.35)",
                               "HIV-specific mortality, under age 30\n (PRCC: 0.15)",
                               "HIV-specific mortality, age 30-50\n (PRCC: -0.11)",
                               "HIV-specific mortality, age 50+\n (PRCC: -0.02)",
                               "Incidence, age 30-50\n (PRCC: -0.02)",
                               "Incidence, over age 50\n (PRCC: -0.11)")



jpeg(file=paste0("results/new_for_manuscript/Figure4.jpeg"), width = 1500,height = 1000,res=200)
plot.low.high(low.high = low.high,parameters.to.plot = parameters.to.plot,
              parameter.names.for.labels=parameter.names.for.labels) + 
    geom_vline(xintercept = 0.1486322, linetype="dashed") + 
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          legend.justification = "center",
          legend.direction = "vertical",
          legend.text = element_text(size = 10),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(margin=margin(r=-10),size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(margin=margin(r=10),size=12),
          axis.title.x = element_text(margin=margin(b=-10))) +
    scale_fill_manual(name = element_blank(),
                      labels = c("high" = "Simulations with the highest 25% of parameter values",
                                 "low" = "Simulations with the lowest 25% of parameter values"),
                      values = cols[1:2]) + 
    labs(x="\nChange in percent of PWH over 50, 2025-2040",
         y="")# Change in parameter value (2025-2040)"
# 0.0817984 if using no intervention scenario
# 0.1486322 if using intervention scenario
dev.off()

