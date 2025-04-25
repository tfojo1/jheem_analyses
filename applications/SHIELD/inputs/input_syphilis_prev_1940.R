
# syphilis diagnosis rate per 100,000 in 19410
diag.rate= 368.2  

n.total.diag= 485560 #includes all stages of syphilis, congenital syphilis, and cases reported with stage of syphilis not stated.

n.diag.by.stage= c(n.ps=68231	,
                 n.el=109018	,
                 n.llu=202984)
n.congenital=17600
known.stages=sum(n.diag.by.stage)+n.congenital
p.congenital=n.congenital/known.stages

#remove congenital syphilis from total: 
n.non.congenital.diagnosis= n.diagnosis*(1-p.congenital)

# prevalence in the population: 
#population size coming from report" Statistical Abstract of the United States: 1943
# June 1944 Report Number: Statistical Abstract of the United States: 1943 (Sixty-fifth Number)
# https://www2.census.gov/library/publications/1944/compendia/1943statab/1943-01.pdf

N=131669275 #US population in 1940
n.under.5= 1054152 #population under 5
N=N-n.under.5 #adjusted population 

# assuming a reporting fraction = %of prevalent cases diagnosed/reported in a year
reported.fraction=.5

# true prevalence estimate:
prev.syphilis=(n.non.congenital.diagnosis/reported.fraction) / N


# prevalence estimate by stage:
p.prev.syphilis.by.stage=as.data.frame(t(n.diag.by.stage/sum(n.diag.by.stage)))

#assumong 1/4 of ps are in primary stage and the rest are secondary stage based on duration
p.prev.syphilis.by.stage$primary=1/4*p.prev.syphilis.by.stage$n.ps
p.prev.syphilis.by.stage$secondary=(1-1/4) * p.prev.syphilis.by.stage$n.ps


# for those with latelatent/unknown duration: assuming 25% are tertiary
p.prev.syphilis.by.stage$tertiary=1/4*p.prev.syphilis.by.stage$n.llu
p.prev.syphilis.by.stage$late.latent=(1-1/4) * p.prev.syphilis.by.stage$n.llu

print("estimated prevalence of non.congenital syphilis in 1940:")
print(prev.syphilis)
print("estiamted proportion of cases in different stages:")
print(p.prev.syphilis.by.stage)
