

specification.metadata = get.specification.metadata('ehe', 'C.12580')

inc = get.idu.incidence.rates(specification.metadata)
rem = get.idu.remission.rates(specification.metadata)
rel = get.idu.relapse.rates(specification.metadata)
pop = array(0, dim=dim(inc), dimnames=dimnames(inc))
pop[,,'female'] = get.base.initial.female.population('C.12580', specification.metadata)
pop[,,'msm'] = pop[,,'heterosexual_male'] = get.base.initial.male.population('C.12580', specification.metadata)

idu.mort = EHE_BASE_PARAMETER_VALUES['idu.mortality']
#p.idu = inc / (inc + EHE_BASE_PARAMETER_VALUES['idu.mortality'])

gen.mort = get.non.idu.general.mortality.rates('C.12580',specification.metadata)


pop.end = apply(simset$population['2025',,,,,,], c('age','race','sex','risk'), sum)
#t(apply(pop.end, c('risk', 'sim'), sum)) / apply(pop.end, 'sim', sum)
pop.p = pop.end / as.numeric(apply(pop.end, c('age','race','sex'), sum))

sim.p.idu = pop.p[,,,'active_IDU']
sim.p.remission = pop.p[,,,'IDU_in_remission']

sim.rem.idu.ratio = sim.p.remission / sim.p.idu
#aging.rate = 1/specification.metadata$age.spans
raw.aging.rate = do.get.empiric.aging.rates('C.12580', specification.metadata)[[1]][,,,2]
aging.rate = array(0, dim=sapply(dimnames(sim.rem.idu.ratio), length), dimnames=dimnames(sim.rem.idu.ratio))
aging.rate[1:4,,] = raw.aging.rate

naive.rem.idu.ratio = rem / rel
naive.rem.idu.ratio[1,,] = (rem[1,,]) / (rel[1,,] + aging.rate[1,,])
naive.rem.idu.ratio[2,,] = (rem[2,,] + aging.rate[2,,]) / (rel[2,,] + aging.rate[2,,])
naive.rem.idu.ratio[1:2,,] = (rem[1:2,,]) / (rel[1:2,,] + aging.rate[1:2,,])
naive.rem.idu.ratio = (rem + idu.mort) / rel
naive.rem.idu.ratio = (rem) / (rel + aging.rate)
naive.rem.idu.ratio[-1,,] = (rem[-1,,]) / (rel[-1,,] + aging.rate[-1,,] - aging.rate[-5,,])
#naive.rem.idu.ratio[2:4,,] = (rem[2:4,,] + aging.rate[2:4,,]) / (rel[2:4,,] + aging.rate[2:4,,])

naive.rem.idu.ratio = (rem + aging.rate) / (rel + aging.rate)
naive.rem.idu.ratio = (rem - aging.rate) / (rel - aging.rate)
naive.rem.idu.ratio[-1,,] = (rem[-1,,] - aging.rate[-1,,] + aging.rate[-5,,]) /
  (rel[-1,,] - aging.rate[-1,,] + aging.rate[-5,,])
naive.rem.idu.ratio = get.prior.to.active.idu.ratio('C.12580',specification.metadata)

range(naive.rem.idu.ratio)
rem.error = naive.rem.idu.ratio - sim.rem.idu.ratio
mean(abs(rem.error)) 

df.rem = reshape2::melt(naive.rem.idu.ratio, value.name = 'naive')
df.rem$sim = as.numeric(sim.rem.idu.ratio)


#library(ggplot2)
ggplot(df.rem, aes(sim, naive, color=age)) + geom_point() +
  geom_abline(intercept = 0, slope = 1)# + ylim(0,1) + xlim(0,1)

qplot(sim.rem.idu.ratio - naive.rem.idu.ratio)

raw.nonidu.aging.rate = do.get.empiric.aging.rates('C.12580', specification.metadata)[[1]][,,,1]
nonidu.aging.rate = array(0, dim=sapply(dimnames(sim.rem.idu.ratio), length), dimnames=dimnames(sim.rem.idu.ratio))
nonidu.aging.rate[1:4,,] = raw.nonidu.aging.rate


orig.naive.p.idu = (inc + aging.rate) / (inc + idu.mort + aging.rate + nonidu.aging.rate)
orig.naive.p.idu = (inc) / (inc + idu.mort + aging.rate)
orig.naive.p.idu = orig.naive.p.idu / (1 + orig.naive.p.idu * naive.rem.idu.ratio)

naive.p2 = naive.p1 = orig.naive.p.idu
idu.aging.up = orig.naive.p.idu[-5,,] * aging.rate[-5,,] 




naive.p2[-5,,] = naive.p2[-5,,] - idu.aging.up
naive.p2[-1,,] = naive.p2[-1,,] + idu.aging.up

naive.p.idu = naive.p2
naive.p.idu = orig.naive.p.idu

idu.error = naive.p.idu - sim.p.idu
mean(abs(idu.error))

df.idu = reshape2::melt(sim.p.idu, value.name = 'sim')
df.idu$naive = as.numeric(naive.p.idu)
ggplot(df.idu, aes(sim, naive, color=age)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + ylim(0,NA) + xlim(0,NA)


naive.p1 = (inc[1,,]) / (inc[1,,] + idu.mort + aging.rate[1,,])

i = 2
naive.pi = (inc[i,,]) / (inc[1,,] + idu.mort + aging.rate[i-1,,] - aging.rate[i,,])


naive.p.idu = array(0, dim=sapply(dimnames(sim.p.idu), length), dimnames=dimnames(sim.p.idu))
naive.p.idu[1,,] = (inc[1,,]) /
  (inc[1,,] + idu.mort + aging.rate[1,,])
for (i in 2:5)
  naive.p.idu[i,,] = (inc[i,,] + naive.p.idu[i-1,,] * aging.rate[i-1,,]) /
        (inc[i,,] + idu.mort + aging.rate[i,,])

naive.p.idu = naive.p.idu / (1 + naive.p.idu * naive.rem.idu.ratio)

idu.error = naive.p.idu - sim.p.idu
mean(abs(idu.error))

df.idu = reshape2::melt(sim.p.idu, value.name = 'sim')
df.idu$naive = as.numeric(naive.p.idu)
ggplot(df.idu, aes(sim, naive, color=age)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + ylim(0,NA) + xlim(0,NA)

naive.p.idu = naive.p.idu / (1 + naive.p.idu * naive.rem.idu.ratio)





sim.active.never.ratio = sim.p.idu / (1-sim.p.idu-sim.p.remission)
naive.active.never.ratio = array(0, dim=sapply(dimnames(sim.p.idu), length), dimnames=dimnames(sim.p.idu))
naive.active.never.ratio[1,,] = (inc[1,,]) /
  (inc[1,,] + idu.mort + aging.rate[1,,])
for (i in 2:5)
  naive.active.never.ratio[i,,] = (inc[i,,] + naive.active.never.ratio[i-1,,] * aging.rate[i-1,,]) /
        (inc[i,,] + idu.mort + aging.rate[i,,])

naive.active.never.ratio = array(0, dim=sapply(dimnames(sim.p.idu), length), dimnames=dimnames(sim.p.idu))
naive.active.never.ratio[1,,] = (inc[1,,]) /
  (inc[1,,] + idu.mort + aging.rate[1,,])
for (i in 2:5)
  naive.active.never.ratio[i,,] = (inc[i,,] + naive.active.never.ratio[i-1,,] * aging.rate[i-1,,] * pmin(1,pop[i-1,,]/pop[i,,])) /
  (inc[i,,] + idu.mort + aging.rate[i,,])

# 
# naive.active.never.ratio = inc / (inc * (1+naive.rem.idu.ratio) + idu.mort)
# 
# 
# naive.active.never.ratio = array(0, dim=sapply(dimnames(sim.p.idu), length), dimnames=dimnames(sim.p.idu))
# naive.active.never.ratio[1,,] = (inc[1,,]) /
#   (inc[1,,] * (1+naive.rem.idu.ratio[1,,]) + idu.mort + aging.rate[1,,])
# for (i in 2:5)
#   naive.active.never.ratio[i,,] = (inc[i,,] + naive.active.never.ratio[i-1,,] * aging.rate[i-1,,]) /
#   (inc[i,,] * (1+naive.rem.idu.ratio[1,,]) + idu.mort + aging.rate[i,,])


#naive.active.never.ratio = (inc) /  (inc + idu.mort + aging.rate)
#naive.active.never.ratio = (inc) /  (inc + idu.mort)

naive.active.never.ratio = get.active.to.non.idu.ratio('C.12580',specification.metadata)

idu.error = naive.active.never.ratio - sim.p.idu
mean(abs(idu.error))

df.idu = reshape2::melt(sim.active.never.ratio, value.name = 'sim')
df.idu$naive = as.numeric(naive.active.never.ratio)
ggplot(df.idu, aes(sim, naive, color=age)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + ylim(0,NA) + xlim(0,NA)

df.idu$inc = as.numeric(inc)
df.idu$rem = as.numeric(rem)
df.idu$rem.active = as.numeric(sim.rem.idu.ratio)
df.idu$aging = as.numeric(aging.rate)
df.idu$aging.into = c(rep(0, 9), as.numeric(aging.rate)[1:36])
ggplot(df.idu, aes(sim, aging.into, color=age)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + ylim(0,NA) + xlim(0,NA)
