# Estimates of IDU sexual oe from here: https://pmc.ncbi.nlm.nih.gov/articles/PMC6816039/

# Observed: 
# “Among women, 17.5% of those who reported injection-related behaviors had sex with a male PWID [...] 
# "Figures for men were similar, with 17.3% versus 0.9% having sex with a female PWID [...]

# Expected: 
# “In 2011–15, 1.4% of sexually active women and 2.6% of sexually active men reported ever engaging in injection-related behaviors.” 

get.idu.sexual.oes = function(specification.metadata){
  # Estimate 
  # Female 
  female.observed = 0.1746
  female.expected = .0137
  female.oe = female.observed/female.expected # 12.74
  
  # Male
  male.observed = .1729
  male.expected = .026
  male.oe = male.observed/male.expected # 6.65
  
  # Variance 
  # Female 
  female.observed.upper = .2553
  female.observed.lower = .1155
  log.female.observed.ci = log(female.observed.upper) - log(female.observed.lower)
  log.female.observed.sd = log.female.observed.ci/4
  
  female.expected.upper = .0178
  female.expected.lower = .0105
  log.female.expected.ci = log(female.expected.upper) - log(female.expected.lower)
  log.female.expected.sd = log.female.expected.ci/4
  
  log.combined.female.variance = (log.female.observed.sd^2) + (log.female.expected.sd^2) # 0.05673213 - exp again? below 
  #exp(log.combined.female.variance) # 1.058372
  
  # Male 
  male.observed.upper = .2776
  male.observed.lower = .1021
  log.male.observed.ci = log(male.observed.upper) - log(male.observed.lower)
  log.male.observed.sd = log.male.observed.ci/4
  
  male.expected.upper = .0331
  male.expected.lower = .0205
  log.male.expected.ci = log(male.expected.upper) - log(male.expected.lower)
  log.male.expected.sd = log.male.expected.ci/4
  
  log.combined.male.variance = (log.male.observed.sd^2) + (log.male.expected.sd^2) # 0.07687512
  #exp(log.combined.male.variance) # 1.079907
  
  log.combined.variance = (log.combined.female.variance + log.combined.male.variance)/2
  combined.sd = sqrt(log.combined.variance)
  
  dim.names = specification.metadata$dim.names[c('sex.to')]
  
  rv = c(heterosexual_male = male.oe,
         msm = male.oe,
         female = female.oe)
  
  dim(rv) = sapply(dim.names, length)
  dimnames(rv) = dim.names
  
  
  
  rv
}


