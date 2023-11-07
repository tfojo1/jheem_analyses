## COVID impact on testing, Viguerie et al (missed diagnoses): 
# https://journals.lww.com/jaids/fulltext/2023/04010/isolating_the_effect_of_covid_19_related.4.aspx 
    # Using incidence-based (averaged) model estimates; Table 2

## COVID impact on testing by age, Patel et al: 
# https://pubmed.ncbi.nlm.nih.gov/36094476/


## COVID impact on sexual transmission: McKay et al: 
# https://link.springer.com/article/10.1007/s13178-021-00625-3
    # Using Table 3, averaged stay-at-home order estimates for: 
        # "stopped having sex with casual partners" (0.178)
        # "had fewer sexual partners compared to Feb/early March" (0.393)


if(1==2){
  source('applications/EHE/ehe_specification.R')
  source('commoncode/age_mappings.R')
  specification.metadata = get.specification.metadata('ehe', 
                                                      location = 'c.12580')
}

get.covid.reduction.in.testing = function(specification.metadata){
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]

  # Relative risk of probability of being tested = 1 - Percent of diagnoses missed 
  # total percent missed = 0.183 --> prob of testing = 1-0.183 = 0.817
  # maybe use MSM (1-.162); can also solve to get the white multiplier
  total.p = 0.817
  
  # Creating a reference group: other, msm 

  # RACE
  # other percent missed = 0.168 --> prob of testing = 1-0.168 = 0.832
  # black = 0.164 --> p = 1-0.164 = 0.836
  # hispanic = 0.218 --> p = 1-0.218 = 0.782
  black.rr = 0.836/0.832 # black/other
  hispanic.rr = 0.782/0.832 # hispanic/other
    
  # SEX
  # male = 0.175 --> p = 1-0.175 = 0.825
  # female = 0.226 --> p = 1-0.226 = 0.774

  # RISK
  # msm = 0.162 --> p = 1-0.162 = 0.838
  # idu = 0.193 --> p = 1-0.193 = 0.807
  # msm_idu = 0.308 --> p = 1-0.308 = 0.692
  # heterosexual = 0.243 --> p = 1-0.243 = 0.757
  idu.rr = 0.807/0.838 # idu/msm
  msm_idu.rr = 0.692/0.838 # msm_idu/msm
  het.female.rr = 0.774/0.838 # female/msm 
  # het.male.rr = 0.825/0.838 # male/msm # solve this better using numerators:  
  
  male.denom = 2631/.175 # numerator divided by proportion --> denom
  msm.denom = 2034/.162 # numerator divided by proportion --> denom
  
  het.male.num = 2631 - 2034 # all male - MSM
  het.male.denom = male.denom - msm.denom  # all male - MSM
  
  het.male = het.male.num/het.male.denom # new % missed 
  het.male.p = 1-het.male # p = 1 - % missed
  
  het.male.rr = het.male.p/0.838
  
  rv = array(total.p,
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv[,"black",,] = rv[,"black",,]*black.rr
  rv[,"hispanic",,] = rv[,"hispanic",,]*hispanic.rr
  
  # heterosexual male and female; non-IDU
  rv[,,"heterosexual_male",c("never_IDU","IDU_in_remission")] = rv[,,"heterosexual_male",c("never_IDU","IDU_in_remission")]*het.male.rr
  rv[,,"female",c("never_IDU","IDU_in_remission")] = rv[,,"female",c("never_IDU","IDU_in_remission")]*het.female.rr
  
  # IDU, non-msm
  rv[,,c("heterosexual_male","female"),"active_IDU"] = rv[,,c("heterosexual_male","female"),"active_IDU"]*idu.rr
  
  # MSM-IDU
  rv[,,"msm","active_IDU"] = rv[,,"msm","active_IDU"]*msm_idu.rr
  
  rv

}

get.covid.reduction.in.sexual.transmission = function(specification.metadata){
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  
  stopped.sex.with.casual.partners = 0.178
  fewer.sexual.partners = 0.393
  
  avg.reduction = mean(c(stopped.sex.with.casual.partners,fewer.sexual.partners))
  
  rv = array(1-avg.reduction,
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv
}

get.q2.covid.reduction = function(){
  
  # Q2 reduction relative to full year reduction
  # overall reduction * (q2 reduction / overall reduction) = q2 reduction
  
  # Using number of tests
  # tests.2019 = sum(2101633,2523317,2572963,2451303)
  # tests.2020 = sum(2471614,1682578,2325554,2274593)
  # 
  # overall.reducton.tests = tests.2020/tests.2019
  # 
  # q2.2019.tests = 2523317
  # q2.2020.tests = 1682578
  # 
  # q2.reduction.tests = q2.2020.tests/q2.2019.tests
  # q2.to.full.year.tests = q2.reduction.tests/overall.reducton.tests # 0.734
  
  # Using diagnosed HIV 
  diagnosed.2019 = sum(9488,9431,9164,8392)
  diagnosed.2020 = sum(8438,6228,7905,7758)
  
  overall.reduction.diagnosed = diagnosed.2020/diagnosed.2019
  
  q2.2019.diagnosed = 9431
  q2.2020.diagnosed = 6228
  
  q2.reduction.diagnosed = q2.2020.diagnosed/q2.2019.diagnosed
  
  q2.to.full.year.diagnosed = q2.reduction.diagnosed/overall.reduction.diagnosed # 0.794
  
  q2.to.full.year.diagnosed # multiply this by overall reduction in previous function to get max reduction in q2
  
}

get.age.covid.reduction = function(specification.metadata){
  
  tests.2019 = 2324421
  tests.2020 = 1255895
  
  overall.reduction = tests.2020/tests.2019
  
  age.2019 = c(4589,152741,836193,589461,326494,394254)
  age.2020 = c(3035,73039,441721,323769,178929,218445)
  
  names(age.2019) = names(age.2020) = c("<13","13-19","20-29","30-39","40-49","50+")

  age.reduction = age.2020/age.2019
  
  age.to.all.ages.reduction = age.reduction/overall.reduction
  
  model.ages = specification.metadata$dim.names[c('age')]$age
  age.info = parse.age.strata.names(names(age.to.all.ages.reduction))
  
  age.to.all.ages.reduction.mapped = map.age.values(values = age.to.all.ages.reduction,
                                                    given.age.lowers = age.info$lower,
                                                    given.age.uppers = age.info$upper, 
                                                    desired.ages = model.ages)
  
  
  age.to.all.ages.reduction.mapped
}












