## COVID impact on testing, Viguerie et al (missed diagnoses): 
# https://journals.lww.com/jaids/fulltext/2023/04010/isolating_the_effect_of_covid_19_related.4.aspx 
    # Using incidence-based (linear fit) model estimates; Table 2

## COVID impact on sexual transmission: McKay et al: 
# https://link.springer.com/article/10.1007/s13178-021-00625-3
    # Using Table 3, stay-at-home order estimate for: "stopped having sex with casual partners" (0.178)
    # Not sure if there are any more from that table we could use? 
        # "had less sex compared to Feb/early March" (0.429)
        # "had fewer sexual partners compared to Feb/early March" (0.393)

get.reduction.in.testing = function(specification.metadata){
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  
  # Relative risk of probability of being tested = 1 - Percent of diagnoses missed 
  # total percent missed = 0.181 --> prob of testing = 1-0.181 = 0.891
  total.p = 0.891
  
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
  het.male.rr = 0.825/0.838 # male/msm (this assumes that the male estimate is overwhelmingly het male I think)
  het.female.rr = 0.774/0.838 # female/msm 
  
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

get.reduction.in.sexual.transmission = function(specification.metadata){
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  
  stopped.sex.with.casual.partners = 0.178
  
  rv = array(1-stopped.sex.with.casual.partners,
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv
}