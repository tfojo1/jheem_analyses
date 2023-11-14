
################################################################################
                      ##Create Proportion of MSM##
              ##Note this dataset pulls from the BRFSS state code
                      ##This is a weighted measure

################################################################################
data.list.brfss.state.msm = lapply(data.list.brfss.state.totals, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location, msm) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>%  #how do I weight this#
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})

###############################################################################
                ##Put MSM Proportion into data manager##
###############################################################################
######State-RISK-proportion.tested.n
msm.state = lapply(data.list.brfss.state.msm, `[[`, 2)  

for (data in msm.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}




