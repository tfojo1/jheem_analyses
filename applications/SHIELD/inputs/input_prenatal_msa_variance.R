library(locations)
source("commoncode/locations_of_interest.R")
len=MSAS.OF.INTEREST
# data by year_location
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location
# those wiht a completeness index:
completeness=SURVEILLANCE.MANAGER$data$completeness.no.prenatal.care$estimate$cdc.wonder.natality$cdc.fertility$year__location
#those with msa estimates: 
#'@Zoe: where are the rest
msas=dimnames(SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location)$location

m=lapply(c(1:length(msas)),function(x){
  # print(x)
  msa=msas[x]
  # print(paste("msa= ",msa," completeness= ",mean(completeness[,msa])))
  msa.est= SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location[,msa]
  
  counties = locations::get.contained.locations(msa, 'county');counties
  counties.est=SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,counties]
  counties.est
  if(length(counties)==1) {
    df=cbind("msa.est"=msa.est,"counties.est"=counties.est)
  }else{
  df<-lapply(c(1:length(counties)),function(y){
    cbind("msa.est"=msa.est,"counties.est"=counties.est[,y])
  })
  df<-do.call(rbind,df)
  }
  # head(df)
  var=sum((df[,"msa.est"]-df[,"counties.est"])^2)
  return(c("msa"=msa,"counties"=length(counties), "completeness"= mean(completeness[,msa]),"variance"=var))
})
m=do.call(rbind,m)
