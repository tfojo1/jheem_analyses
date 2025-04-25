library(locations)
source("commoncode/locations_of_interest.R")
len=MSAS.OF.INTEREST;print(len)

# data by year_location
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location
# those wiht a completeness index:
completeness=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location

#those with msa estimates: 
msas=dimnames(SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location)$location
print(length(msas))


m=lapply(c(1:length(msas)),function(x){
   # print(x)
   msa=msas[x]
   # print(paste("msa= ",msa," completeness= ",mean(completeness[,msa])))
  msa.est= SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location[,msa]
  
  counties = locations::get.contained.locations(msa, 'county');counties
  counties.subset= intersect(counties,dimnames(SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location)$location)
  
  counties.est=SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,counties.subset]
  counties.est
  if(length(counties.subset)==1) {
    df=cbind("msa.est"=msa.est,"counties.est"=counties.est)
  }else{
  df<-lapply(c(1:length(counties.subset)),function(y){
    cbind("msa.est"=msa.est,"counties.est"=counties.est[,y])
  })
  df<-do.call(rbind,df)
  }
  return(df)
  # head(df)
  # var=sum((df[,"msa.est"]-df[,"counties.est"])^2)
  # return(c("msa"=msa,"counties"=length(counties.subset), "completeness"= mean(completeness[,msa]),"variance"=var))
})
m=do.call(rbind,m)
# dim(m) 
# head(m)
var=mean((m[,"msa.est"]-m[,"counties.est"])^2)
print(var)
# qplot(m[,"msa.est"],m[,"counties.est"],geom="point") 
