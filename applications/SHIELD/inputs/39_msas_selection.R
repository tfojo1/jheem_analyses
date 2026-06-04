# Finding the MSAs that account for 60% of PS syphilis diagnosis among adults in 2023

# ps diagnosis
x=sort(SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2023",],decreasing = T);x
y=cumsum(x)/sum(x);y
#
msas<-y[y<.6]
length(msas)
msa.codes= names(msas);
msa.names=(locations::get.location.name(msa.codes))
 

