
# Finding the MSAs that account for 70% of PS syphilis diagnosis among adults in 2023
# total diag
# x=sort(SURVEILLANCE.MANAGER$data$total.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2023",],decreasing = T)

# ps diagnosis
x=sort(SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location["2023",],decreasing = T);x
y=cumsum(x)/sum(x);y
#
msas<-y[y<.6]
length(msas)
msa.codes= names(msas);
msa.names=(locations::get.location.name(msa.codes))
as.list(msa.names)

msa.codes[!msa.codes %in% MSAS.OF.INTEREST]
MSAS.OF.INTEREST[!MSAS.OF.INTEREST %in% msa.codes ]
# msa_names=names(msas)[!names(msas) %in% MSAS.OF.INTEREST]



