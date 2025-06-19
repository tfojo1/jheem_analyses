# MSA data on syphilis diagnosis from 1993-1999. 
# National data on diagnosis for 1941 - 2022 . 
# how can we estimate local data for the period 1941-1993?
    
# NAtional level data available from 1941 - 2022 
total=SURVEILLANCE.MANAGER$data$total.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'US']
ps=SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'US']
early=SURVEILLANCE.MANAGER$data$early.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'US']
late=SURVEILLANCE.MANAGER$data$unknown.duration.or.late.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'US']
cong=SURVEILLANCE.MANAGER$data$congenital.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'US']
# the total includes congenital syphilis as well
ps/total +early/total +late/total+cong/total

#non congenital total:
total=total-cong
{year <- 1941:2022
    plot(year, total, type = "l", col = "black", xlab = "Year", ylab = "Number of Diagnoses",
         main = "Syphilis diagnoses in the US, 1941-2022")
    # Add other lines
    lines(year, ps, col = "blue")
    lines(year, early, col = "red")
    lines(year, late, col = "green")
}

# if we have the value in 1993, how can we map the previous periods based on that? 
ref.year=1997
rel.est= ps/ps[which(year==ref.year)]

#original data
plot(year, ps, type = "l", col = "black", xlab = "Year", ylab = "Number of Diagnoses",
     main = "Syphilis PS diagnoses in the US, 1941-2022")
#estiamted trend:
lines(year, rel.est * ps[which(year==ref.year)], col = "red")

# What proportion of non-congenital syphilis diagnosis are in ps and early stages?
# rel.ps= ps/(total-cong)
# rel.early= early/(total-cong)


#how to map this on MSAs: 
# data from 1993-1999
ps.c12580=SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'C.12580']
# 
est.c12580=rel.est * ps.c12580[which(year==ref.year)]

#
plot(year, ps.c12580, type = "l", col = "black", 
     ylim=c(0,2000),
     xlab = "Year", ylab = "Number of Diagnoses",
     main = "Syphilis diagnoses in the US, 1941-2022")
# Add other lines
lines(year, est.c12580, col = "red")


##########
#approach 2: 
# take average ratio between local and nationa data and apply in previous periods
# Subset overlapping years
years_overlap <- 1993:1999
local_overlap <- ps.c12580[year %in% years_overlap]
national_overlap <- ps[year %in% years_overlap]
# Calculate yearly ratio
ratio <- local_overlap / national_overlap
# Use average ratio or model the ratio
mean_ratio <- mean(ratio, na.rm = TRUE)
# Subset national data for 1941–1992
national_est <- ps 
# Estimate local counts
local_est <- national_est * mean_ratio
# Plot to compare:
plot(year, ps.c12580, type = "l", col = "black", 
     ylim=c(0,2000),
     xlab = "Year", ylab = "Number of Diagnoses",
     main = "Syphilis diagnoses in the US, 1941-2022")
lines(1941:2022,local_est, col = "red")

