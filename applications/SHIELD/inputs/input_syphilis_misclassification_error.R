### Estimating the % of EL cases that are misclassified as LL/Unknown and LL/U cases misclassified as EL
### Data from a manuscript (see below)

# Misclassification of the Stages of Syphilis: Implications for Surveillance
# https://journals.lww.com/stdjournal/fulltext/2005/03000/A_Randomized,_Comparative_Pilot_Study_of.2.aspx?casa_token=bsLapfDIFtsAAAAA:fqDn-YMkzY9qasYsfSke0N8QCVQlon6msOKbyv5N0-nbSQSJDVjXe4CjHUUDrjiTnHt9KVm6QiclzHarN0Dk2qI

# TABLE 1: Stage of Syphilis as Classified by the Sites and Reclassified by the Authors Using the CDC Case Definition
df<-as.data.frame(rbind(
  c(150, 94, 0, 3.3, 0, 0, 2.7),
  c(218, 0, 95.4, 2.8, 0.5, 0.5, 0.5),
  c(250, 2.8, 1.6, 48.4, 14.4, 31.6, 1.2),
  c(143, 0, 0, 4.2, 49.7, 41.3, 4.9),
  c(212, 0.5, 1.4, 3.8, 2.4, 80.2, 11.8))
)
names(df)<-c("n","primary","secondary","el","unkown","ll","notSyphilis") 
rownames(df)<-c("primary","secondary","el","unkown","ll")
df
# rowSums(df[,2:7])

#add ll and unknown together
df$llu=df$ll+df$unkown;df

# EL TO LL/UNKOWN
# true EL cases
true.el<-sum(df[,"el"]*df[,"n"]/100);true.el 
#correctly classified el cases as el
el.as.el<-df[3,"el"]*df[3,"n"]/100;el.as.el 
# misclassified el cases as not el
misclassified.el<-(true.el - el.as.el);misclassified.el
# misclassified el cases as ll or unkown
misclassified.el.as.llu<-sum(df[4:5,"el"]*df[4:5,"n"]/100);misclassified.el.as.llu 
# % miclassification el to llu
percent.misclassified.el.as.llu<-misclassified.el.as.llu/true.el*100;percent.misclassified.el.as.llu


# LL/UNKOWN TO EL
#true LL Unkown cases 
true.ll<-sum(df[,"ll"]*df[,"n"]/100);true.ll
true.un<-sum(df[,"unkown"]*df[,"n"]/100);true.un
true.llu= true.ll+true.un;true.llu
#correctly classified ll and unknown
llu.as.llu<-sum(df[4:5,"llu"]*df[4:5,"n"]/100);llu.as.llu
# misclassified llu as other stages
misclassified.llu= true.llu- llu.as.llu; misclassified.llu
# misclassified llu as el
misclassified.llu.as.el<-df[3,"llu"]*df[3,"n"]/100;misclassified.llu.as.el
# %misclassified llu as el
percent.misclassified.llu.as.el<-misclassified.llu.as.el/true.llu*100;percent.misclassified.llu.as.el

print(paste("percent.misclassified.el.as.llu = ",percent.misclassified.el.as.llu))
print(paste("percent.misclassified.llu.as.el = ", percent.misclassified.llu.as.el))

# return(list("percent.misclassified.el.as.llu"=percent.misclassified.el.as.llu/100,
# "percent.misclassified.llu.as.el"= percent.misclassified.llu.as.el/100))

# # Table 2: Percent Agreement Between the Local Stage and the CDC Stage Definitions, by Site
# # % agreement is the positive predictive value (PPV) — of cases reported as EL, what proportion truly met the CDC EL definition. So:
# # 
# df2 <- as.data.frame(rbind(
#     Chicago = c(100.0, 100.0, 50.0, 12.9, 80.0),
#     Puerto_Rico = c(71.4, 87.5, 51.8, NA, 86.1),
#     Washington_DC = c(100.0, 90.9, 26.3, 23.1, 66.7),
#     New_York = c(96.8, 100.0, 23.7, 17.2, 71.4),
#     San_Diego = c(88.2, 100.0, 85.7, 85.7, 84.6),
#     Los_Angeles = c(100.0, 100.0, 68.8, 82.9, 91.7)
# ))
# df2
# # Rename columns for display
# names(df2) <-c("Primary", "Secondary", "Early latent", "Unknown duration", "Late latent")
# 
# # PPV for EL:
# el_agreement <- df2[,"Early latent"]
# el_misclass <- 100 - el_agreement;el_misclass
# #
# mean(el_misclass)      # central estimate ~48.8%
# sd(el_misclass)        # ~23%
# range(el_misclass)     # 14 to 76%
# # > almost half of reported EL-diagnosis were not truely EL (according to CDC) and came from other stages
# 
# # PPV for LL:
# ll_agreement <- df2[,"Late latent"]
# ll_misclass <- 100 - ll_agreement;ll_misclass
# #
# mean(ll_misclass)      # central estimate ~19.9%
# sd(ll_misclass)        # ~8.8%
# range(ll_misclass)     # 8.3% to 33.3%
# # > 20% of reported LL cases were not truely LL
# 
# #######################
# 
# # Reported vs adjusted rates from Table 4
# reported_el <- c(14.2, 21.0, 33.6, 9.1, 1.2, 3.9)
# adjusted_el <- c(7.6,  11.8, 11.3, 3.8, 1.2, 2.7)
# 
# reported_ll <- c(4.3,  14.3, 17.8, 28.4, 2.5, 9.0)
# adjusted_ll <- c(11.0, 18.7, 37.9, 24.5, 2.2, 9.0)
# 
# sites <- c("Chicago", "Puerto Rico", "Washington DC", 
#            "New York", "San Diego", "Los Angeles")
# 
# # Proportion of True EL Misclassified as LL
# shifted_el_to_ll <- reported_el - adjusted_el
# prop_el_misclass_as_ll <- shifted_el_to_ll / adjusted_el * 100
# 
# data.frame(
#     Site = sites,
#     Reported_EL = reported_el,
#     Adjusted_EL = adjusted_el,
#     Shifted = shifted_el_to_ll,
#     Pct_Misclassified = round(prop_el_misclass_as_ll, 1)
# )
# 
# mean(prop_el_misclass_as_ll)
# sd(prop_el_misclass_as_ll)
# range(prop_el_misclass_as_ll)