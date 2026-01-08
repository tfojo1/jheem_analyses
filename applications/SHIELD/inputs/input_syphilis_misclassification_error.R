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
#true EL cases
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
