

x = load("Q:data_clean/brfss/brfss.subset.RData")

dim(appended)
head(appended)
table(appended$tested.past.year)


appended[appended$tested.past.year>0 & appended$tested.past.year<1,][1:10,]


fit = glm(tested.past.year ~ age + sex + race + year, data=appended)
fit = glm(tested.past.year ~ age + sex + race + msm + high.risk + year + year:age + year:sex + year:race + year:msm + year:high.risk, data=appended)

fit = glm(tested.past.year ~ age*sex*race*year, data=appended)



