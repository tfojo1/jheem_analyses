
#Read data frame 
df <- read.csv("cdc_diagnoses.csv")

#Set reference categories

df$Race <- relevel(factor(df$Race), ref = "all1")
df$Age <- relevel(factor(df$Age), ref = "all2")

#Logistic regression
oddsratio_model <- glm(Diagnoses ~ as.factor(Race) + as.factor(Age) + as.factor(Year), data = df, weights = Number, family = binomial(link = "logit"))
summary(oddsratio_model)
