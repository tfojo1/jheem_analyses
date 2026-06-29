### Estimating progression rate from untreated syphilis to CNS or tertiary
### Data OSLO study
### Returning victor of estimates used in the base perameter
### called by  shield_base_parameters.R
## ******************************* ******************************* ******************************* ******************************* ##
# From
# https://www.sciencedirect.com/science/article/pii/0021968155901399
# the pdf:
#   https://pdf.sciencedirectassets.com/273120/1-s2.0-S0021968100X02998/1-s2.0-0021968155901399/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjECsaCXVzLWVhc3QtMSJIMEYCIQCZSIYMMDcHniY%2FrlQqAfK23W48cskk1wFpgRQtgmN%2B9gIhAO8PIcZIpwkrzt0EtMkXJ4W%2BBNLoUnn1fpf1k4vLcFm%2FKrIFCCQQBRoMMDU5MDAzNTQ2ODY1Igz2zxvr89kBGsKGzssqjwXP2PaWrVU7faBKwy6GVqdMAJeRp1c97ShtH3xp9ovb49paE4sbdeQCNn%2BUb0Ly%2FrOrcRwBn%2BePrqQE75lX9Na262tY2lzGlyGPGEsz5UzT28QQHNUVY%2BrZa9hxA1AMwubriqKgoH7TemQjdiHIHf%2F0DTo5o9M2mT4cRbm8lURBMQ%2F5ONd%2Bl%2F13vjBL%2FnrdD5JU5DfFtjBai7M2HA0uuQ3JtpGtWWLGV41xUuD8PFvWmamRy7K3W6wZE7RKQj1FYGX7z5RyduSLHNz3avkkoTWn3O2XfLffrblszPgPZbLLlSh959bmi3FmOyKjZyYLoKcnHrv3jOfIgMf%2F%2BC0EiAiudDRSx8yBomJwyevpnUM%2BO4OPfHI%2B8B1w7PyYpfXWQ8mr1z%2FTvEpIJ%2Bupo%2F3g48MNSqpAizqg4PRrQHd6q2sSM0c%2BBFe5Ns4s4cOufj0eOBmQscpcHMaeV1yzJa5BqtBt%2BUNZffqMY0IKd7PM9wK1f7n%2FAmdT0lRWjvjTEoQCHqTIHcXD04v4dmgKwzvIQ54S8Jw3pwSC00dF4xoTw0ISKCj8mIqsZh2ThIaupIXCTAPdkxKymhfGdvDWh4AaCZSiyzkoCQXWi3irED7ghaTgPk8HfKzNXIGr6H4HdaakU2%2BwyB5LWgU%2BiIEL8ZqDYcOXCadp4RPbD4p2USmlpEBcjcIZh2BTqMaA67LVLm3zSw3f%2FGTv2unzl0BigY1%2BVDg%2Bb3RZZua170xNhi9NwYiTwuXniQWhZONU6Jy9Eo7sso53mCY1eoWuib%2BXIa95CrQpH%2FE4HO4PA2F5lzuYK3Jrxwcu5fMq7%2BOys%2Fckbg1wi%2FFKu1yFNOcnEVbR%2B0UtGEQbLltkMijCqfDEZhv9Tu8AMKDHnLwGOrABFryDi%2B66%2FM9EWDa6ajcsBiV5qpBsN1Ao6jYFkCD1e4KwYMz7pAxqLjcknovpUR8oYTz358GyJp%2B8aRqlJJF%2FemwKCLufxOzybsjrCFWSYYkAw0lVXGoCoTuSU9oUkhie2aLt28o%2FitqKaKB4LHuKtgu%2FeMIG4RdUp3mdbPJJzu0aHcFS1mLVjiwRe%2Bd9eksAfzAoshU3lWOQkbvRRd1RrkrV4JCtrZk9C%2BKROflL85w%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20250115T040536Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYW4SYPWKZ%2F20250115%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=bf5c7c4735f4c6372322836510507ab3dd4825d5f5fc488ab01bcd6753cc289b&hash=72a6c4f4061ca864f68f8ba09a69c8bc31bee37328e9d4b716a826ea92231288&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=0021968155901399&tid=spdf-dec051e8-4525-49fa-82fa-71863c684ec8&sid=5f3ae68722c55640f968fcc22ce757619d47gxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=161c5856575304075e&rr=9022fd060fcaf283&cc=us

ESTIMATES = list()

#-- TERTIARY:  BENIGN LATE SYPHILIS --# ----

# From Table 7: cumulative percent developing benign late syphilis by the end of the 15th, 30th, and 35th years
male.p.benign.late = c('15'=10.9, '30'=15.7, '35'=16.4)/100
female.p.benign.late = c('15'=15.9, '30'=19.8, '35'=21.4)/100
times.p.benign.late = as.numeric(names(male.p.benign.late))

# rates for men:
# -log(1-male.p.benign.late)/times.p.benign.late
fit.male.p.benign.late = lm(-log(1-male.p.benign.late)~ times.p.benign.late + 0) # using a linear regression model (lm) (The + 0 in the formula means you're not including an intercept term in the model (you want the regression to pass through the origin).)
ESTIMATES$rate.male.benign.late.est = as.numeric(fit.male.p.benign.late$coefficients[1]) #extracting the coefficient (slope) from the model fit, which represents the rate
ESTIMATES$rate.male.benign.late.se = sqrt(as.numeric(vcov(fit.male.p.benign.late))) #extracting the variance-covariance matrix of the model and taking the square root to get the standard error
# women for women:
-log(1-female.p.benign.late)/times.p.benign.late
fit.female.p.benign.late = lm(-log(1-female.p.benign.late)~times.p.benign.late + 0)
ESTIMATES$rate.female.benign.late.est = as.numeric(fit.female.p.benign.late$coefficients[1])
ESTIMATES$rate.female.benign.late.se = sqrt(as.numeric(vcov(fit.female.p.benign.late)))

print(ESTIMATES)

#-- TERTIARY: CARDIOVASCULAR SYPHILIS --# ----

# from Fig9/Table10: proportion of each agegroup developled some type of cardiovascular involvement
# the limitation here is the average time of 35 years is not representative of all the events 
cardiovascular.male.15older=0.149
cardiovascular.female.15older=0.08
n.male=303
n.female=584
average.time=35 #years Fig10

# point estimate: lambda = -log(1-p)/t
ESTIMATES$rate.male.cardiovascular.15older.est = -log(1-cardiovascular.male.15older)/average.time #MALE
ESTIMATES$rate.female.cardiovascular.15older.est = -log(1-cardiovascular.female.15older)/average.time #FEMALE

# Delta method SE for lambda = -log(1-p)/t
# d(lambda)/dp = 1/((1-p)*t)
# SE_lambda = SE_p * |d(lambda)/dp| = sqrt(p(1-p)/n) / ((1-p)*t)
ESTIMATES$rate.male.cardiovascular.15older.se   <- sqrt(cardiovascular.male.15older   *
                                                            (1 - cardiovascular.male.15older)   / n.male)   / ((1 - cardiovascular.male.15older)   * average.time)
ESTIMATES$rate.female.cardiovascular.15older.se <- sqrt(cardiovascular.female.15older * 
                                                            (1 - cardiovascular.female.15older) / n.female) / ((1 - cardiovascular.female.15older) * average.time)

print(ESTIMATES)
# we also tried to break down uncomplicated versus complicated cardiovascular and approximate the mean year duration to its development from figure 10 the final results were very similar
# MALE
# cardiovascular.male.outcomes = data.frame(
#     # from table 8: PROPORTION OF "KNOWNS" OBSERVED TO HAVE DEVELOPED CARDIOVASCULAR SYPHILIS  
#     p = c(uncomplicated.aortitis=2.6/100,
#           total.complicated=12.3/100),
#     # from figure 10 (digitized): Duration of infection at discovery, in mean years
#     mean.time = c(uncomplicated.aortitis=28.9,
#                   total.complicated=30.7)
# )
# # FEMALE
# cardiovascular.female.outcomes = data.frame(
#     # from table 8
#     p = c(uncomplicated.aortitis=2.9/100,
#           total.complicated=5.1/100),
#     # from figure 10 (digitized)
#     mean.time = c(uncomplicated.aortitis=23.7,
#                   total.complicated=32.8)
# )
#approach1-: rate of transition is 1/duration, then weighted based on probabiliry of event occuring (effective rate)
# assuming that the risk of event is uniform over time:
# cardiovascular.male.rates.among.those.with.outcome = 1/cardiovascular.male.outcomes$mean.time
# cardiovascular.male.rates = cardiovascular.male.rates.among.those.with.outcome * cardiovascular.male.outcomes$p

#appraoch2: using -log(1-p)/time to get the rate 
# assuming that the rate of event is fix over time (but the risk of event is not): the time to event follows a logistic or exponential distribution, where the event’s likelihood increases over time in a non-linear way.
# cardiovascular.male.rates = -log(1- cardiovascular.male.outcomes$p)/cardiovascular.male.outcomes$mean.time
# names(cardiovascular.male.rates) = dimnames(cardiovascular.male.outcomes)[[1]]
# ESTIMATES$rate.male.cardiovascular.est = as.numeric(cardiovascular.male.rates['uncomplicated.aortitis'] + cardiovascular.male.rates['total.complicated'])
# 
# cardiovascular.female.rates = -log(1- cardiovascular.female.outcomes$p)/cardiovascular.female.outcomes$mean.time
# names(cardiovascular.female.rates) = dimnames(cardiovascular.female.outcomes)[[1]]
# ESTIMATES$rate.female.cardiovascular.est = as.numeric(cardiovascular.female.rates['uncomplicated.aortitis'] + 
#                                                           cardiovascular.female.rates['total.complicated'])

#-- TERTIARY: COMBINED --# -----
# assuming that the rates are independant, we can sum the means and variances 
# --- 95% CI range for teriary rates (normal approximation) # Half-width = 1.96 * SE ---
rate.male.tertiary.late.est <- ESTIMATES$rate.male.benign.late.est + ESTIMATES$rate.male.cardiovascular.15older.est
rate.male.tertiary.late.95hw <- 1.96 * sqrt(ESTIMATES$rate.male.benign.late.se^2 + ESTIMATES$rate.male.cardiovascular.15older.se^2)
ESTIMATES$rate.male.tertiary.late.range <- c(
    est   = rate.male.tertiary.late.est,
    lower = rate.male.tertiary.late.est - rate.male.tertiary.late.95hw,
    upper = rate.male.tertiary.late.est + rate.male.tertiary.late.95hw
)

rate.female.tertiary.late.est <- ESTIMATES$rate.female.benign.late.est + ESTIMATES$rate.female.cardiovascular.15older.est
rate.female.tertiary.late.95hw <- 1.96 * sqrt(ESTIMATES$rate.female.benign.late.se^2 + ESTIMATES$rate.female.cardiovascular.15older.se^2)
ESTIMATES$rate.female.tertiary.late.range <- c(
    est   = rate.female.tertiary.late.est,
    lower = rate.female.tertiary.late.est - rate.female.tertiary.late.95hw,
    upper = rate.female.tertiary.late.est + rate.female.tertiary.late.95hw
)

print(ESTIMATES)
sapply(ESTIMATES,round,4)

#-- NEUROSYPHILIS ----

# Neurosyphilis progression rates from Oslo cohort 
# Sub-outcomes treated as mutually exclusive by design (Table 9: sub-outcome proportions sum exactly to Oslo totals: men 9.4%, women 5.0%)
# n = 331 males, n = 622 females (full followed cohort, Table 9 denominators)
# Mean times to diagnosis digitized from Figure 12

# --- MALE ---
n.male.neuro <- 331
neurosyphilis.male.outcomes <- data.frame(
    p = c(diffuse.meningovascular = 3.6/100,
          general.paresis         = 3.0/100,
          tabes.dorsalis          = 2.5/100),
    mean.time = c(diffuse.meningovascular = 14.9,
                  general.paresis         = 25.4,
                  tabes.dorsalis          = 28.8)
)

# Point estimates: lambda = -log(1-p) / t
neurosyphilis.male.rates <- -log(1 - neurosyphilis.male.outcomes$p) / 
    neurosyphilis.male.outcomes$mean.time
names(neurosyphilis.male.rates) <- rownames(neurosyphilis.male.outcomes)

# Delta method SE per sub-outcome: SE_lambda = sqrt(p(1-p)/n) / ((1-p) * t)
neurosyphilis.male.se <- sqrt(neurosyphilis.male.outcomes$p * 
                                  (1 - neurosyphilis.male.outcomes$p) / n.male.neuro) /
    ((1 - neurosyphilis.male.outcomes$p) * 
         neurosyphilis.male.outcomes$mean.time)
names(neurosyphilis.male.se) <- rownames(neurosyphilis.male.outcomes)

# Total: sum rates, combine SEs under independence (sqrt of sum of squares)
ESTIMATES$rate.male.neurosyphilis.est <- sum(neurosyphilis.male.rates)
ESTIMATES$rate.male.neurosyphilis.se  <- sqrt(sum(neurosyphilis.male.se^2))

# --- FEMALE ---
n.female.neuro <- 622
neurosyphilis.female.outcomes <- data.frame(
    p = c(diffuse.meningovascular = 1.7/100,
          general.paresis         = 1.7/100,
          tabes.dorsalis          = 1.4/100),
    mean.time = c(diffuse.meningovascular = 18.8,
                  general.paresis         = 19.6,
                  tabes.dorsalis          = 30.7)
)

# Point estimates
neurosyphilis.female.rates <- -log(1 - neurosyphilis.female.outcomes$p) / 
    neurosyphilis.female.outcomes$mean.time
names(neurosyphilis.female.rates) <- rownames(neurosyphilis.female.outcomes)

# Delta method SE per sub-outcome
neurosyphilis.female.se <- sqrt(neurosyphilis.female.outcomes$p * 
                                    (1 - neurosyphilis.female.outcomes$p) / n.female.neuro) /
    ((1 - neurosyphilis.female.outcomes$p) * 
         neurosyphilis.female.outcomes$mean.time)
names(neurosyphilis.female.se) <- rownames(neurosyphilis.female.outcomes)

# Total
ESTIMATES$rate.female.neurosyphilis.est <- sum(neurosyphilis.female.rates)
ESTIMATES$rate.female.neurosyphilis.se  <- sqrt(sum(neurosyphilis.female.se^2))


# --- 95% CI range for neurosyphilis rates (normal approximation) ---
# Half-width = 1.96 * SE, consistent with tertiary rate block

ESTIMATES$rate.male.neurosyphilis.range <- c(
    est   = ESTIMATES$rate.male.neurosyphilis.est,
    lower = ESTIMATES$rate.male.neurosyphilis.est - 1.96 * ESTIMATES$rate.male.neurosyphilis.se,
    upper = ESTIMATES$rate.male.neurosyphilis.est + 1.96 * ESTIMATES$rate.male.neurosyphilis.se
)

ESTIMATES$rate.female.neurosyphilis.range <- c(
    est   = ESTIMATES$rate.female.neurosyphilis.est,
    lower = ESTIMATES$rate.female.neurosyphilis.est - 1.96 * ESTIMATES$rate.female.neurosyphilis.se,
    upper = ESTIMATES$rate.female.neurosyphilis.est + 1.96 * ESTIMATES$rate.female.neurosyphilis.se
)
print(ESTIMATES)
sapply(ESTIMATES,round,4)

#-- CHECK --#

# MALE
# from page 339
# male.with.outcome = c(
#     infection.relapse = 22.7/100,
#     benign.late = 14.4/100,
#     cardiovascular = 13.6/100, 
#     neurosyphilis = 9.4/100,
#     death = 15/100
# )
# 
# 
# # FEMALE
# # from page 32
# female.with.outcome = c(
#     infection.relapse = 24/100,
#     benign.late = 16.7/100,
#     cardiovascular = 7.6/100,
#     neurosyphilis = 5.0/100,
#     death = 8/100
# )
# 
# years.for.check.male = 29
# years.for.check.female = 26
# cbind(
#     observed.male = male.with.outcome[c('benign.late','cardiovascular','neurosyphilis')],
#     projected.male = round(1-exp(-as.numeric(ESTIMATES[c('rate.male.benign.late.est','rate.male.cardiovascular.est','rate.male.neurosyphilis.est')]) * years.for.check.male),3),
#     observed.female = female.with.outcome[c('benign.late','cardiovascular','neurosyphilis')],
#     projected.female = round(1-exp(-as.numeric(ESTIMATES[c('rate.female.benign.late.est','rate.female.cardiovascular.est','rate.female.neurosyphilis.est')]) * years.for.check.female),3)
#     
# )

