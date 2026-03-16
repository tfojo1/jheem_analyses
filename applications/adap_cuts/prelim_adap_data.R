

# From 2023 ADAP report

full.pay.income.counts.2023 = c(
    '0-100%' = 68953,
    '101-138%' = 8302,
    '139-250%' = 20856,
    '251-400%' = 9446,
    '>400%' = 1440
)

full.pay.income.dist.2023 = full.pay.income.counts.2023 / sum(full.pay.income.counts.2023)

premium.assistance.income.counts.2023 = c(
    '0-100%' = 3710,
    '101-138%' = 1229,
    '139-250%' = 4282,
    '251-400%' = 3117,
    '>400%' = 725
)

premium.assistance.income.dist.2023 = premium.assistance.income.counts.2023 / sum(premium.assistance.income.counts.2023)

copay.income.counts.2023 = c(
    '0-100%' = 7518,
    '101-138%' = 5822,
    '139-250%' = 15308,
    '251-400%' = 11554,
    '>400%' = 3458
)

copay.income.dist.2023 = copay.income.counts.2023 / sum(copay.income.counts.2023)

multiple.services.income.counts.2023 = c(
    '0-100%' = 17981,
    '101-138%' = 5931,
    '139-250%' = 23461,
    '251-400%' = 17830,
    '>400%' = 4025
)

multiple.services.income.dist.2023 = multiple.services.income.counts.2023 / sum(multiple.services.income.counts.2023)

full.pay.with.or.without.insurance.counts.2023 = full.pay.income.counts.2023 + multiple.services.income.counts.2023

full.pay.with.or.without.insurance.dist.2023 = full.pay.with.or.without.insurance.counts.2023 / sum(full.pay.with.or.without.insurance.counts.2023)

adap.insurance.income.counts.2023 = premium.assistance.income.counts.2023 + copay.income.counts.2023

adap.insurance.income.dist.2023 = adap.insurance.income.counts.2023 / sum(adap.insurance.income.counts.2023)


adap.income.counts.2023 = full.pay.with.or.without.insurance.counts.2023 + adap.insurance.income.counts.2023
adap.income.dist.2023 = adap.income.counts.2023 / sum(adap.income.counts.2023)
# adap.income.counts.2023 = c(
#     '0-100%' = 98162,
#     '101-138%' = 21284,
#     '139-250%' = 63907,
#     '251-400%' = 41947,
#     '>400%' = 9648
# ) / 234948

round(100*cbind(adap.income.dist.2023,
                full.pay.with.or.without.insurance.dist.2023,
                adap.insurance.income.dist.2023,
                full.pay.income.dist.2023,
                multiple.services.income.dist.2023,
                premium.assistance.income.dist.2023,
                copay.income.dist.2023))



fl.adap.income.dist.2023 = c(
    '0-100%' = 13742,
    '101-138%' = 3167,
    '139-250%' = 9992,
    '251-400%' = 5840,
    '>400%' = 84
) / 32825

adap.black.income.dist.2023 = c(
    '0-100%' = 53526,
    '101-138%' = 9711,
    '139-250%' = 25816,
    '251-400%' = 17860,
    '>400%' = 3997
) / 110910

adap.white.income.dist.2023 = c(
    '0-100%' = 28199,
    '101-138%' = 9566,
    '139-250%' = 26155,
    '251-400%' = 16548,
    '>400%' = 4238
) / 84706

adap.hispanic.income.dist.2023 = c(
    '0-100%' = 48140,
    '101-138%' = 8115,
    '139-250%' = 21397,
    '251-400%' = 12233,
    '>400%' = 2732
) / 93617

round(100*cbind(
    black = adap.black.income.dist.2023,
    white = adap.white.income.dist.2023,
    hispanic = adap.hispanic.income.dist.2023
))

p.full.pay.of.adap.black = c("0-400%" = 42069 / (42069 + 5479 + 14384)) #black
p.full.pay.of.adap.white = c("0-400%" = 15673 / (15673 + 4452 + 19611)) #white,
p.full.pay.of.adap.hispanic = c("0-400%" = 49347 / (49347 + 5479 + 14384)) #hispanic



medicaid.income.counts.2023 = c(
    '0-100%' = 26026,
    '101-138%' = 1713,
    '139-250%' = 3762,
    '251-400%' = 1676,
    '>400%' = 247
)

medicaid.income.distribution.2023 = medicaid.income.counts.2023 / sum(medicaid.income.counts.2023)


frac.on.medicaid.by.income = c(
    '0-100%' = .194+.049,
    '101-138%' = .061+.090,
    '139-250%' = .050+.023,
    '251-400%' = .035+.007,
    '>400%' = .022+.003
)

# From medical monitoring project

mmp.income.counts.2022 = c(
    '0-100%' = 1106,
    '101-138%' = 449,
    '139-399%' = 1200,
    '>400%' = 572
)
mmp.income.dist.2022 = mmp.income.counts.2022 / sum(mmp.income.counts.2022)


mmp.black.income.counts.2022 = c(
    '0-100%' = 485,
    '101-138%' = 169,
    '139-399%' = 453,
    '>400%' = 142
)
mmp.black.income.dist.2022 = mmp.black.income.counts.2022 / sum(mmp.black.income.counts.2022)

mmp.white.income.counts.2022 = c(
    '0-100%' = 193,
    '101-138%' = 134,
    '139-399%' = 410,
    '>400%' = 291
)
mmp.white.income.dist.2022 = mmp.white.income.counts.2022 / sum(mmp.white.income.counts.2022)

mmp.hispanic.income.counts.2022 = c(
    '0-100%' = 339,
    '101-138%' = 116,
    '139-399%' = 261,
    '>400%' = 92
)
mmp.hispanic.income.dist.2022 = mmp.hispanic.income.counts.2022 / sum(mmp.hispanic.income.counts.2022)