

# 2021 data
# From https://www.cdc.gov/hiv-data/program/index.html
# The latest report should be accessible through this link:
#   https://www.cdc.gov/hiv-data/program/index.html

# From Appendix Table 4

n.tests.by.age = c(
    '0-12 years' = 2910,
    '13-19 years' = 88767,
    '20-29 years' = 583150,
    '30-39 years' = 466054,
    '40-49 years' = 257568,
    '50+ years' = 317968
)



# 2022 data
# https://stacks.cdc.gov/view/cdc/162351
# Appendix B, Table 1

n.tests.by.age = c(
    '0-12 years' = 3235,
    '13-19 years' = 91032,
    '20-29 years' = 577241,
    '30-39 years' = 487231,
    '40-49 years' = 267269,
    '50+ years' = 320711
)


restratified.n.tests = restratify.age.counts(n.tests.by.age, 
                                             desired.age.brackets = c('18-24 years','13-24 years','18-19 years', '15-19 years'),
                                             smooth.infinite.age.to = 101)


p.18_24.of.13_24 = restratified.n.tests['18-24 years'] / restratified.n.tests['13-24 years'];p.18_24.of.13_24

p.18_19.of.15_19 = restratified.n.tests['18-19 years'] / restratified.n.tests['15-19 years'];p.18_19.of.15_19
