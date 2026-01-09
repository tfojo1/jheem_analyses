
# from https://www.cdc.gov/mmwr/volumes/71/wr/mm7125a2.htm#T1_down
# 2019 data


tests = c(cdc.total = 1752586+632757,
          cdc.healthcare = 1752586,
          cdc.community = 632757,
          other.federal = 2713628,
          commercial = 9178836,
          total = 9178836+2713628+1752586+632757)

diagnoses = c(cdc.total = 5374+3556,
          cdc.healthcare = 5374,
          cdc.community = 3556,
          other.federal = 7164,
          commercial = 36940-5374-3556-7164,
          total = 36940)

all.us.tests = tests['total']
cdc.funded.tests = tests['cdc.total']
other.federally.funded.tests = tests['other.federal']
commercial.tests = tests['commercial']

all.us.diagnoses = diagnoses['total']
cdc.funded.diagnoses = diagnoses['cdc.total']
other.federally.funded.diagnoses = diagnoses['other.federal']

print(paste0("fraction of tests funded by CDC = ", floor(cdc.funded.tests/all.us.tests*100), "%"))

print(paste0("fraction of diagnoses funded by CDC = ", floor(cdc.funded.diagnoses/all.us.diagnoses*100), "%"))

print("")
print(paste0("CDC-funded positivity = ", round(100*cdc.funded.diagnoses/cdc.funded.tests,2), '%'))
print(paste0("Other federally funded positivity = ", round(100*other.federally.funded.diagnoses/other.federally.funded.tests,2), '%'))
print(paste0("Max commercial positivity = ", round(100*(all.us.diagnoses-cdc.funded.diagnoses-other.federally.funded.diagnoses)/commercial.tests,2), '%'))


tests / tests['total']
diagnoses / diagnoses['total']
diagnoses / tests

# The correction for other commercial labs

quest.labcorp.fraction.of.total = 0.45
    # https://foundersib.com/2018/05/30/labs-diagnostics-news-labcorp-quest/

estimated.total.tests = as.numeric(round(tests['commercial'] / quest.labcorp.fraction.of.total))
estimated.other.tests = as.numeric(estimated.total.tests - tests['total'])
adjusted.tests = c(
    tests[c('cdc.total','cdc.healthcare','cdc.community','other.federal')],
    quest.labcorp.other = as.numeric(tests['commercial'] + estimated.other.tests),
    quest.labcorp = as.numeric(tests['commercial']),
    est.other = estimated.other.tests,
    est.total = estimated.total.tests
)

adjusted.diagnoses = c(
    diagnoses[c('cdc.total','cdc.healthcare','cdc.community','other.federal')],
    quest.labcorp.other = as.numeric(diagnoses['commercial']),
    quest.labcorp = NA,
    est.other = NA,
    est.total = as.numeric(diagnoses['total'])
)

adjusted.tests
adjusted.diagnoses
adjusted.tests / adjusted.tests['est.total']
adjusted.diagnoses / adjusted.diagnoses['est.total']
adjusted.diagnoses / adjusted.tests

