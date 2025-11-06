
# from https://www.cdc.gov/mmwr/volumes/71/wr/mm7125a2.htm#T1_down

all.us.tests = 9178836+2713628+1752586+632757
cdc.funded.tests = 1752586+632757

all.us.diagnoses = 36940
cdc.funded.diagnoses = 5374+3556

print(paste0("fraction of tests funded by CDC = ", floor(cdc.funded.tests/all.us.tests*100), "%"))

print(paste0("fraction of diagnoses funded by CDC = ", floor(cdc.funded.diagnoses/all.us.diagnoses*100), "%"))
