# https://www.cdc.gov/mmwr/volumes/71/wr/mm7125a2.htm
# using 2019 data 

commercial.lab.tests = sum(9178836,2713628)
# assuming these two commercial labs are quest and labcorp, they conduct 45% of tests: 
              # https://foundersib.com/2018/05/30/labs-diagnostics-news-labcorp-quest/
total.tests = commercial.lab.tests/0.45
total.new.diagnoses = 36940
total.positivity = total.new.diagnoses/total.tests

cdc.tests = sum(1752586,632757)
cdc.new.diagnoses = sum(5374,3556)
cdc.positivity = cdc.new.diagnoses/cdc.tests

cdc.to.total.positivity.ratio = cdc.positivity/total.positivity # 2.68

## figure out uncertainty of this estimate; need uncertainty around 0.45 estimate first - this dominates our uncertainty 
