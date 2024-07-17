#Decide if you need to put the full population into the census manager
#or if we just need the adult.population for these

#This is stratified data, county level, census data, 2020-2023

#Estimate the adult.population from age grouped data
#Then find adult.population from the single year ages

#Total: county_agegr_sex_race_eth (estimated) ; single year ages
#Age: county_agegr_sex_race_eth (estimated) ; single year ages
#Race: county_agegr_sex_race_eth (estimated)
#Sex: county_agegr_sex_race_eth (estimated); single year ages
#Ethnicity? I think this will be combined with race? 

#The  'county_agegr_sex' file has total so I'll use that for total

#First start with estimates

#Manually read in files- update later:
 county_agegr_sex <- read.csv("C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/New Census Data June 2024/county_agegr_sex.csv")
 county_agegr_sex_race_eth <- read.csv("C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/New Census Data June 2024/county_agegr_sex_race_eth.csv")

# Total Adult population - Using restratify age function ------------------


