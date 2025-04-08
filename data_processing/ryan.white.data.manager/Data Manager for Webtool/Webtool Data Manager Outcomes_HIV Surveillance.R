#This code pulls data from the HIV Surveillance Manager (JHEEM) to create the Ryan White Webtool Data Manager

#Outcomes include:

# FROM HIV SURVEILLANCE MANAGER
# diagnoses
# diagnosed.prevalence
# prep
# suppression
# proportion.tested
# proportion.tested.n
# awareness
# total.prevalence

surveillance.manager = load.data.manager("Q:/data_managers/surveillance.manager.rdata")
