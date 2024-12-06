#Use this code to merge the different saved sections of the census manager into a complete version

library(jheem2)

#Load the saved sections of the census manager
population1 = load.data.manager(name="census.manager_population1", file="../../cached/data.manager.merge/census.manager_population1.rdata")

population2 = load.data.manager(name="census.manager_population2", file="../../cached/data.manager.merge/census.manager_population2.rdata")

population3 = load.data.manager(name="census.manager_population3", file="../../cached/data.manager.merge/census.manager_population3.rdata")

births.deaths1 = load.data.manager(name="census.manager_births.deaths1", file="../../cached/data.manager.merge/census.manager_births.deaths1.rdata")

#Merge 

population1$import.data(population2) #This order doesnt matter, do it this way: big.one$importdata(smaller.one)

population1$import.data(births.deaths1) #this would be your next
