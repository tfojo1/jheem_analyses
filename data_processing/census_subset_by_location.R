# Takes the full census manager and subsets it to only include the "counties of
# interest" as specified in jheem_analyses/commoncode/locations_of_interest.R

library(locations)
source('commoncode/locations_of_interest.R')
COUNTIES.OF.INTEREST <- unlist(COUNTIES.OF.INTEREST)

census.manager.filepath = "cached/census.manager.rdata"

load(census.manager.filepath)

smaller.census.manager = census.manager$subset.data(dimension.values = list(location = COUNTIES.OF.INTEREST))

save(census.manager, file="cached/smaller.census.manager.rdata")

# NOTES: Get the COUNTIES.OF.INTEREST from jheem_analyses/commoncode/lcations.of.interest.R
# Change these filepaths based on what the current working directory is.
# Also, find a better name for the new census manager if desired