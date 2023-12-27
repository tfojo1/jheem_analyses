
parse.and.put.immigrantion.emigration.all.locations <- function(dir,
                                                                data.manager)
{
#    filenames = list.files(...)
    filenames = c(
        "immigration.total.2011.2015.csv",
        "emigration.total.2011.2015.csv",
        "immigration.race.2011.2015.csv",
        "immigration.age.2011.2015.csv",
        "immigration.ethnicity.2011.2015.csv"
    )
    
    dfs = lapply(filenames, function(file){
        read.csv(file)
    })
    
    split.up.filenames = strsplit(filenames, split='\\.')
    type.per.file = sapply(split.up.filenames, function(split){split[1]})
    stratification.per.file = sapply(split.up.filenames, function(split){split[2]})
    year.range.per.file = sapply(split.up.filenames, function(split){
        paste0(split[3], "-", split[4])
        })
    
    for (type in c('immigration','emigration'))
    {
        print(paste0("Now we are dealing with ", type))
        years.for.type = unique(year.range.per.file[type.per.file==type])
        for (year in years.for.type)
        {
            df.total = dfs[type.per.file==type & year.range.per.file==year & stratification.per.file=='total'][[1]]
            df.age = dfs[type.per.file==type & year.range.per.file==year & stratification.per.file=='age'][[1]]
            df.race = dfs[type.per.file==type & year.range.per.file==year & stratification.per.file=='race'][[1]]
            df.ethnicity = dfs[type.per.file==type & year.range.per.file==year & stratification.per.file=='ethnicity'][[1]]
            
            all.locations = unique(df.total$location)
            for (location in all.locations)
            {
                parse.and.put.immigration.emigration.one.location(location = location,
                                                                  year = year,
                                                                  type = type,
                                                                  data.manager = data.manager,
                                                                  df.total = df.total[df.total$location==location,],
                                                                  df.age = df.age[df.age$location==location,],
                                                                  df.race = df.race[df.race$location==location,],
                                                                  df.ethnicity = df.ethnicity[df.ethnicity$location==location,])
            }
        }
    }
}

# Assume that df.total, etc each have data just for our one location
# Actually put the data to the data manager
parse.and.put.immigration.emigration.one.location <- function(location,
                                                              year,
                                                              type,
                                                              data.manager,
                                                              df.total,
                                                              df.age,
                                                              df.race,
                                                              df.ethnicity)
{
    # Split the age brackets as we want them: 13+
    # Put the age data
    
    # Calculate the percentage of the population that is 13 and up
    
    # Take the total, multiply it by the fraction 13 and up, put that as total for adults
    # Put the total
    
    # Clean up race
    # Take hispanic from df.ethnicity
    # Take black from df.race, and multiply by sqrt(p.black.of.hispanic * n.hispanic * p.hispanic.of.black * n.black)
    #   this becomes non-hispanic-black
    # Multiply hispanic and non-hispanic-black by fraction 13 and up
    # Calculate other as total minus hispanic minus non-hispanic black
    # Put the race data
}