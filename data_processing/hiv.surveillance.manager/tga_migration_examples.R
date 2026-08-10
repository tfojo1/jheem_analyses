# A couple of examples of calling tga_migration.R to get TGA immigration/
# emigration as a long-form data frame (outcome / year / location / value).
# Just examples to get started -- adapt as you see fit.

source('data_processing/hiv.surveillance.manager/tga_migration.R')

# -- one TGA, one window --------------------------------------------------
df <- area.migration.long.form("TGA.OAKLAND",
                               window = "2016.2020", year.label = "2016-2020")
df

# That df should be ready to put, roughly something like:
# data.manager$put.long.form(
#   data = df,
#   ontology.name = 'census.immigration',
#   source = 'census.population',
#   url = 'https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html',
#   details = 'Census County-to-County Migration Flows, aggregated to TGA')

# -- a few windows (and/or areas) at once ---------------------------------
# `file` is the workbook token; `label` is what gets stored under `year`.
tga.areas <- c("TGA.OAKLAND")
windows <- data.frame(file  = c("2006.2010", "2011.2015", "2016.2020"),
                      label = c("2006-2010", "2011-2015", "2016-2020"))

df.all <- do.call(rbind, lapply(seq_len(nrow(windows)), function(i)
  do.call(rbind, lapply(tga.areas, function(area)
    area.migration.long.form(area, windows$file[i], windows$label[i])))))
df.all

# ...then put df.all the same way.
