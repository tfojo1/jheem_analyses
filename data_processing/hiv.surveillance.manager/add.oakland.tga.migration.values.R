#Note: If this fails you may have to update the locations package.

source('data_processing/hiv.surveillance.manager/tga_migration.R')

# -- Pull Oakland --------------------------------------------------

oakland.tga.1 <- area.migration.long.form("TGA.OAKLAND",
                                        window = "2005.2009", year.label = "2005-2009", census.manager = census.manager)

oakland.tga.2 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2006.2010", year.label = "2006-2010", census.manager = census.manager)

oakland.tga.3 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2007.2011", year.label = "2007-2011", census.manager = census.manager)

oakland.tga.4 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2008.2012", year.label = "2008-2012", census.manager = census.manager)

oakland.tga.5 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2009.2013", year.label = "2009-2013", census.manager = census.manager)

oakland.tga.6 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2010.2014", year.label = "2010-2014", census.manager = census.manager)

oakland.tga.7 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2011.2015", year.label = "2011-2015", census.manager = census.manager)

oakland.tga.8 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2012.2016", year.label = "2012-2016", census.manager = census.manager)

oakland.tga.9 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2013.2017", year.label = "2013-2017", census.manager = census.manager)

oakland.tga.10 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2014.2018", year.label = "2014-2018", census.manager = census.manager)

oakland.tga.11 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2015.2019", year.label = "2015-2019", census.manager = census.manager)

oakland.tga.12 <- area.migration.long.form("TGA.OAKLAND",
                                          window = "2016.2020", year.label = "2016-2020", census.manager = census.manager)

oakland.all.migration <- rbind(oakland.tga.1, oakland.tga.2, oakland.tga.3, oakland.tga.4, oakland.tga.5, oakland.tga.6,
                               oakland.tga.7, oakland.tga.8, oakland.tga.9, oakland.tga.10, oakland.tga.11, oakland.tga.12)

# Put ---------------------------------------------------------------------
#immigration and emigration are 5-year totals; adult.immigration and adult.emigration (13+)
#are estimated from them with the census age distribution of the TGA's counties (see tga_migration.R)
surveillance.manager$put.long.form(
    data = oakland.all.migration[oakland.all.migration$outcome %in% c('immigration', 'emigration'), ],
    ontology.name = 'census.immigration',
    source = 'census.population',
    url = 'https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html',
    details = 'Census County-to-County Migration Flows, aggregated to TGA')

surveillance.manager$put.long.form(
    data = oakland.all.migration[oakland.all.migration$outcome %in% c('adult.immigration', 'adult.emigration'), ],
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    url = 'https://www.census.gov/topics/population/migration/guidance/county-to-county-migration-flows.html',
    details = 'Census County-to-County Migration Flows, aggregated to TGA')
