
my.msas = get.all.for.type("CBSA")

for (msa in my.msas) {
  
  counties.this.msa = locations::get.contained.locations(my.msas, 'county')
  
  births.msas = syphilis.manager$pull(
    debug = T,
    outcome = "births",
    source = 'cdc.wonder.natality', 
    from.ontology.names = "cdc.fertility", 
    dimension.values = list(location = counties.this.msa), 
    keep.dimensions = c('year', 'race', 'ethnicity', 'age'),
    na.rm = T) 
  
  browser()
}
