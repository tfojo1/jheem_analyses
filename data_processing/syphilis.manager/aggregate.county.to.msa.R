
my.msas = get.all.for.type("CBSA")

for (msa in my.msas) {
  
  counties.this.msa = locations::get.contained.locations(msa, 'county')
  
  births.msas = syphilis.manager$pull(
    outcome = "births",
    source = 'cdc.wonder.natality', 
    from.ontology.names = "cdc.fertility", 
    dimension.values = list(location = counties.this.msa), 
    keep.dimensions = c('year', 'race', 'ethnicity', 'age'), 
    na.rm = T) 
  
  if (msa == "C.12580") browser()
  
  if(is.null(births.msas)) next
  
  #Have to remove the 'source' dimension
  new.dimnames = dimnames(births.msas)[-length(dim(births.msas))] #create the new dimnames (4 not 5)
  dim(births.msas) = sapply(new.dimnames, length) #apply the new dim
  dimnames(births.msas)=new.dimnames #apply the new dimension attribute
  
  #Add location as a dimension (msa name)
  new.dimnames = c(dimnames(births.msas), list(location = msa))
  dim(births.msas) = sapply(new.dimnames, length) #apply the new dim
  dimnames(births.msas)=new.dimnames #apply the new dimension attribute

  syphilis.manager$put(
    births.msas,
    outcome = 'births',
    source = 'cdc.wonder.natality',
    ontology.name = 'cdc.fertility',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data'
  )} 
