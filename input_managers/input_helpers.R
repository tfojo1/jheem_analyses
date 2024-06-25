
restratify.data.to.specification <- function(data, dim.names,
                                             covariate.names = setdiff(names(data), c('year','p','value','weight')),
                                             max.age = 85,
                                             catchall.covariate.value = 'all')
{
    missing.covariate.names = setdiff(covariate.names, names(dim.names))
    if (length(missing.covariate.names)>0)
        stop(paste0(collapse.with.and("'", missing.covariate.names, "'"),
                    " ",
                    ifelse(length(missing.covariate.names)==1, 
                           "is listed as a covariate but is not a dimension",
                           "are listed as covariates but are not dimensions"),
                    " in the given dim.names"))
    
    if (all(names(data)!='weight'))
        data$weight = 1
    
    # Set up the covariates (besides age) that we will handle
    non.age.covariates = setdiff(covariate.names, 'age')
    
    current.ontology = lapply(non.age.covariates, function(covariate){
        setdiff(unique(data[,covariate]), catchall.covariate.value)
    })
    names(current.ontology) = non.age.covariates
    
    target.ontology = dim.names[non.age.covariates]
    
    # Get a mapping to the new covariate values
    mappings = get.mappings.to.align.ontologies(ontology.1 = current.ontology,
                                                ontology.2 = target.ontology)
    
    if (is.null(mappings))
        stop(paste0("Cannot find a mapping for the '", 
                    collapse.with.and("'", covariate.names, "'"),
                    "' covariate(s) to or from the given dim.names"))
    
    # Set up indices from old to new covariate value sets
    target.indices.arr = array(1:prod(sapply(target.ontology, length)),
                               dim = sapply(target.ontology, length),
                               dimnames = target.ontology)
    
    # target.flattened = reshape2::melt(target.indices.arr)[1:length(target.ontology)]
    # for (j in 1:ncol(target.flattened))
    #     target.flattened[,j] = as.character(target.flattened[,j])
    target.flattened = get.every.combination(target.ontology)
    
    intermediate.dim.names = mappings[[1]]$apply.to.dim.names(from.dim.names = current.ontology)
    current.to.intermediate = mappings[[1]]$get.reverse.mapping.indices(from.dim.names = current.ontology,
                                                                        to.dim.names = intermediate.dim.names)
    
    intermediate.to.target = mappings[[2]]$get.mapping.indices(from.dim.names = target.ontology,
                                                               to.dim.names = intermediate.dim.names)
    
    current.to.target = intermediate.to.target[current.to.intermediate]
    dim(current.to.target) = sapply(current.ontology, length)
    dimnames(current.to.target) = current.ontology
    
    # Figure out the new covariate values for each row
    # Plug in 'all' when possible to simplify
    
    data.to.map = data
    data = NULL
    for (i in 1:nrow(data.to.map))
    {
        i.dim.names = as.list(data.to.map[i,non.age.covariates])
        i.dim.names = lapply(i.dim.names, function(values){
          if (length(values)==1 && values==catchall.covariate.value)
            NULL
          else
            as.character(values)
        })
        names(i.dim.names) = non.age.covariates
        
        if (any(!sapply(i.dim.names, is.null)))
        {
            indices = unlist(array.access(current.to.target, i.dim.names))
            if (length(indices)>0)
            {
                expand.to.covariates = target.flattened[unique(indices),,drop=F]
                
                if (length(non.age.covariates)>1)
                {
                    for (j in 1:length(non.age.covariates))
                    {
                        covariate = non.age.covariates[j]
                        non.j.values = unique(expand.to.covariates[,-j,drop=F])
                        
                        if (nrow(non.j.values) < nrow(expand.to.covariates))
                        {
                            for (k in 1:nrow(non.j.values))
                            {
                                mask = apply(expand.to.covariates, 1, function(values){
                                    all(values[-j] == non.j.values[k,])
                                })
                              
                                if (setequal(as.character(expand.to.covariates[mask,j]), dim.names[[covariate]]))
                                {
                                    new.row = expand.to.covariates[mask,,drop=F][1,]
                                    new.row[j] = catchall.covariate.value
                                    
                                    expand.to.covariates = expand.to.covariates[!mask,]
                                    expand.to.covariates = rbind(expand.to.covariates, new.row)
                                }
                            }
                        }
                    }
                }
              
                row = data.to.map[i,]
                row$weight = row$weight / nrow(expand.to.covariates)
              
                for (k in 1:nrow(expand.to.covariates))
                {
                    row[,non.age.covariates] = expand.to.covariates[k,non.age.covariates]
                    data = rbind(data, row)
                }
            }
        }
        else
        {
            data = rbind(data,
                         data.to.map[i,])
        }
    }
    
    
    if (any(covariate.names=='age'))
    {
        not.all.age.mask = data$age != catchall.covariate.value
        mapped.data = NULL
        
        not.all.age.data = data[not.all.age.mask,]
        
        parsed.ages = parse.age.strata.names(as.character(not.all.age.data$age))
        if (is.null(parsed.ages))
            stop("Could not parse all age strata in the data")
        
        parsed.dim.names.age = parse.age.strata.names(dim.names$age)
        if (is.null(parsed.ages))
            stop("Could not parse all age strata in the given dim.names$age")
        spec.lower = parsed.dim.names.age$lower
        spec.upper = pmin(parsed.dim.names.age$upper, max.age)
        
        for (i in 1:nrow(not.all.age.data))
        {
            lower = parsed.ages$lower[i]
            upper = min(max.age, parsed.ages$upper[i])
            fraction.in.specification.age = pmax(0,
                                                 (pmin(upper, spec.upper) - pmax(lower, spec.lower)) / (upper - lower)
            )
            
            non.zero.ages = (1:length(spec.upper))[fraction.in.specification.age>0]
            
            data.row = not.all.age.data[i,]
            weight = data.row$weight 
            
            for (age in non.zero.ages)
            {
                data.row$age = dim.names$age[age]
                data.row$weight = weight * fraction.in.specification.age[age]
                mapped.data = rbind(mapped.data, data.row)
            }
        }
        
        all.age.data = data[!not.all.age.mask,]
        data = rbind(all.age.data,
                     mapped.data)
    }
    
    data
}


OLD.restratify.data.to.specification <- function(data, dim.names,
                                                 covariate.names = setdiff(names(data), c('year','p','value','weight')),
                                                 max.age = 85)
{
  missing.covariate.names = setdiff(covariate.names, names(dim.names))
  if (length(missing.covariate.names)>0)
    stop(paste0(collapse.with.and("'", missing.covariate.names, "'"),
                " ",
                ifelse(length(missing.covariate.names)==1, 
                       "is listed as a covariate but is not a dimension",
                       "are listed as covariates but are not dimensions"),
                " in the given dim.names"))
  
  if (all(names(data)!='weight'))
    data$weight = 1
  
  non.age.covariates = setdiff(covariate.names, 'age')
  for (covariate in non.age.covariates)
  {
    from.ontology = list(setdiff(unique(data[,covariate]), 'all'))
    names(from.ontology) = covariate
    
    mapping = get.ontology.mapping(from.ontology = from.ontology,
                                   to.ontology = dim.names[covariate])
    
    not.all.mask = data[,covariate] != 'all'
    
    if (is.null(mapping))
    {   
      mapping = get.ontology.mapping(from.ontology = dim.names[covariate],
                                     to.ontology = from.ontology)
      
      if (is.null(mapping))
        stop(paste0("Cannot find a mapping for the '", covariate, "' covariate to or from the given dim.names"))
      
      mapping.vector = mapping$get.mapping.vector(from.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
      tabled.mapping.vector = table(mapping.vector)
      
      reverse.mapping.vector = mapping$get.reverse.mapping.vector(to.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
      
      mapped.data = NULL
      unmapped.mask = !not.all.mask
      unmapped.mask[!unmapped.mask] = tabled.mapping.vector[ data[!unmapped.mask,covariate] ] == 1
      data.to.map = data[!unmapped.mask,]
      unmapped.data = data[unmapped.mask,]
      unmapped.data[unmapped.data[,covariate]!='all', covariate] = reverse.mapping.vector[ unmapped.data[,covariate] ]
      
      for (i in 1:nrow(data.to.map))
      {
        row = data.to.map[i,]
        val = data.to.map[i,covariate]
        row$weight = row$weight / tabled.mapping.vector[val]
        for (map.to.val in names(mapping.vector)[mapping.vector==val])
        {
          row[,covariate] = map.to.val
          mapped.data = rbind(mapped.data, row)
        }
      }
      
      data = rbind(unmapped.data,
                   mapped.data)
    }
    else
    {
      mapping.vector = mapping$get.mapping.vector(from.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
      weight.mapping = rep(1, length(mapping.vector))
      names(weight.mapping) = names(mapping.vector)
      data[not.all.mask,covariate] = mapping.vector[ data[not.all.mask,covariate] ]
    }
    
  }
  
  
  if (any(covariate.names=='age'))
  {
    not.all.age.mask = data$age != 'all'
    mapped.data = NULL
    
    not.all.age.data = data[not.all.age.mask,]
    
    parsed.ages = parse.age.strata.names(not.all.age.data$age)
    if (is.null(parsed.ages))
      stop("Could not parse all age strata in the data")
    
    parsed.dim.names.age = parse.age.strata.names(dim.names$age)
    if (is.null(parsed.ages))
      stop("Could not parse all age strata in the given dim.names$age")
    spec.lower = parsed.dim.names.age$lower
    spec.upper = pmin(parsed.dim.names.age$upper, max.age)
    
    for (i in 1:nrow(not.all.age.data))
    {
      lower = parsed.ages$lower[i]
      upper = min(max.age, parsed.ages$upper[i])
      fraction.in.specification.age = pmax(0,
                                           (pmin(upper, spec.upper) - pmax(lower, spec.lower)) / (upper - lower)
      )
      
      non.zero.ages = (1:length(spec.upper))[fraction.in.specification.age>0]
      
      data.row = not.all.age.data[i,]
      weight = data.row$weight 
      
      for (age in non.zero.ages)
      {
        data.row$age = dim.names$age[age]
        data.row$weight = weight * fraction.in.specification.age[age]
        mapped.data = rbind(mapped.data, data.row)
      }
    }
    
    all.age.data = data[!not.all.age.mask,]
    data = rbind(all.age.data,
                 mapped.data)
  }
  
  data
}
