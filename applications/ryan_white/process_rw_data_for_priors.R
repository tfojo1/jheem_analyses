
#source('applications/ryan_white/ryan_white_specification.R')

source('../jheem_analyses/applications/ryan_white/ryan_white_data_ontology_mappings.R')

build.rw.proportion.outcome.data.frame <- function(numerator.data,
                                                   denominator.data,
                                                   specification.metadata,
                                                   use.single.denominator = !is.list(denominator.data),
                                                   verbose=F)
{
    df = NULL
    
    all.dimensions = unique(unlist(lapply(numerator.data, function(d){names(dim(d))})))
    all.dimensions.less.year.location = setdiff(all.dimensions, c('year','location'))
    all.columns = c('p', 'n', all.dimensions)
    
    for (i in 1:length(numerator.data))
    {
        num = numerator.data[[i]]
        year = dimnames(num)$year
        if (!use.single.denominator || any(year==dimnames(denominator.data)$year))
        {
            if (1==2 && any(names(dim(num))=='risk') && !any(names(dim(num))=='sex'))
            {
                if (verbose)
                  print(paste0("Skipping data for year ", year, " - has risk without sex"))
            }
            else if (length(dimnames(num)$sex)==1 && dimnames(num)$sex=='transgender')
            {}
            else
            {
                if (use.single.denominator)
                    denom = denominator.data
                else
                    denom = denominator.data[[i]]
              
                num.dim.names = dimnames(num)
                if (!is.null(num.dim.names$sex) && any(grepl('trans', num.dim.names$sex)))
                {
                    num = array.access(num, sex=c('male','female'))
                    if (!use.single.denominator)
                        denom = array.access(denom, sex=c('male','female'))
                }
                if (!is.null(num.dim.names$risk) &&
                    (any(num.dim.names$risk=='sexual') || any(num.dim.names$risk=='perinatal')))
                {
                    num = array.access(num, risk=intersect(dimnames(num)$risk, c('msm','idu','msm_idu','heterosexual')))
                    if (!use.single.denominator)
                        denom = array.access(denom, risk=intersect(dimnames(num)$risk, c('msm','idu','msm_idu','heterosexual')))
                }
                
                if (any(names(dim(num))=='risk') && !any(names(dim(num))=='sex'))
                {
                    target.dim.names = c(dimnames(num)[c('year','location')],
                                         specification.metadata$dim.names[intersect(names(dim(num)), all.dimensions.less.year.location)])
                    target.dim.names$sex = specification.metadata$dim.names$sex
                    
                    if (length(dimnames(num)$risk)==1 && dimnames(num)$risk=='idu')
                    {
                        target.dim.names$sex = setdiff(target.dim.names$sex, 'msm')
                        target.dim.names$risk = c('active_IDU','IDU_in_remission')
                        
                        if (use.single.denominator)
                            denom = array.access(denominator.data, risk=c('idu'))
                    }
                    else if (all(dimnames(num)$risk != 'msm'))
                    {
                        target.dim.names$sex = setdiff(target.dim.names$sex, 'msm')
                        if (use.single.denominator)
                            denom = array.access(denominator.data, risk=c('heterosexual','idu'))
                    }
                }
                else
                {
                    target.dim.names = c(dimnames(num)[c('year','location')],
                                         specification.metadata$dim.names[intersect(names(dim(num)), all.dimensions.less.year.location)])
                    
                    if (length(dimnames(num)$risk)==1 && dimnames(num)$risk=='idu')
                    {
                        target.dim.names$risk = c('active_IDU','IDU_in_remission')
                        
                        if (use.single.denominator)
                          denom = array.access(denominator.data, risk=c('idu'))
                    }
                    
                    if (length(dimnames(denom)$risk)>0 && any(dimnames(denom)$risk=='other'))
                      denom = array.access(denom, risk=setdiff(dimnames(denom)$risk, 'other'))
                }    
                
                if (length(dimnames(num)$sex)==1 && length(intersect(dimnames(num)$sex, target.dim.names$sex))==1)
                    target.dim.names$sex = dimnames(num)$sex    
                if (length(dimnames(num)$sex)==1 && dimnames(num)$sex=='male')
                    target.dim.names$sex = c('heterosexual_male','msm')   
                
                error.prefix = paste0("Cannot map ontology for ", get.ordinal(i), " numerator: ")

                mapped.num = map.value.ontology(value = num, target.dim.names = target.dim.names, error.prefix=error.prefix)
                mapped.denom = map.value.ontology(value = denom, target.dim.names = target.dim.names, error.prefix=error.prefix)
                mapped.p = mapped.num / mapped.denom
                
                melted = reshape2::melt(mapped.p, value.name = 'p')
                melted$n = as.numeric(mapped.denom)
                for (missing.col in setdiff(all.columns, dimnames(melted)[[2]]))
                    melted[,missing.col] = 'all'
                
                df = rbind(df, melted[,all.columns])
            }
        }
        else if (verbose)
            print(paste0("Skipping data for year ", year, " - no denominator data"))
    }
    
    for (col in all.dimensions.less.year.location)
      df[,col] = factor(df[,col], levels = union('all', unique(df[,col])))
    
    df
}

read.non.adap.data <- function(root.dir)
{
    sub.dirs = list.dirs(root.dir, full.names = F, recursive = F)
    years = as.numeric(substr(sub.dirs, 1, 4))
    
    rv = list()
    for (i in 1:length(sub.dirs))
    {
        sub.dir = file.path(root.dir, sub.dirs[i])
        year = years[i]
        
        files = list.files(sub.dir)
        
        # Race Table
        race.table.name = paste0(year, "_table_2_race.csv")
        race.table = read.csv(file.path(sub.dir, race.table.name), stringsAsFactors = F)
        rv = c(rv,
               read.rw.table(data=race.table,
                             column.dimension='race',
                             year=year,
                             location='US',
                             fixed.dimension.values = list()))
        
        # Sex Table
        sex.table.name = paste0(year, "_table_3_sex.csv")
        sex.table = read.csv(file.path(sub.dir, sex.table.name), stringsAsFactors = F)
        rv = c(rv,
               read.rw.table(data=sex.table,
                             column.dimension='sex',
                             year=year,
                             location='US',
                             fixed.dimension.values = list()))
        
        
        # Sex x Risk Tables
        sex.risk.arr = NULL
        for (sex in c('male','female'))
        {
            if (sex == 'male')
                letter = 'a'
            else
                letter = 'b'
            
            sex.risk.table.name = paste0(year, "_table_4", letter, "_", sex, "_risk.csv")
            
            sex.risk.table = read.csv(file.path(sub.dir, sex.risk.table.name), stringsAsFactors = F)
            sex.risk.values = read.rw.table(data=sex.risk.table,
                                 column.dimension='risk',
                                 year=year,
                                 location='US',
                                 fixed.dimension.values = list(sex=sex))
            
            if (is.null(sex.risk.arr))
            {
                sex.risk.dim.names = dimnames(sex.risk.values[[1]])
                sex.risk.dim.names$sex = c('male','female')
                sex.risk.arr = array(NA, dim=sapply(sex.risk.dim.names, length), dimnames=sex.risk.dim.names)
            }
            
            array.access(sex.risk.arr, sex=sex, risk=dimnames(sex.risk.values[[1]])$risk) = sex.risk.values[[1]]
        }
        
        rv = c(rv, list(sex.risk.arr))
    }
    
    rv
}

read.oahs.and.suppression.data <- function(root.dir)
{
    sub.dirs = list.dirs(root.dir, full.names = F, recursive = F)
    years = as.numeric(substr(sub.dirs, 1, 4))
    
    rv = list(oahs=list(),
              suppression=list())
    for (i in 1:length(sub.dirs))
    {
        sub.dir = file.path(root.dir, sub.dirs[i])
        year = years[i]
        
        files = list.files(sub.dir)
        
        # female
        female.race.table.name = paste0(year, "_table_11b_female_race.csv")
        female.race.table = read.csv(file.path(sub.dir, female.race.table.name), stringsAsFactors = F)
        female.race.oahs.values = read.rw.table(data=female.race.table,
                                                column.dimension='race',
                                                year=year,
                                                location='US',
                                                n.per.col = 3,
                                                get.nth.per.col = 1,
                                                fixed.dimension.values = list(sex='female'))
        female.race.suppression.values = read.rw.table(data=female.race.table,
                                                column.dimension='race',
                                                year=year,
                                                location='US',
                                                n.per.col = 3,
                                                get.nth.per.col = 2,
                                                fixed.dimension.values = list(sex='female'))
        
        rv$oahs = c(rv$oahs, female.race.oahs.values)
        rv$suppression = c(rv$suppression, female.race.suppression.values)
        
        # msm
        msm.race.table.name = paste0(year, "_table_12d_msm_race.csv")
        msm.race.table = read.csv(file.path(sub.dir, msm.race.table.name), stringsAsFactors = F)
        msm.race.oahs.values = read.rw.table(data=msm.race.table,
                                                column.dimension='race',
                                                year=year,
                                                location='US',
                                                n.per.col = 3,
                                                get.nth.per.col = 1,
                                                fixed.dimension.values = list(sex='msm'))
        msm.race.suppression.values = read.rw.table(data=msm.race.table,
                                                       column.dimension='race',
                                                       year=year,
                                                       location='US',
                                                       n.per.col = 3,
                                                       get.nth.per.col = 2,
                                                       fixed.dimension.values = list(sex='msm'))
        
        rv$oahs = c(rv$oahs, msm.race.oahs.values)
        rv$suppression = c(rv$suppression, msm.race.suppression.values)
        
        # male
        male.race.table.name = paste0(year, "_table_12c_male_race.csv")
        male.race.table = read.csv(file.path(sub.dir, male.race.table.name), stringsAsFactors = F)
        male.race.oahs.values = read.rw.table(data=male.race.table,
                                                column.dimension='race',
                                                year=year,
                                                location='US',
                                                n.per.col = 3,
                                                get.nth.per.col = 1,
                                                fixed.dimension.values = list(sex='male'))
        male.race.suppression.values = read.rw.table(data=male.race.table,
                                                       column.dimension='race',
                                                       year=year,
                                                       location='US',
                                                       n.per.col = 3,
                                                       get.nth.per.col = 2,
                                                       fixed.dimension.values = list(sex='male'))

        het.male.only.oahs = male.race.oahs.values[[1]] - msm.race.oahs.values[[1]]
        het.male.only.supp = male.race.suppression.values[[1]] - msm.race.suppression.values[[1]]
        dimnames(het.male.only.oahs)$sex = 'heterosexual_male'
        dimnames(het.male.only.supp)$sex = 'heterosexual_male'
        
        
        rv$oahs = c(rv$oahs, list(het.male.only.oahs), male.race.oahs.values[-1])
        rv$suppression = c(rv$suppression, list(het.male.only.supp), male.race.suppression.values[-1])
        
        # idu
        # NOT using this because it makes the prior on suppression MUCH (1.08 OR) more likely for IDU
        
        # idu.race.table.name = paste0(year, "_table_18b_idu_race.csv")
        # idu.race.table = read.csv(file.path(sub.dir, idu.race.table.name), stringsAsFactors = F)
        # idu.race.oahs.values = read.rw.table(data=idu.race.table,
        #                                      column.dimension='race',
        #                                      year=year,
        #                                      location='US',
        #                                      n.per.col = 3,
        #                                      get.nth.per.col = 1,
        #                                      fixed.dimension.values = list(risk='idu'))
        # idu.race.suppression.values = read.rw.table(data=idu.race.table,
        #                                             column.dimension='race',
        #                                             year=year,
        #                                             location='US',
        #                                             n.per.col = 3,
        #                                             get.nth.per.col = 2,
        #                                             fixed.dimension.values = list(risk='idu'))
        
     #   rv$oahs = c(rv$oahs, idu.race.oahs.values)
    #    rv$suppression = c(rv$suppression, idu.race.oahs.values)
    }
    
    rv
}

read.adap.data <- function(root.dir)
{
    sub.dirs = list.dirs(root.dir, full.names = F, recursive = F)
    years = as.numeric(substr(sub.dirs, 1, 4))
    
    rv = list()
    for (i in 1:length(sub.dirs))
    {
      sub.dir = file.path(root.dir, sub.dirs[i])
      year = years[i]
      
      files = list.files(sub.dir)
      
      # Race Table
      race.table.name = paste0(year, "_table_2_race.csv")
      race.table = read.csv(file.path(sub.dir, race.table.name), stringsAsFactors = F)
      rv = c(rv,
             read.rw.table(data=race.table,
                           column.dimension='race',
                           year=year,
                           location='US',
                           fixed.dimension.values = list()))
      
      # Sex Table
      sex.table.name = paste0(year, "_table_3_sex.csv")
      sex.table = read.csv(file.path(sub.dir, sex.table.name), stringsAsFactors = F)
      rv = c(rv,
             read.rw.table(data=sex.table,
                           column.dimension='sex',
                           year=year,
                           location='US',
                           fixed.dimension.values = list()))
    }
    
    rv
}

# returns a list of arrays
read.rw.table <- function(data, column.dimension, year, location, 
                          n.per.col = 2,
                          get.nth.per.col = 1,
                          fixed.dimension.values = list())
{
    subtotal.mask = grepl("total", data[,1], ignore.case = T)
    data = data[!subtotal.mask,]
  
    split.across.rows = (1:nrow(data))[data[,1] == "and injection drug use"]
    if (length(split.across.rows)>0)
    {
        data[split.across.rows-1,1] = paste0(data[split.across.rows-1,1], " ", data[split.across.rows,1])
        data = data[-split.across.rows,]
    }
    
    n.col = ncol(data)
    n.row = nrow(data)
    
    # Split out the columsn in to n and pct columns
    num.col.indices = 1 + n.per.col*(1:floor(n.col/n.per.col)) - (n.per.col-get.nth.per.col)
    col.name.indices = 1 + n.per.col*(1:floor(n.col/n.per.col)) - (n.per.col-1)
    column.values = dimnames(data)[[2]][col.name.indices]
    
    keep.mask = !grepl('total', column.values)
    num.col.indices = num.col.indices[keep.mask]
    col.name.indices = col.name.indices[keep.mask]
    column.values = column.values[keep.mask]
     
    # Split up the rows by each category
    category.row.indices = (1:n.row)[is.na(data[,2]) | data[,2]=='']
    categories = data[category.row.indices,1]
    
    category.data.row.indices = lapply(1:length(categories), function(category.i){
        if (category.i == length(categories))
            last.row = n.row
        else
            last.row = category.row.indices[category.i+1]-1
        
        (category.row.indices[category.i]+1):last.row
    })
    
    keep.mask = c(category.row.indices[-1]!=(category.row.indices[-length(category.row.indices)]+1),T)
    category.row.indices = category.row.indices[keep.mask]
    categories = categories[keep.mask]
    category.data.row.indices = category.data.row.indices[keep.mask]
    
    # Parse the dimensions for each category
    category.dimensions = rep(as.character(NA), length(categories))
    category.dimension.values = lapply(categories, function(cat){list()})
    
    category.dimensions[grepl('age', categories, ignore.case = T)] = 'age'
    
    category.dimensions[grepl('gender', categories, ignore.case = T) & !grepl('trans', categories, ignore.case = T)] = 'sex'
    
    category.dimensions[grepl('race', categories, ignore.case = T)] = 'race'
    
    category.dimensions[grepl('transmission category', categories, ignore.case = T)] = 'risk'
    
    female.risk.mask = grepl('female client', categories, ignore.case = T)
    male.risk.mask = grepl('Male client', categories, ignore.case = T) & !female.risk.mask
    trans.risk.mask = grepl('Transgender client', categories, ignore.case = T) & !female.risk.mask
    category.dimensions[female.risk.mask] = 'risk'
    category.dimension.values[female.risk.mask] = lapply(1:sum(female.risk.mask), function(i){list(sex='female')})
    category.dimensions[male.risk.mask] = 'risk'
    category.dimension.values[male.risk.mask] = lapply(1:sum(male.risk.mask), function(i){list(sex='male')})
    category.dimensions[trans.risk.mask] = 'risk'
    category.dimension.values[trans.risk.mask] = lapply(1:sum(trans.risk.mask), function(i){list(sex='transgender')})
    
    
    category.dimensions[grepl('poverty', categories, ignore.case = T)] = 'income'
    
    if (any(is.na(category.dimensions)))
        stop(paste0("Don't know how to interpret the dimension from row heading(s): ",
                    collapse.with.and("'", categories[is.na(category.dimensions)], "'")))
    
    keep.mask = category.dimensions != 'income'
    category.row.indices = category.row.indices[keep.mask]
    categories = categories[keep.mask]
    category.data.row.indices = category.data.row.indices[keep.mask]

    # Parse up the data
    lapply(1:length(categories), function(category.i){
        
          row.indices = category.data.row.indices[[category.i]]
          
          sub.data = data[row.indices,num.col.indices]
          sub.rv = apply(sub.data, 2, function(x){
              suppressWarnings(as.numeric(gsub(',', '', x)))
          })
          
          orig.row.values = row.values = data[row.indices,1]
          if (category.dimensions[category.i]=='age')
              row.values = convert.rw.age.names(row.values)
          else if (category.dimensions[category.i]=='risk')
              row.values = RW.RISK.MAPPINGS[row.values]
          else if (category.dimensions[category.i]=='sex')
              row.values = RW.SEX.MAPPINGS[row.values]
          
          if (any(is.na(row.values)))
              stop(paste0("Don't know how to interpret the value from row(s) called ",
                          collapse.with.and("'", orig.row.values[is.na(row.values)], "'"),
                          " for dimension '", category.dimensions[category.i], "'"))
          
          dim.names = list(as.character(year), location, as.character(row.values), column.values)
          names(dim.names) = c('year', 'location', category.dimensions[category.i], column.dimension)
          dim.names = c(dim.names, category.dimension.values[[category.i]], fixed.dimension.values)
          
          if (!is.null(dim.names$race))
              dim.names$race = gsub('Latinob', 'Latino', dim.names$race)
          if (!is.null(dim.names$risk))
              dim.names$risk = gsub('heteosexual', 'heterosexual', dim.names$risk)
          
          dim(sub.rv) = sapply(dim.names, length)
          dimnames(sub.rv) = dim.names
          
          sub.rv
    })
}

convert.rw.age.names <- function(age.names)
{
    less.than.mask = grepl("<", age.names)
    age.names[less.than.mask] = paste0("0-", as.numeric(substr(age.names[less.than.mask], 2, nchar(age.names[less.than.mask])))-1)
    
    greater.than.mask = grepl("≥", age.names)
    age.names[greater.than.mask] = paste0(substr(age.names[greater.than.mask], 2, nchar(age.names[greater.than.mask])),"+")
    
    paste0(age.names, " years")
}

RW.SEX.MAPPINGS = c(
  'Male' = 'male',
  'Female' = 'female',
  'Transgender male' = 'trans_male',
  'Transgender female' = 'trans_female',
  'Other gender identity' = 'other',
  'Transgender' = 'trans'
)

RW.RISK.MAPPINGS = c(
    'Male-to-male sexual contact' = 'msm',
    'Injection drug use' = 'idu',
    'Male-to-male sexual contact and injection drug use' = 'msm_idu',
    'Heterosexual contact' = 'heterosexual',
    'Heterosexual contactb' = 'heterosexual',
    'Heterosexual contactc' = 'heterosexual',
    'Perinatal' = 'perinatal',
    'Otherc' = 'other',
    'Sexual contacte and injection drug use' = 'sexual_idu',
    'Sexual contacte' = 'sexual',
    'Otherd' = 'other'
)
