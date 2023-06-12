
# depends on age


read.race.stratified.age.data <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    arr = apply(df[,-1], 2, function(x){
        as.numeric(gsub(',', '', x))
    })
    
    col.names = dimnames(arr)[[2]]
    col.names = col.names[(1:length(col.names))%%3==1]
    dim.names = list(
        age = standardize.age.strata.names(df[,1]),
        type = c('n','est','rate'),
        race = substr(col.names, 3, nchar(col.names))
    )
    
    dim(arr) = sapply(dim.names, length)
    dimnames(arr) = dim.names
    arr
}