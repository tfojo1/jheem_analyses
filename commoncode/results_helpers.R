

add.total.location <- function(arr)
{
    target.dim.names = dimnames(arr)
    target.dim.names$location = c(target.dim.names$location, 'Total')
    
    rv = array(0, dim=sapply(target.dim.names, length), dimnames = target.dim.names)
    
    array.access(rv, dimnames(arr)) = arr
    array.access(rv, location='Total') = apply(arr, setdiff(names(target.dim.names), 'location'), sum)
    
    rv
}

interleave.columns <- function(mat1, mat2)
{
    rv = cbind(mat1, mat2)
    
    indices = rep(1:ncol(mat1), each=2) + rep(c(0,ncol(mat1)), ncol(mat2))
    
    rv[,indices]
}


interleave.rows <- function(mat1, mat2)
{
    rv = rbind(mat1, mat2)
    
    indices = rep(1:nrow(mat1), each=2) + rep(c(0,nrow(mat1)), nrow(mat2))
    
    rv[indices,]
}
