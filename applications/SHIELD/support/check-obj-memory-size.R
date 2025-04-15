
# install.packages("pryr")
library(pryr)
mem_used()  # total memory used by R

# Check individual object sizes (includes shared memory)
objs <- ls()
sizes <- sapply(objs, function(x) object_size(get(x)))
obj_df <- data.frame(
    Object = names(sizes),
    Size_Bytes = as.numeric(sizes)
)
obj_df <- obj_df[order(-obj_df$Size_Bytes), ]
obj_df$Size_MB <- format(obj_df$Size_Bytes / 1024^2, digits = 3)

head(obj_df, 20)
