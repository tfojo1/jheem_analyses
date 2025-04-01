
library(ggplot2)
tryCatch({
    library(xlsx)
},
error=function(e){
    print("UNABLE TO LOAD 'xlsx' package - write.shaded.table will not work")
})

#'@param tab The tabular object to write
#'@param file The file to save to
#'@param color.by A tabular object, of the same dimensions as tab, 
write.shaded.table <- function(tab,
                               file,
                               color.by=tab,
                               thresholds,
                               colors=NULL,
                               lower.threshold.colors=NULL,
                               upper.threshold.colors=NULL,
                               na.value=NULL,
                               na.color=NULL,
                               text.colors = NULL,
                               allow.na=!is.null(na.color),
                               write.row.names=T,
                               write.col.names=T)
{
    #-- Check Arguments --#
    
    if (is.null(dim(tab)) || length(dim(tab))!=2)
        stop("'tab' must be a two-dimensional tabular object (matrix, array, data.frame, etc.)")
    
    if (is.null(dim(color.by)) || length(dim(color.by))!=2 || any(dim(tab)!=dim(color.by)))
        stop("'color.by' must have the same dimensions as 'tab'")
    
   
    #-- Assign Colors --#
    
    colors = assign.pretty.colors(color.by=color.by,
                                  thresholds=thresholds,
                                  colors=colors,
                                  lower.threshold.colors=lower.threshold.colors,
                                  upper.threshold.colors=upper.threshold.colors,
                                  na.value=na.value,
                                  na.color=na.color,
                                  allow.na=allow.na)
    
    #-- Call the sub function and return --#
    
    do.write.shaded.xlsx(x=tab,
                         colors=colors,
                         file=file,
                         na=na.value,
                         text.colors = text.colors,
                         row.names=write.row.names,
                         col.names=write.col.names)
}

plot.shaded.table <- function(tab,
                              color.by=tab,
                              thresholds,
                              colors=NULL,
                              lower.threshold.colors=NULL,
                              upper.threshold.colors=NULL,
                              na.value=NULL,
                              na.color=NULL,
                              allow.na=!is.null(na.color))
{
    #-- Check Arguments --#
    
    if (is.null(dim(tab)) || length(dim(tab))!=2)
        stop("'tab' must be a two-dimensional tabular object (matrix, array, data.frame, etc.)")
    
    if (is.null(dim(color.by)) || length(dim(color.by))!=2 || any(dim(tab)!=dim(color.by)))
        stop("'color.by' must have the same dimensions as 'tab'")
    
    
    #-- Assign Colors --#
    
    colors = assign.pretty.colors(color.by=color.by,
                                  thresholds=thresholds,
                                  colors=colors,
                                  lower.threshold.colors=lower.threshold.colors,
                                  upper.threshold.colors=upper.threshold.colors,
                                  na.value=na.value,
                                  na.color=na.color,
                                  allow.na=allow.na)
    
    color.mapping = as.character(colors)
    names(color.mapping) = paste0('c', 1:length(colors))
    
    #-- Make the data frame --#
    
    x.values = dimnames(tab)[[2]]
    if (is.null(x.values))
        x.values = as.character(1:dim(tab)[2])
    y.values = dimnames(tab)[[1]]
    if (is.null(y.values))
        y.values = as.character(1:dim(tab)[1])
    
#    melted = suppressWarnings(reshape2::melt(tab))
    
    df = data.frame(
        value = as.character(tab),
        x=rep(x.values, each=length(y.values)),
        y=rep(y.values, length(x.values)),
        color = names(color.mapping)
    )
    df$y = factor(df$y, rev(y.values))
    df$x = factor(df$x, levels=x.values)
    
    x.lab = names(dimnames(tab))[2]
    y.lab = names(dimnames(tab))[1]    
    
    ggplot(df, aes(x,y,fill=color,label=value)) +
        geom_tile() + geom_text() +
        scale_fill_manual(values=color.mapping, guide='none') +
        xlab(x.lab) + ylab(y.lab)
}

make.pretty.table.legend <- function(colors,
                                     thresholds=rep(1:length(colors)))
{
    df = data.frame(
        x=as.character(thresholds),
        y=rep(1, length(colors))
    )
    
    names(colors) = df$x
    
    ggplot(df, aes(x,y,fill=x)) + geom_bar(stat='identity') +
        scale_fill_manual(values=colors)
}

##-------------##
##-- HELPERS --##
##-------------##

assign.pretty.colors <- function(color.by=tab,
                                 thresholds,
                                 colors,
                                 lower.threshold.colors,
                                 upper.threshold.colors,
                                 na.value=NULL,
                                 na.color=NULL,
                                 allow.na=!is.null(na.color))
{
    original.color.by.na = is.na(color.by)
    color.by = apply(color.by, 2, as.numeric)
    if (any(is.na(color.by) & !original.color.by.na))
        stop("'color.by' must contain only numeric values")
    if (any(is.na(color.by)))
    {
        if (!allow.na)
            stop("'color.by' cannot contain NA values unless 'allow.na' is set to TRUE")
        if (is.null(na.color))
            stop("'na.color' must be set to handle NA values")
    }
    
    if (!is.numeric(thresholds) || length(thresholds)<2)
        stop("'thresholds' must be a numeric vector with at least two elements")
    
    n.segments = length(thresholds)-1
    if (any(is.na(thresholds)))
        stop("'thresholds' cannot contain NA values")
    if (any(is.infinite(thresholds)))
        stop("'thresholds' cannot contain infinite values")
    
    threshold.lowers = thresholds[-length(thresholds)]
    threshold.uppers = thresholds[-1]
    if (any(threshold.uppers<=threshold.lowers))
        stop("'thresholds' must be strictly increasing")
    
    
    if (is.null(colors))
    {
        if (is.null(lower.threshold.colors) || is.null(upper.threshold.colors))
            stop("You must set either 'colors' or 'lower.threshold.colors' AND 'upper.threshold.colors")
        
        if (length(lower.threshold.colors)!=n.segments)
            stop(paste0("'lower.threshold.colors' must have ", n.segments, " values (one less than the number of values in 'thresholds')"))
        if (length(upper.threshold.colors)!=n.segments)
            stop(paste0("'upper.threshold.colors' must have ", n.segments, " values (one less than the number of values in 'thresholds')"))
        
    }
    else
    {
        if (length(colors) != length(thresholds))
            stop("colors must have the same length as thresholds")
        lower.threshold.colors = colors[-length(colors)]
        upper.threshold.colors = colors[-1]
    }
    
    
    #-- Set up the color ramps --#
    
    color.ramps = lapply(1:n.segments, function(i){
        colorRamp(c(lower.threshold.colors[i], upper.threshold.colors[i]))
    })
    
    #-- Calculate the colors for each cell --#
    
    colors = apply(color.by, 1:2, function(val){
        if (is.na(val))
            na.color
        else if (val <= threshold.lowers[1])
            lower.threshold.colors[1]
        else if (val >= threshold.uppers[n.segments])
            upper.threshold.colors[n.segments]
        else
        {
            segment.mask = val >= threshold.lowers & val < threshold.uppers
            scaled.val = (val-threshold.lowers[segment.mask]) /
                (threshold.uppers[segment.mask]-threshold.lowers[segment.mask])
            ramp = color.ramps[segment.mask][[1]]
            rgb.to.hex(ramp(scaled.val), prepend = '#')
        }
    })
    
    colors
}

do.write.shaded.xlsx <- function(x,
                                 colors,
                                 file,
                                 text.colors = NULL,
                                 row.names=T,
                                 col.names=T,
                                 na='NA')
{
    if (!any(.packages(T)=='xlsx'))
        stop("'xlsx' package has not been loaded - unable to write a shaded xlsx file")
    
    start.row = 1-row.names
    start.col = 1-col.names
    
    wb = createWorkbook()
    sheet = createSheet(wb, "Sheet1")
    if (col.names)
    {
        names.for.column = colnames(x)
        if (is.null(names.for.column))
            names.for.column = 1:dim(x)[2]
        row = createRow(sheet, rowIndex=1)
        for (j in 1:length(names.for.column))
        {
            cell = createCell(row, colIndex = j + row.names)[[1,1]]
            setCellValue(cell, names.for.column[j])
        }
    }
    
    if (row.names)
    {
        names.for.row=rownames(x)
        if (is.null(names.for.row))
            names.for.row = 1:dim(x)[1]
    }
    for (i in 1:nrow(x))
    {
        row = createRow(sheet, rowIndex=i+col.names)
        if (row.names)
        {
            cell = createCell(row, colIndex=1)[[1,1]]
            setCellValue(cell, names.for.row[i])
        }
        
        for (j in 1:ncol(x))
        {
            if (!is.na(x[i,j]) || !is.null(na))
            {
                cell = createCell(row, colIndex=j+row.names)[[1,1]]
                if (is.na(x[i,j]))
                    setCellValue(cell, na)
                else                
                    setCellValue(cell, x[i,j])
                
                if (is.null(text.colors))
                {
                    cell.style <- CellStyle(wb) +
                        Fill(backgroundColor=colors[i,j], foregroundColor=colors[i,j],
                             pattern="SOLID_FOREGROUND") 
                }
                else
                {
                    cell.style <- CellStyle(wb,
                                            fill = Fill(backgroundColor = colors[i,j], foregroundColor=colors[i,j],
                                                        pattern="SOLID_FOREGROUND"),
                                            font = Font(wb, color = text.colors[i,j]))
                }
                setCellStyle(cell, cell.style) 
            }
        }
    }
    
    saveWorkbook(wb, file = file)
}


rgb.to.hex <- function(rgb, prepend='')
{
    rgb = round(rgb)
    if (is.null(dim(rgb)))
        rv = toupper(paste0(format(as.hexmode(rgb[1]), width=2), 
                            format(as.hexmode(rgb[2]), width=2), 
                            format(as.hexmode(rgb[3]), width=2)))
    else   
        rv = toupper(paste0(format(as.hexmode(rgb[,1]), width=2), 
                            format(as.hexmode(rgb[,2]), width=2), 
                            format(as.hexmode(rgb[,3]), width=2)))
    
    paste0(prepend, rv)
}

##--------------------------------##
##-- OLD (DEPRECATED) FUNCTIONS --##
##--------------------------------##

OLD.write.systematic.table <- function(estimates = table.v2,
                                   file = "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita",
                                   interventions,
                                   include.interval=F,
                                   below.threshold.min.color='red',
                                   below.threshold.max.color='yellow2',
                                   above.threshold.min.color='green3',
                                   above.threshold.max.color='green4',
                                   na.color='gray',
                                   threshold=0.9,
                                   digits=0)
{
    x = estimates$estimates
    
    int.names = sapply(interventions, get.intervention.name)
    int.codes = sapply(interventions, get.intervention.code)
    x = x[,int.codes]
    
    dim.names = list(location = unlist(msa.names(as.character(attr(estimates, 'location')))),
                     intervention = int.names)
    
    dim(x) = sapply(dim.names, length)
    dimnames(x) = dim.names
    #    dimnames(x)[[1]] = 
    #   dimnames(x)[[2]] = sapply(interventions, get.intervention.name)
    
    write.shaded.table(x=x,
                       file=file,
                       threshold=threshold,
                       below.threshold.min.color = below.threshold.min.color,
                       below.threshold.max.color = below.threshold.max.color,
                       above.threshold.min.color = above.threshold.min.color,
                       above.threshold.max.color = above.threshold.max.color,
                       na.color = na.color,
                       digits=digits,
                       as.pct=T)
}



#'
#'@description Writes a table to a MS Excel file with shading based on the values of the table. Uses two color scales - one for below a threshold value, and another before it
#'
#'@param x - A table-like R object (matrix, 2d array, data frame)
#'@param file - the name of the file where the xls file should be written
#'@param threshold - the numeric value below which the "below.threshold" color gradient is applied, and above which the "above.threshold" color gradient applies
#'@param below.threshold.min.color,below.threshold.max.color The colors for the endpoints of the gradient applied to values below the threshold value
#'@param above.threshold.min.color,above.threshold.max.color The colors for the endpoints of the gradient applied to values above the threshold value
#'@param min.value The value at which below.threshold.min.color applies (lower values also get shaded this same color)
#'@param max.value The value at which above.threshold.min.color applies (higher values also get shaded this same color)
#'@param as.pct Whether to format values as percentages
OLD.write.shaded.table <- function(x = table.v2,
                               file = "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita",
                               threshold=0.9,
                               below.threshold.min.color='red',
                               below.threshold.max.color='yellow2',
                               above.threshold.min.color='green3',
                               above.threshold.max.color='green4',
                               na.color='gray',
                               digits=0,
                               as.pct=T,
                               write.row.names=T,
                               write.col.names=T,
                               lowers=NULL,
                               uppers=NULL,
                               use.floor.not.round=T,
                               interval.open='[',
                               interval.close=']',
                               interval.spacer='-',
                               labels=NULL,
                               label.lower=NULL,
                               min.value=0,
                               max.value=1)
{
    if (as.pct)
    {
        mult = 100
        #        digits = digits + 2
        suffix = '%'
    }
    else
    {
        mult = 1
        suffix = ''
    }
    
    if (use.floor.not.round)
        format.function = function(z) {
            na.mask = is.na(z)
            rv = floor(mult * as.numeric(z) * 10^digits) / 10^digits
            rv = format(rv, nsmall=digits)
            rv[na.mask] = NA
            rv
        }
    else
        format.function = function(z) {
            na.mask = is.na(z)
            rv = format(round(mult*z, digits=digits), nsmall=digits)
            rv[na.mask] = NA
            rv
        }
    
    if (is.null(labels))
    {
        if (is.null(lowers) || is.null(uppers))
            labels = paste0(format.function(x), suffix)
        else
            labels = paste0(format.function(x), suffix, " ",
                            interval.open, format.function(lowers), 
                            interval.spacer,
                            format.function(uppers), interval.close)
        
        if (!is.null(label.lower))
            labels[x<label.lower] = paste0("<", format.function(label.lower), suffix)
        
        dim(labels) = dim(x)
        dimnames(labels) = dimnames(x)
    }
    
    #    labels = apply(x, 1:2, function(val){
    #       if (is.na(val))
    #          NA
    #     else
    #        paste0(mult*floor(as.numeric(val) * 10^digits)/10^digits, suffix)
    #})
    
    below.threshold.color = colorRamp(c(below.threshold.min.color, below.threshold.max.color))
    above.threshold.color = colorRamp(c(above.threshold.min.color, above.threshold.max.color))
    
    colors = apply(x, 1:2, function(val){
        if (is.na(val))
            na.color
        else if (val>=threshold)
        {
            scaled.val = (min(max.value,val)-threshold)/(max.value-threshold)
            rgb.to.hex(above.threshold.color(scaled.val), prepend='#')
        }
        else
        {
            scaled.val = max(min.value,as.numeric(val))/(threshold-min.value)
            rgb.to.hex(below.threshold.color(scaled.val), prepend='#')
        }
    })
    
    dimnames(labels)[[1]] = dimnames(x)[[1]]
    dimnames(labels)[[2]] = dimnames(x)[[2]]
    
    do.write.shaded.xlsx(x=labels,
                         colors=colors,
                         file=file,
                         text.colors = text.colors,
                         na=NULL,
                         row.names=write.row.names,
                         col.names=write.col.names)
    
}

