
# library(ggplot2)

# source('applications/ryan_white/ryan_white_main.R')

join.caption.to.plot <- function(plot,
                                 labels,
                                 label.colors)
{
    if (is.null(names(label.colors)))
        names(label.colors) = as.character(1:length(label.colors))
    
    df = data.frame(
        label = labels,
        catg = names(label.colors),
        x = rep(0, length(labels)),
        y = -(1:length(labels))
    )
    
    label.plot = ggplot(df, aes(x,y,label=label,color=catg)) +
        geom_text() +
        theme_bw() +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              line = element_blank(), 
              rect = element_blank(),
              axis.text = element_blank()
        ) +
        scale_color_manual(values = label.colors, guide=NULL)
    
    gridExtra::arrangeGrob(plot,
                           label.plot)
}

join.two.captions.to.plot <- function(plot,
                                      label1,
                                      color1,
                                      label2,
                                      color2,
                                      height1,
                                      height2)
{
    label.colors = c(one=color1, two=color2)
    
    df = data.frame(
        label = c(label1, label2),
        catg = names(label.colors),
        x = 0,
        y = 0
    )
    
    label.plot = ggplot(mapping = aes(x,y,label=label,color=catg)) +
        geom_text(data = df[1,], vjust = 1) +
        geom_text(data = df[2,], vjust = 0) +
        theme_bw() +
        theme(axis.title = element_blank(),
              panel.grid = element_blank(),
              line = element_blank(), 
              rect = element_blank(),
              axis.text = element_blank()
        ) +
        scale_color_manual(values = label.colors, guide=NULL)
    
    gridExtra::arrangeGrob(plot,
                                    label.plot,
                                    heights = c(height1,height2))
}

joined = join.two.captions.to.plot(plot=plot,
                                   label1 = 'X [Y-Z]', 
                                   label2 = 'X2 [Y2-Z2]',
                                   color1 = RW.EXP.COLOR, 
                                   color2 = RW.NONEXP.COLOR)


grid::showGrob(joined)

ggsave(plot=joined, filename = 'joined.png')
