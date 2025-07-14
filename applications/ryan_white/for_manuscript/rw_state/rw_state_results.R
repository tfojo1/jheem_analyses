library(ggrepel)

# first make sure RW.IS.STATE.LEVEL = T in ryan_white_main.R
source("../jheem_analyses/applications/ryan_white/for_manuscript/load_survey_results.R")
print(paste0("RW.IS.STATE.LEVEL is set to ",RW.IS.STATE.LEVEL))

# text results 
source("../jheem_analyses/applications/ryan_white/for_manuscript/text_results.R")

# figures 
{
    source("../jheem_analyses/applications/ryan_white/for_manuscript/city_by_city_results_horizontal.R")
    # this won't work until I fix the xlsx issue (I think I need to fix java setup?)
    # for now, just run all lines except 336-349, will save a figure in the directory ../../Ryan White..."
    
    # this makes the scatterplot figures, but they won't be labeled since these labels are for cities
    source("../jheem_analyses/applications/ryan_white/for_manuscript/between_city_variation_secondary.R")
}

urbanicity <- get(load("../../cached/urban_metric.RData"))
names(urbanicity) = c("Alabama","California","Florida","Georgia","Illinois","Louisiana","Missouri",
                         "Mississippi","New.York","Texas","Wisconsin")

# df.city.summ comes from betwee_city_variation_secondary.R; run through line 106
df.city.summ$urbanicity = c(as.numeric(urbanicity))

rw.clients.corr = cor(df.city.summ$excess, df.city.summ$rw.clients)
urbanicity.corr = cor(df.city.summ$excess, df.city.summ$urbanicity)
sexual.transmission.corr = cor(df.city.summ$excess, df.city.summ$sexual.transmission)
new.per.pop.corr = cor(df.city.summ$excess, df.city.summ$new.per.pop)
supp.corr = cor(df.city.summ$excess, df.city.summ$suppression)

corr.labels.df = data.frame(
    value = paste0("One-way Correlation:", format(round(c(rw.clients.corr,
                                                   urbanicity.corr,
                                                   new.per.pop.corr,
                                                   sexual.transmission.corr,
                                                   supp.corr
                                                   ),2), nsmall = 2)),
    x = c(.58,.6,2.4e-04,0.63,0.85),
    y = c(0.05),
    var = c("rw.clients","urbanicity","new.per.pop","sexual.transmission","viral.suppression")
)

LEGEND.SIZE = 2.5

# % RW clients (A)
plot = ggplot() + 
    geom_point(data = df.city.summ, 
               aes(rw.clients, excess, size=new, fill=medicaid), 
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of HIV+ Residents\nReceiving Ryan White Services in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    #geom_segment(data = df.label, aes(x, y, xend=rw.clients, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.32)) +
    scale_x_continuous(labels = scales::percent, limits = c(.25,1)) +
    geom_text_repel(data = df.city.summ, 
              aes(rw.clients, excess,label = location)) + 
    geom_label(data=corr.labels.df[corr.labels.df$var=="rw.clients",],
               aes(x, y, label=value), size=LEGEND.SIZE,
               vjust = 0.5, hjust = 0, show.legend = F); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4A.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# sexual transmission (B)
plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(sexual.transmission, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Average Transmission\nRate in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    #geom_segment(data = df.label, aes(x, y, xend=sexual.transmission, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.32)) +
    scale_x_continuous(limits = c(0,0.63)) +
    geom_text_repel(data = df.city.summ, 
                    aes(sexual.transmission, excess,label = location)) + 
    geom_label(data=corr.labels.df[corr.labels.df$var=="sexual.transmission",],
               aes(x, y, label=value), size=LEGEND.SIZE,
               vjust = 0.5, hjust = 1, show.legend = F); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4B.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# viral suppression (c)
plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(suppression, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of All HIV+ Residents\nVirally Suppressed in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    #geom_segment(data = df.label, aes(x, y, xend=viral.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.32)) +
    scale_x_continuous(labels = scales::percent, limits = c(0.7,0.85)) +
    geom_text_repel(data = df.city.summ, 
                    aes(suppression, excess,label = location)) + 
    geom_label(data=corr.labels.df[corr.labels.df$var=="viral.suppression",],
               aes(x, y, label=value), size=LEGEND.SIZE,
               vjust = 0.5, hjust = 1, show.legend = F); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4C.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# new per pop (D)
plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(new.per.pop, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("New HIV Diagnoses in 2025\nper 100,000 population") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    #geom_segment(data = df.label, aes(x, y, xend=new.per.pop, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.32)) +
    scale_x_continuous(labels = function(x){format(x * 100000, big.mark=',')}, limits = c(2e-05,2.4e-04)) +
    geom_text_repel(data = df.city.summ, 
                    aes(new.per.pop, excess,label = location)) + 
    geom_label(data=corr.labels.df[corr.labels.df$var=="new.per.pop",],
               aes(x, y, label=value), size=LEGEND.SIZE,
               vjust = 0.5, hjust = 1, show.legend = F); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4D.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Urbanicity (E)
plot = ggplot() + 
    geom_point(data = df.city.summ, 
               aes(urbanicity, excess, size=new, fill=medicaid), 
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of Total Residents\nResiding in an Urban Environment") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    #geom_segment(data = df.label, aes(x, y, xend=rw.clients, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.32)) +
    scale_x_continuous(labels = scales::percent, limits = c(0.3,1)) +
    geom_text_repel(data = df.city.summ, 
                    aes(urbanicity, excess,label = location)) + 
    geom_label(data=corr.labels.df[corr.labels.df$var=="urbanicity",],
               aes(x, y, label=value), size=LEGEND.SIZE,
               vjust = 0.5, hjust = 0, show.legend = F); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4E.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# Excess infections by number of states
plot = ggplot() + 
    geom_histogram(data=df.city.summ, 
                   aes(excess, fill=medicaid), show.legend = F,
                   position = 'dodge',
                   bins = 12) + 
    xlab("Relative Excess HIV Infections") +
    ylab("Number of States") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR + 
    scale_x_continuous(labels = scales::percent); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_S4F.png"),
       height = PLOT.HEIGHT/2, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

