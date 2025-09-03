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
    
    # this makes the scatterplot figures, but they won't be labeled since these labels are for cities; use code below
    source("../jheem_analyses/applications/ryan_white/for_manuscript/between_city_variation_secondary.R")
}

# looking at Kelly's request
{
    # Incidence 
    annual.inc.noint = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','year'), sum, na.rm=T)
    annual.inc.rw.ends = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end',drop=F], c('sim','year'), sum, na.rm=T)
    abs.delta.cessation.annual.inf.noint1 = annual.inc.rw.ends - annual.inc.noint
    print(paste0("Absolute Increase in Infections 2025-2030 if Ryan White Ends Indefinitely:", 
                 format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
                 " [",
                 format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    print(paste0(c(2025:2030)," absolute increase in infections if Ryan White Ends Indefintely: ", 
                 format(round(apply(abs.delta.cessation.annual.inf.noint1, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(abs.delta.cessation.annual.inf.noint1, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(abs.delta.cessation.annual.inf.noint1, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    # just to check the plot
    print(paste0(c(2025:2030)," new infections if Ryan White Continues: ", 
                 format(round(apply(annual.inc.noint, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(annual.inc.noint, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(annual.inc.noint, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    print(paste0(c(2025:2030)," new infections if Ryan White Ends Indefinitely: ", 
                 format(round(apply(annual.inc.rw.ends, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(annual.inc.rw.ends, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(annual.inc.rw.ends, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    # Diagnoses 
    annual.new.noint = apply(total.new[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','year'), sum, na.rm=T)
    annual.new.rw.ends = apply(total.new[YEARS.TO.CONSIDER,,,'rw.end',drop=F], c('sim','year'), sum, na.rm=T)
    abs.delta.cessation.annual.new.noint1 = annual.new.rw.ends - annual.new.noint
    print(paste0("Absolute Increase in Diagnoses 2025-2030 if Ryan White Ends Indefinitely:", 
                 format(round(mean(abs.delta.cessation.tot.dx.noint1, na.rm=T)), big.mark=','),
                 " [",
                 format(round(quantile(abs.delta.cessation.tot.dx.noint1, probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(quantile(abs.delta.cessation.tot.dx.noint1, probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    print(paste0(c(2025:2030)," absolute increase in diagnoses if Ryan White Ends Indefintely: ", 
                 format(round(apply(abs.delta.cessation.annual.new.noint1, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(abs.delta.cessation.annual.new.noint1, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(abs.delta.cessation.annual.new.noint1, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    # just to check the plot
    print(paste0(c(2025:2030)," new diagnoses if Ryan White Continues: ", 
                 format(round(apply(annual.new.noint, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(annual.new.noint, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(annual.new.noint, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    print(paste0(c(2025:2030)," new diagnoses if Ryan White Ends Indefinitely: ", 
                 format(round(apply(annual.new.rw.ends, c('year'),mean, na.rm=T)), big.mark=','),
                 " [",
                 format(round(apply(annual.new.rw.ends, c('year'),quantile,probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(apply(annual.new.rw.ends, c('year'),quantile,probs=.975, na.rm=T)), big.mark=','),
                 "]"))
}


# Scatter plots 
{
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
    
}


# Infections over time 
{
    PLOT.DIR = file.path(RW.ROOT.PLOT.DIR, 'timeline_projections/')
    PLOT.HEIGHT = 1.3
    PLOT.WIDTH = 2.2
    PLOT.DPI = 600
    PLOT.DEVICE = 'png'
    
    TIMELINE.COLORS = c('noint'=RW.BASELINE.COLOR,
                        'rw.end'=RW.END.COLOR,
                        'rw.b.intr'=RW.B.INTR.COLOR,
                        'rw.p.intr'=RW.P.INTR.COLOR)
    TIMELINE.LABELS = c('noint'='Continuation',
                        'rw.end'='Cessation',
                        'rw.b.intr'='Brief Interruption',
                        'rw.p.intr'='Prolonged Interruption')
    
    make.time.plot <- function(locations,
                               intervention.codes,
                               intervention.colors = TIMELINE.COLORS,
                               intervention.labels = TIMELINE.LABELS,
                               outcome = 'incidence',
                               years = 2018:2030,
                               label.years = 2025:2030,
                               data.color = RW.DATA.COLOR,
                               ci.coverage = 0.95,
                               y.label = 'Infections (n)',
                               text.label = 'infections',
                               base.size = 8,
                               label.size = 2,
                               show.legend = F)
    {
        alpha = (1-ci.coverage)/2
        
        # sim.data = apply(full.results[as.character(years),,,,,,outcome,locations,,drop=F],
        #                  c('year','sim','intervention'), sum)
        sim.data = apply(total.results[as.character(years),,outcome,locations,,drop=F],
                         c('year','sim','intervention'), sum)
        
        y.max = max(sim.data, na.rm=T)
        y.max = ceiling(y.max/100) * 100
        
        sim.means = apply(sim.data[,,intervention.codes,drop=F], c('year','intervention'), mean, na.rm=T)
        sim.lowers = apply(sim.data[,,intervention.codes,drop=F], c('year','intervention'), 
                           quantile, probs=alpha, na.rm=T)
        sim.uppers = apply(sim.data[,,intervention.codes,drop=F], c('year','intervention'),
                           quantile, probs=1-alpha, na.rm=T)
        
        sim.df = reshape2::melt(sim.means, value.name = 'estimate')
        sim.df$lower = as.numeric(sim.lowers)
        sim.df$upper = as.numeric(sim.uppers)
        
        sim.df = as.data.frame(sim.df)
        
        abs.delta = apply(sim.data[as.character(label.years),,intervention.codes[2],drop=F], 'sim', sum) -
            apply(sim.data[as.character(label.years),,intervention.codes[1],drop=F], 'sim', sum)
        rel.delta = abs.delta / apply(sim.data[as.character(label.years),,intervention.codes[1],drop=F], 'sim', sum)
        
        label.x.year = min(label.years)
        label.xend.year1 = ceiling((max(years) + label.x.year+1) / 2) + 1
        label.xend.year2 = label.years[order(sim.means[as.character(label.years),2], decreasing=T)[1]]
        label.xend.year = min(label.xend.year1, label.xend.year2)
        df.label = data.frame(
            #abs.value = paste0(format(round(mean(abs.delta)), big.mark=','))
            # label = paste0(round(100*mean(rel.delta)), '% [',
            #                round(100*quantile(rel.delta, probs=alpha)), "-",
            #                round(100*quantile(rel.delta, probs=1-alpha)), "%]\nmore ",
            #                text.label),
            label = paste0(format(round(mean(abs.delta)), big.mark=','), ' [',
                           format(round(quantile(abs.delta, probs=alpha)), big.mark=','), "-",
                           format(round(quantile(abs.delta, probs=1-alpha)), big.mark=','), "]\nmore ",
                           text.label, "   "),
            x = label.x.year,
            y = (y.max + sim.means[as.character(label.x.year),1]) / 2,
            xend = label.xend.year,
            yend = (sim.means[as.character(label.xend.year),1] + sim.means[as.character(label.xend.year),2]) / 2
        )
        
        plot = ggplot() +
            geom_ribbon(data=sim.df, 
                        aes(year, estimate, ymin=lower, ymax=upper,
                            color=intervention, fill=intervention),
                        alpha=0.25, linewidth = 0.02) +
            geom_line(data=sim.df, 
                      aes(year, estimate, ymin=lower, ymax=upper,
                          color=intervention, fill=intervention),
                      linewidth = 0.25) +
            geom_segment(data=df.label,
                         aes(x=x, y=y, xend=xend, yend=yend),
                         linetype = 'dotted', linewidth=0.1) +
            geom_text(data = df.label, 
                      aes(label=label, x=x, y=y),
                      hjust = 1,
                      vjust = 0,
                      nudge_x = 0.5,
                      size = label.size) +
            scale_fill_manual(name = NULL,
                              values = intervention.colors,
                              labels = intervention.labels, 
                              guide = NULL) +
            scale_color_manual(name = NULL,
                               values = intervention.colors,
                               labels = intervention.labels, 
                               guide = NULL) +
            scale_y_continuous(labels = function(x){format(x, big.mark=',')},
                               limits = c(0,y.max),
                               name = NULL) +
            scale_x_continuous(breaks = years[years%%5==0]) +
            xlab(NULL) +
            theme_bw(base_size = base.size)
    }
    
    plot = make.time.plot(location=RW.LOCATIONS, 
                          intervention.codes = c('noint','rw.end')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'Figure_2A.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location=RW.LOCATIONS, 
                          intervention.codes = c('noint','rw.p.intr')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'Figure_2B.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location=RW.LOCATIONS, 
                          intervention.codes = c('noint','rw.b.intr')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'Figure_2C.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

}

