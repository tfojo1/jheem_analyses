
#source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

PLOT.DIR = file.path('../../results/ryan_white/timeline_projections/')
PLOT.HEIGHT = 1.5
PLOT.WIDTH = 2
PLOT.DPI = 600
PLOT.DEVICE = 'png'

make.time.plot <- function(locations,
                            intervention.codes,
                            intervention.colors = c('noint'=RW.BASELINE.COLOR,
                                                    'rw.end'=RW.END.COLOR,
                                                    'rw.b.intr'=RW.B.INTR.COLOR,
                                                    'rw.p.inter'=RW.P.INTR.COLOR),
                            intervention.labels = c('noint'='Continuation',
                                                    'rw.end'='Cessation',
                                                    'rw.b.intr'='Brief Interruption',
                                                    'rw.p.inter'='Prolonged Interruption'),
                            outcome = 'incidence',
                            years = 2010:2035,
                           label.years = 2025:2030,
                            data.color = RW.DATA.COLOR,
                            ci.coverage = 0.95,
                           y.label = 'Infections (n)',
                           text.label = 'infections',
                           base.size = 8,
                           label.size = 2)
{
    alpha = (1-ci.coverage)/2
  
    sim.data = apply(full.results[as.character(years),,,,,,outcome,locations,,drop=F],
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
    
    label.x.year = min(label.years)-1
    label.xend.year1 = ceiling((max(years) + label.x.year+1) / 2) + 1
    label.xend.year2 = label.years[order(sim.means[as.character(label.years),2], decreasing=T)[1]]
    label.xend.year = min(label.xend.year1, label.xend.year2)
    df.label = data.frame(
      #abs.value = paste0(format(round(mean(abs.delta)), big.mark=','))
      label = paste0(round(100*mean(rel.delta)), '% [',
                     round(100*quantile(rel.delta, probs=alpha)), "-",
                     round(100*quantile(rel.delta, probs=1-alpha)), "%]\nmore ",
                     text.label),
      
      x = label.x.year,
      y = (y.max + sim.means[as.character(label.x.year),1]) / 2,
      xend = label.xend.year,
      yend = (sim.means[as.character(label.xend.year),1] + sim.means[as.character(label.xend.year),2]) / 2
    )

    ggplot() +
      geom_ribbon(data=sim.df, 
                  aes(year, estimate, ymin=lower, ymax=upper,
                      color=intervention, fill=intervention),
                  alpha=0.2, linewidth = 0.02) +
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
                nudge_x = -0.25,
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
      xlab(NULL) +
      theme_bw(base_size = base.size)
}


plot = make.time.plot(location=HOUSTON.MSA, 
                      intervention.codes = c('noint','rw.end')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'houston_inc_end.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=HOUSTON.MSA, 
                      intervention.codes = c('noint','rw.b.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'houston_inc_b.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=HOUSTON.MSA, 
                      intervention.codes = c('noint','rw.p.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'houston_inc_p.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = make.time.plot(location=RW.MEDICAID.EXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.end')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'expansion_inc_end.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.MEDICAID.EXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.b.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'expansion_inc_b.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.MEDICAID.EXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.p.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'expansion_inc_p.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = make.time.plot(location=RW.MEDICAID.NONEXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.end')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'nonexpansion_inc_end.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.MEDICAID.NONEXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.b.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'nonexpansion_inc_b.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.MEDICAID.NONEXPANSION.CITIES, 
                      intervention.codes = c('noint','rw.p.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'nonexpansion_inc_p.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = make.time.plot(location=RW.LOCATIONS, 
                      intervention.codes = c('noint','rw.end')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'all_inc_end.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.LOCATIONS, 
                      intervention.codes = c('noint','rw.b.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'all_inc_b.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=RW.LOCATIONS, 
                      intervention.codes = c('noint','rw.p.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'all_inc_p.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Texas MSAs
texas.msas = RW.LOCATIONS[city.main.state=='TX']

plot = make.time.plot(location=texas.msas, 
                      intervention.codes = c('noint','rw.end')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'texas_inc_end.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=texas.msas, 
                      intervention.codes = c('noint','rw.b.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'texas_inc_b.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = make.time.plot(location=texas.msas, 
                      intervention.codes = c('noint','rw.p.intr')); print(plot)
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'texas_inc_p.intr.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

