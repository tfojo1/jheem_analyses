if (!exists("total.incidence"))
    x = load("Q:/results/ryan_white/ryan_white_results_state_partb_2026_2025-10-23.Rdata")

# [year,sim,location,intervention]
# 2 locations (MS and total)
# 2 interventions (noint and part.b.rw.end.26)

YEARS.TO.CONSIDER = as.character(2026:2031)
END.NAME = 'part.b.rw.end.26' 

total.incidence = total.incidence[,,"MS",]
total.results = total.results[,,,"MS",]

inc.black = incidence.by.race[,"black",,,]
inc.hispanic = incidence.by.race[,"hispanic",,,]
inc.other = incidence.by.race[,"other",,,]

# figures
{
    PLOT.DIR = file.path("../../results/ryan_white", 'timeline_projections/')
    PLOT.HEIGHT = 1.3
    PLOT.WIDTH = 2.2
    PLOT.DPI = 600
    PLOT.DEVICE = 'png'
    
    TOTAL.TIMELINE.COLORS = c('noint'=RW.BASELINE.COLOR,
                        'part.b.rw.end.26'=RW.END.COLOR
                        )
    
    BLACK.TIMELINE.COLORS = c('noint'=RW.BASELINE.COLOR,
                              'part.b.rw.end.26'=RW.DATA.COLOR
    )
    
    HISPANIC.TIMELINE.COLORS = c('noint'=RW.BASELINE.COLOR,
                              'part.b.rw.end.26'=RW.P.INTR.COLOR
    )
    
    OTHER.TIMELINE.COLORS = c('noint'=RW.BASELINE.COLOR,
                                 'part.b.rw.end.26'=RW.B.INTR.COLOR
    )
    
    TIMELINE.LABELS = c('noint'='Continuation',
                        'part.b.rw.end.26'='Cessation'# ,
                        #'rw.b.intr'='Brief Interruption',
                        #'rw.p.intr'='Prolonged Interruption'
                        )
    
    make.time.plot <- function(locations,
                               intervention.codes,
                               intervention.colors = TIMELINE.COLORS,
                               intervention.labels = TIMELINE.LABELS,
                               outcome = 'incidence',
                               results.array = total.results,
                               years = 2018:2031,
                               label.years = 2026:2031,
                               data.color = RW.DATA.COLOR,
                               ci.coverage = 0.95,
                               y.label = 'Infections (n)',
                               text.label = 'infections',
                               base.size = 8,
                               label.size = 2,
                               show.legend = F)
    {
        alpha = (1-ci.coverage)/2
        if("outcome" %in% names(dimnames(results.array)))
            sim.data = apply(results.array[as.character(years),,outcome,,drop=F],
                             c('year','sim','intervention'), sum)
        else{
            sim.data = results.array[as.character(years),,]
        }
        
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
    
    plot = make.time.plot(location="MS", 
                          intervention.colors = TOTAL.TIMELINE.COLORS,
                          intervention.codes = c('noint','part.b.rw.end.26')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'MS_PartB_inf.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location="MS", 
                          outcome = "new",
                          text.label = "new diagnoses",
                          intervention.codes = c('noint','part.b.rw.end.26')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'MS_PartB_new.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location="MS", 
                          results.array = inc.black,
                          intervention.colors = BLACK.TIMELINE.COLORS,
                          intervention.codes = c('noint','part.b.rw.end.26')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'MS_PartB_black.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location="MS", 
                          results.array = inc.hispanic,
                          intervention.colors = HISPANIC.TIMELINE.COLORS,
                          intervention.codes = c('noint','part.b.rw.end.26')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'MS_PartB_hispanic.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot = make.time.plot(location="MS", 
                          results.array = inc.other,
                          intervention.colors = OTHER.TIMELINE.COLORS,
                          intervention.codes = c('noint','part.b.rw.end.26')); print(plot)
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, 'MS_PartB_other.png'),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    

    
}

# text results
{
    #-- Description if RW Continues --#
    
    print("RYAN WHITE CONTINUES: ")
    
    tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,'noint',drop=F], c('sim'), sum, na.rm=T)
    
    print(paste0("Infections 2026-2031 if Ryan White Continues: ", 
                 format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
                 " [",
                 format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    
    print("")
    print("CESSATION (Primary): ")
    
    total.suppression = apply(total.results[YEARS.TO.CONSIDER,,'suppression',], c('year','sim','intervention'), sum) / apply(total.results[YEARS.TO.CONSIDER,,'diagnosed.prevalence',], c('year','sim','intervention'), sum)
    print(paste0("Total Suppression pre-drop in 2026: ",
                 round(100*mean(total.suppression['2026',,'noint'])), '%',
                 " [", round(100*quantile(total.suppression['2026',,'noint'], probs=.025)),
                 " - ", round(100*quantile(total.suppression['2026',,'noint'], probs=.975)),
                 "%]"))
    print(paste0("Total Suppression post-drop in 2027: ",
                 round(100*mean(total.suppression['2027',,END.NAME])), '%',
                 " [", round(100*quantile(total.suppression['2027',,END.NAME], probs=.025)),
                 " - ", round(100*quantile(total.suppression['2027',,END.NAME], probs=.975)),
                 "%]"))
    
    abs.delta.cessation.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
    rel.delta.cessation.tot.inf.noint1 = abs.delta.cessation.tot.inf.noint1 / tot.inf.noint1
    
    print(paste0("Absolute Increase in Infections 2026-2031 if Ryan White Ends Indefinitely:", 
                 format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
                 " [",
                 format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
                 " - ",
                 format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
                 "]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
                 format(round(mean(rel.delta.cessation.tot.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    
    
    print("")
    print("INCIDENCE BY SUBGROUP:")
    
    #years.incidence.by.race = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('race','sim','location','intervention'), sum)
    years.incidence.by.race = apply(incidence.by.race[YEARS.TO.CONSIDER,,,, ,drop = F], c('race','sim','location','intervention'), sum)
    
    inf.by.race.noint1 = apply(years.incidence.by.race[,,,'noint',drop=F], c('race','sim'), sum, na.rm=T)
    abs.delta.cessation.race.inf.noint1 = apply(years.incidence.by.race[,,,END.NAME,drop=F], c('race','sim'), sum, na.rm=T) - inf.by.race.noint1
    rel.delta.cessation.race.inf.noint1 = abs.delta.cessation.race.inf.noint1 / inf.by.race.noint1
    
    mean.ci.rel.delta.cessation.race.inf.noint1 = cbind(
        mean = rowMeans(rel.delta.cessation.race.inf.noint1),
        lower = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.025),
        upper = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.975)
    )
    
    print(paste0("Relative Increase in Infections 2026-2031 for Black residents in Cessation: ", 
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',1]*100), big.mark=','),
                 "% [",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',2]*100), big.mark=','),
                 " - ",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',3]*100), big.mark=','),
                 "%]"))
    print(paste0("Relative Increase in Infections 2026-2031 for Hispanic residents in Cessation: ", 
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',1]*100), big.mark=','),
                 "% [",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',2]*100), big.mark=','),
                 " - ",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',3]*100), big.mark=','),
                 "%]"))
    print(paste0("Relative Increase in Infections 2026-2031 for Other residents in Cessation: ", 
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',1]*100), big.mark=','),
                 "% [",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',2]*100), big.mark=','),
                 " - ",
                 format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',3]*100), big.mark=','),
                 "%]"))
    
    #years.incidence.by.age = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('age','sim','location','intervention'), sum)
    years.incidence.by.age = apply(incidence.by.age[YEARS.TO.CONSIDER,,,,, drop = F], c('age','sim','location','intervention'), sum)
    
    inf.by.age.noint1 = apply(years.incidence.by.age[,,,'noint',drop=F], c('age','sim'), sum, na.rm=T)
    abs.delta.cessation.age.inf.noint1 = apply(years.incidence.by.age[,,,END.NAME,drop=F], c('age','sim'), sum, na.rm=T) - inf.by.age.noint1
    rel.delta.cessation.age.inf.noint1 = abs.delta.cessation.age.inf.noint1 / inf.by.age.noint1
    
    mean.ci.rel.delta.cessation.age.inf.noint1 = cbind(
        mean = rowMeans(rel.delta.cessation.age.inf.noint1),
        lower = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.025),
        upper = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.975)
    )
    
    
    #incidence.not.age1 = apply(full.results[YEARS.TO.CONSIDER,-1,,,,,'incidence',,], c('sim','location','intervention'), sum)
    incidence.not.age1 = apply(incidence.by.age[YEARS.TO.CONSIDER,-1,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.age1 = apply(incidence.by.age[YEARS.TO.CONSIDER,1,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.age2 = apply(incidence.by.age[YEARS.TO.CONSIDER,2,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.age3 = apply(incidence.by.age[YEARS.TO.CONSIDER,3,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.age4 = apply(incidence.by.age[YEARS.TO.CONSIDER,4,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.age5 = apply(incidence.by.age[YEARS.TO.CONSIDER,5,,,, drop = F], c('age','sim','location','intervention'), sum)
    incidence.not.age5 = apply(incidence.by.age[YEARS.TO.CONSIDER,-5,,,, drop = F], c('age','sim','location','intervention'), sum)
    
    inf.not.age1.noint1 = apply(incidence.not.age1[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.age1.noint1 = apply(incidence.age1[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.age2.noint1 = apply(incidence.age2[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.age3.noint1 = apply(incidence.age3[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.age4.noint1 = apply(incidence.age4[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.age5.noint1 = apply(incidence.age5[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
    inf.not.age5.noint1 = apply(incidence.not.age5[,,,'noint',drop=F], c('sim'), sum, na.rm=T)

    abs.delta.cessation.not.age1.inf.noint1 = apply(incidence.not.age1[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.not.age1.noint1
    rel.delta.cessation.not.age1.inf.noint1 = abs.delta.cessation.not.age1.inf.noint1 / inf.not.age1.noint1

    abs.delta.cessation.age1.inf.noint1 = apply(incidence.age1[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.age1.noint1
    rel.delta.cessation.age1.inf.noint1 = abs.delta.cessation.age1.inf.noint1 / inf.age1.noint1
    
    abs.delta.cessation.age2.inf.noint1 = apply(incidence.age2[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.age2.noint1
    rel.delta.cessation.age2.inf.noint1 = abs.delta.cessation.age2.inf.noint1 / inf.age2.noint1
    
    abs.delta.cessation.age3.inf.noint1 = apply(incidence.age3[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.age3.noint1
    rel.delta.cessation.age3.inf.noint1 = abs.delta.cessation.age3.inf.noint1 / inf.age3.noint1
    
    abs.delta.cessation.age4.inf.noint1 = apply(incidence.age4[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.age4.noint1
    rel.delta.cessation.age4.inf.noint1 = abs.delta.cessation.age4.inf.noint1 / inf.age4.noint1
    
    abs.delta.cessation.age5.inf.noint1 = apply(incidence.age5[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.age5.noint1
    rel.delta.cessation.age5.inf.noint1 = abs.delta.cessation.age5.inf.noint1 / inf.age5.noint1
    
    abs.delta.cessation.not.age5.inf.noint1 = apply(incidence.not.age5[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.not.age5.noint1
    rel.delta.cessation.not.age5.inf.noint1 = abs.delta.cessation.not.age5.inf.noint1 / inf.not.age5.noint1
    

    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 1 in Cessation: ", 
                 format(round(mean(rel.delta.cessation.age1.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.age1.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.age1.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 2 in Cessation: ", 
                 format(round(mean(rel.delta.cessation.age2.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.age2.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.age2.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 3 in Cessation: ", 
                 format(round(mean(rel.delta.cessation.age3.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.age3.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.age3.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 4 in Cessation: ", 
                 format(round(mean(rel.delta.cessation.age4.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.age4.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.age4.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 5 in Cessation: ", 
                 format(round(mean(rel.delta.cessation.age5.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.age5.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.age5.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 1-4 in Cessation: ",
                 format(round(mean(rel.delta.cessation.not.age5.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.not.age5.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.not.age5.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    
    # old way: age 1 versus all not age 1:
    # print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 1 in Cessation: ", 
    #              format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,1]*100), big.mark=','),
    #              "% [",
    #              format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,2]*100), big.mark=','),
    #              " - ",
    #              format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,3]*100), big.mark=','),
    #              "%]"))
    # 
    # print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 2-4 in Cessation: ", 
    #              format(round(mean(rel.delta.cessation.not.age1.inf.noint1, na.rm=T)*100), big.mark=','),
    #              "% [",
    #              format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
    #              " - ",
    #              format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
    #              "%]"))
    
    
    #years.incidence.by.sex = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sex','sim','location','intervention'), sum)
    years.incidence.by.sex = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,,,,,, drop = F], c('sex','sim','location','intervention'), sum)
    
    inf.by.sex.noint1 = apply(years.incidence.by.sex[,,,'noint',drop=F], c('sex','sim'), sum, na.rm=T)
    abs.delta.cessation.sex.inf.noint1 = apply(years.incidence.by.sex[,,,END.NAME,drop=F], c('sex','sim'), sum, na.rm=T) - inf.by.sex.noint1
    rel.delta.cessation.sex.inf.noint1 = abs.delta.cessation.sex.inf.noint1 / inf.by.sex.noint1
    
    mean.ci.rel.delta.cessation.sex.inf.noint1 = cbind(
        mean = rowMeans(rel.delta.cessation.sex.inf.noint1),
        lower = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.025),
        upper = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.975)
    )
    
    
    #years.incidence.by.risk = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('risk','sim','location','intervention'), sum)
    years.incidence.by.risk = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,,,,,, drop = F], c('risk','sim','location','intervention'), sum)
    
    inf.by.risk.noint1 = apply(years.incidence.by.risk[,,,'noint',drop=F], c('risk','sim'), sum, na.rm=T)
    abs.delta.cessation.risk.inf.noint1 = apply(years.incidence.by.risk[,,,END.NAME,drop=F], c('risk','sim'), sum, na.rm=T) - inf.by.risk.noint1
    rel.delta.cessation.risk.inf.noint1 = abs.delta.cessation.risk.inf.noint1 / inf.by.risk.noint1
    
    mean.ci.rel.delta.cessation.risk.inf.noint1 = cbind(
        mean = rowMeans(rel.delta.cessation.risk.inf.noint1),
        lower = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.025),
        upper = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.975)
    )
    
    #incidence.msm = apply(full.results[YEARS.TO.CONSIDER,,,'msm','never_IDU',,'incidence',,], c('sim','location','intervention'), sum)
    #incidence.msm = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,'msm','never_IDU',,,], c('sim','location','intervention'), sum)
    incidence.msm = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,'msm',,,,, drop =F], c('sim','location','intervention'), sum)
    print("Keeping MSM-IDU with MSM")
    
    inf.msm.noint1 = apply(incidence.msm[,,'noint',drop=F], c('sim'), sum, na.rm=T)
    abs.delta.cessation.msm.inf.noint1 = apply(incidence.msm[,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.msm.noint1
    rel.delta.cessation.msm.inf.noint1 = abs.delta.cessation.msm.inf.noint1 / inf.msm.noint1
    
    print(paste0("Relative Increase in Infections AMONG MSM 2026-2031 FOR MSM in Cessation: ", 
                 format(round(mean(rel.delta.cessation.msm.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    #inf.total = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sim','location','intervention'), sum)
    inf.total = apply(total.results[YEARS.TO.CONSIDER,,'incidence',, drop = F], c('sim','intervention'), sum)
    #inf.non.msm.noint1 = apply(inf.total[,,'noint'], 'sim', sum) - inf.msm.noint1
    inf.non.msm.noint1 = (inf.total[,'noint']) - inf.msm.noint1
    abs.delta.cessation.non.msm.inf.noint1 = (inf.total[,END.NAME,drop=F]) - apply(incidence.msm[,,END.NAME,drop=F], c('sim'), sum, na.rm=T)  - inf.non.msm.noint1
    rel.delta.cessation.non.msm.inf.noint1 = abs.delta.cessation.non.msm.inf.noint1 / inf.non.msm.noint1
    
    print(paste0("Relative Increase in Infections AMONG NON-MSM 2026-2031 FOR Non-MSM in Cessation: ", 
                 format(round(mean(rel.delta.cessation.non.msm.inf.noint1, na.rm=T)*100), big.mark=','),
                 "% [",
                 format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    
    
    
    
    
    # State-level results: making sure it's just Mississippi
    if(1==2){
        n.cities = 1#nrow(mean.ci.rel.total.infections.averted.end.by.city)
        
        abs.total.infections.averted.end.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
        rel.total.infections.averted.end.by.city = abs.total.infections.averted.end.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
        mean.ci.rel.total.infections.averted.end.by.city = cbind(
            mean = apply(rel.total.infections.averted.end.by.city, 'location', mean, na.rm=T),
            lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.025, na.rm=T),
            upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.975, na.rm=T)
        )
        mean.ci.rel.total.infections.averted.end.by.city = mean.ci.rel.total.infections.averted.end.by.city[order(mean.ci.rel.total.infections.averted.end.by.city[,1]),]
        
        
        print(paste0("Smallest ", RW.LOCATION.DESCRIPTOR, "-Level Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
                     get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[1]), " - ",
                     format(round(mean.ci.rel.total.infections.averted.end.by.city[1,1]*100), big.mark=','),
                     "% [",
                     format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,2], probs=.025, na.rm=T)*100), big.mark=','),
                     " - ",
                     format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,3], probs=.975, na.rm=T)*100), big.mark=','),
                     "%]"))
        
        
        print(paste0("Largest ", RW.LOCATION.DESCRIPTOR, "-Level Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
                     get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[n.cities]), " - ",
                     format(round(mean.ci.rel.total.infections.averted.end.by.city[n.cities,1]*100), big.mark=','),
                     "% [",
                     format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,2], probs=.025, na.rm=T)*100), big.mark=','),
                     " - ",
                     format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,3], probs=.975, na.rm=T)*100), big.mark=','),
                     "%]"))  
    }
    
    
      
}
