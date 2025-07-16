
PLOT.DIR = file.path(paste0('../../results/ryan_white/kernel_description'))
PLOT.HEIGHT = 1.3
PLOT.WIDTH = 2.5
PLOT.DPI = 600
PLOT.DEVICE = 'png'

library(ggplot2)
source('applications/ryan_white/ryan_white_main.R')

means = cbind(apply(RW.effect.values, 1, mean)[c(1,4,2,5,3,6)],
                apply(adjusted.RW.effect.values, 1, mean)[c(1,4,2,5,3,6)])

lowers = cbind(apply(RW.effect.values, 1, quantile, probs=0.25)[c(1,4,2,5,3,6)],
               apply(adjusted.RW.effect.values, 1, quantile, probs=0.25)[c(1,4,2,5,3,6)])

uppers = cbind(apply(RW.effect.values, 1, quantile, probs=0.75)[c(1,4,2,5,3,6)],
               apply(adjusted.RW.effect.values, 1, quantile, probs=0.75)[c(1,4,2,5,3,6)])

intervals = paste0("[", round(100*lowers), "-", round(100*uppers), "%]")


values = rep(paste0(round(100*means), '%'), each=2)
values[2*(1:length(intervals))] = intervals

tab = matrix(values, ncol=2,
             dimnames = list(rep(rownames(means), each=2), c('survey', 'adjusted')))

write.csv(tab, file=file.path(PLOT.DIR, 'kernel_descriptions.csv'))



kde.fit = RW.EFFECT.KDEs.EXPANSION$q1_adap_loss
x = seq(0,1, length=1000)
d.kde = dkde(asin(sqrt(x)), kde.fit) / 2 / sqrt(-((x-1)*x))

mask = d.kde < 3
qplot(x[mask], d.kde[mask], geom='line')

plot.kernel.samples <- function(samples, filename)
{
    plot = qplot(samples, bins=40) + 
        theme_bw(base_size=10) + 
        ylab(NULL) + xlab(NULL) +
        # ylab("Number of Samples") + 
        # xlab("Viral Suppression Loss") +
        scale_x_continuous(label=scales::percent, limits = c(-.02, 1.02))
    
    
    ggsave(plot = plot, 
           filename=file.path(PLOT.DIR, paste0(filename, '.png')),
           height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    plot
}

plot.kernel.samples(RW.effect.values[1,], 'adap_exp')
plot.kernel.samples(RW.effect.values[4,], 'adap_nonexp')
plot.kernel.samples(adjusted.RW.effect.values[1,], 'adap_exp_adj')
plot.kernel.samples(adjusted.RW.effect.values[4,], 'adap_nonexp_adj')

plot.kernel.samples(RW.effect.values[2,], 'oahs_exp')
plot.kernel.samples(RW.effect.values[5,], 'oahs_nonexp')
plot.kernel.samples(adjusted.RW.effect.values[2,], 'oahs_exp_adj')
plot.kernel.samples(adjusted.RW.effect.values[5,], 'oahs_nonexp_adj')

plot.kernel.samples(RW.effect.values[3,], 'support_exp')
plot.kernel.samples(RW.effect.values[6,], 'support_nonexp')
plot.kernel.samples(adjusted.RW.effect.values[3,], 'support_exp_adj')
plot.kernel.samples(adjusted.RW.effect.values[6,], 'support_nonexp_adj')
