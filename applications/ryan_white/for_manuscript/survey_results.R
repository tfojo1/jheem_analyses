
library(ggplot2)

PLOT.DIR = file.path(RW.ROOT.PLOT.DIR, 'figure_survey_results/')
PLOT.HEIGHT = 1.7 + 0.35
CAPTION.HEIGHT = 0.25
PLOT.WIDTH = 2
PLOT.DPI = 600
PLOT.DEVICE = 'png'
PLOT.TEXT.SIZE = 8
CAPTION.TEXT.SIZE = 11


KERNEL.XLIM = c(0,1)
dim(rw_survey)

states.plus = locations::get.all.for.type('STATE')
state.to.survey.q.mappings = paste0("X.choice.",
                                    gsub(" ", ".", get.location.name(states.plus)),
                                    ".")
names(state.to.survey.q.mappings) = states.plus
state.to.survey.q.mappings = state.to.survey.q.mappings[sapply(state.to.survey.q.mappings, function(name){
    any(name==colnames(rw_survey))
})]

state.counts = colSums(rw_survey[,state.to.survey.q.mappings])
names(state.counts) = names(state.to.survey.q.mappings)

# Make Masks
expansion.states = names(STATE.MEDICAID.EXPANSION)[STATE.MEDICAID.EXPANSION]
expansion.states = gsub("_",".",expansion.states)
expansion.col.names = paste0("X.choice.", expansion.states, ".")

medicaid.expansion.mask = rowSums(rw_survey[,expansion.col.names]) > 0


non.expansion.states = names(STATE.MEDICAID.EXPANSION)[!STATE.MEDICAID.EXPANSION]
non.expansion.states = gsub("_",".",non.expansion.states)
non.expansion.col.names = paste0("X.choice.", non.expansion.states, ".")

medicaid.nonexpansion.mask = rowSums(rw_survey[,non.expansion.col.names]) > 0

# Print some info

print(paste0(nrow(rw_survey), " respondents from ",
             sum(state.counts>0)-1, " unique states + Washington DC"))
print(paste0(sum(medicaid.expansion.mask), " from ", sum(state.counts[MEDICAID.EXPANSION.STATES]>0)-1, " Medicaid expansion states + Washington DC"))
print(paste0(sum(medicaid.nonexpansion.mask), " from ", sum(state.counts[MEDICAID.NONEXPANSION.STATES]>0), " Medicaid NON-expansion states"))

print(paste0("The median proportion from rural states was ", median(rw_survey$q4_rural_percentage, na.rm=T)))

print(paste0("The median expected ADAP suppression loss was ", median(rw_survey$q1_adap_loss, na.rm=T)))
print(paste0("    ",  median(rw_survey$q1_adap_loss[medicaid.expansion.mask], na.rm=T),
             " expansion, ", median(rw_survey$q1_adap_loss[medicaid.nonexpansion.mask], na.rm=T),
             " NON-expansion"))
print(paste0("The median expected OWHS suppression loss was ", median(rw_survey$q2_oahs_loss, na.rm=T)))
print(paste0("    ",  median(rw_survey$q2_oahs_loss[medicaid.expansion.mask], na.rm=T),
             " expansion, ", median(rw_survey$q2_oahs_loss[medicaid.nonexpansion.mask], na.rm=T),
             " NON-expansion"))
print(paste0("The median expected other RW suppression loss was ", median(rw_survey$q3_support_loss, na.rm=T)))
print(paste0("    ",  median(rw_survey$q3_support_loss[medicaid.expansion.mask], na.rm=T),
             " expansion, ", median(rw_survey$q3_support_loss[medicaid.nonexpansion.mask], na.rm=T),
             " NON-expansion"))

print("")

print(paste0("The mean expected ADAP suppression loss was ", round(mean(rw_survey$q1_adap_loss, na.rm=T))))
print(paste0("    ",  round(mean(rw_survey$q1_adap_loss[medicaid.expansion.mask], na.rm=T)),
             " expansion, ", round(mean(rw_survey$q1_adap_loss[medicaid.nonexpansion.mask], na.rm=T)),
             " NON-expansion"))
print(paste0("The mean expected OWHS suppression loss was ", round(mean(rw_survey$q2_oahs_loss, na.rm=T))))
print(paste0("    ",  round(mean(rw_survey$q2_oahs_loss[medicaid.expansion.mask], na.rm=T)),
             " expansion, ", round(mean(rw_survey$q2_oahs_loss[medicaid.nonexpansion.mask], na.rm=T)),
             " NON-expansion"))
print(paste0("The mean expected other RW suppression loss was ", round(mean(rw_survey$q3_support_loss, na.rm=T))))
print(paste0("    ",  round(mean(rw_survey$q3_support_loss[medicaid.expansion.mask], na.rm=T)),
             " expansion, ", round(mean(rw_survey$q3_support_loss[medicaid.nonexpansion.mask], na.rm=T)),
             " NON-expansion"))

#-- Set up DFs and Summaries --#

df.for.hist = rbind(
    cbind(rw_survey[medicaid.expansion.mask,c(4,6,8)]/100, medicaid='exp'),
    cbind(rw_survey[medicaid.nonexpansion.mask,c(4,6,8)]/100, medicaid='nonexp')
)


df.kernel = as.data.frame(
    rbind(t(RW.effect.values[1:3,]),
          t(RW.effect.values[4:6,])))
df.kernel$medicaid = rep(c('exp','nonexp'), each=ncol(RW.effect.values))

colnames(df.kernel) = colnames(df.for.hist)


df.kernel.adj = as.data.frame(
    rbind(t(adjusted.RW.effect.values[1:3,]),
          t(adjusted.RW.effect.values[4:6,])))
df.kernel.adj$medicaid = rep(c('exp','nonexp'), each=ncol(RW.effect.values))

colnames(df.kernel.adj) = colnames(df.for.hist)

dfs = list(survey=df.for.hist, samples=df.kernel, cons_samples=df.kernel.adj)
questions = colnames(df.for.hist)[1:3]

summaries = sapply(questions, function(q){
    sapply(dfs, function(df){
        sapply(c('exp','nonexp'), function(medicaid){
            
            values = df[[q]][ df[['medicaid']]==medicaid ]
            values = values[!is.na(values)]
            
            paste0(round(100*mean(values)), '% [',
                   round(100*quantile(values, probs=.25)), ' - ',
                   round(100*quantile(values, probs=.75)), "%], n=",
                   length(values))
            
        })
    })
})

rownames(summaries) = paste0(rep(names(dfs), each=2), ", ", rep(c('expansion','nonexp'), 3))
write.csv(summaries, file=file.path(PLOT.DIR, 'summary_stats.csv'))




#-- make row1 panels --#

survey.items = c(adap = 'q1_adap_loss',
                 oahs = 'q2_oahs_loss',
                 support = 'q3_support_loss')
survey.summary = sapply(c('all','exp','nonexp'), function(expansion){
    sapply(c('adap','oahs','support'), function(service){
        
        if (expansion=='exp')
            mask = medicaid.expansion.mask
        else if (expansion=='nonexp')
            mask = medicaid.nonexpansion.mask
        else
            mask = rep(T, length(medicaid.expansion.mask))
        
        data = rw_survey[ mask, survey.items[service] ]
        
        c(
            mean = mean(data, na.rm=T),
            median = median(data, na.rm=T),
            lower = quantile(data, probs=0.25, na.rm=T),
            upper = quantile(data, probs=0.75, na.rm=T)
        )
        
    })
})

dim(survey.summary) = c(statistic=4, service=3, expansion=3)
dimnames(survey.summary) = list(
    statistic = c('mean','median','lower','upper'),
    service = c('adap','oahs','support'),
    expansion = c('all','exp','nonexp')
)

mean.and.ci.table = apply(survey.summary, 3, function(summ){
    paste0(round(summ['mean',]), "% [",
           round(summ['lower',]), "-",
           round(summ['upper',]), "%]")
})
rownames(mean.and.ci.table) = dimnames(survey.summary)$service

print(mean.and.ci.table)

print("N Respondents by question: ")
n.responses = colSums(!is.na(rw_survey[,c(4,6,8)]))
print(paste0(names(n.responses), ": ", n.responses, collapse=', '))

N.BINS = 15

Y.LIM = 23

plot = ggplot(df.for.hist, aes(x=q1_adap_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[1,1]) +
    labs(caption=summaries[2,1]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylim(0,Y.LIM) + 
    ylab(NULL); plot
#    ylab("Number of Respondents"); plot


ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1A.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.for.hist, aes(x=q2_oahs_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[1,2]) +
    labs(caption=summaries[2,2]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylim(0,Y.LIM) + 
    ylab(NULL); print(plot)
#    ylab("Number of Respondents"); plot


ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1B.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.for.hist, aes(x=q3_support_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[1,3]) +
    labs(caption=summaries[2,3]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylim(0,Y.LIM) + 
    ylab(NULL); print(plot)
#    ylab("Number of Respondents"); plot
 
ggsave(plot = plot,
       filename=file.path(PLOT.DIR, "Figure_1C.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



#-- For sampled effects --#

N.BINS.KERNEL = 25
KERNEL.YLIM = c(0,110)


plot = ggplot(df.kernel, aes(x=q1_adap_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[3,1]) +
    labs(caption=summaries[4,1]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1D.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.kernel, aes(x=q2_oahs_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[3,2]) +
    labs(caption=summaries[4,2]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1E.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.kernel, aes(x=q3_support_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[3,3]) +
    labs(caption=summaries[4,3]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1F.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- For adjusted sampled effects --#

KERNEL.ADJ.YLIM = c(0,420)


plot = ggplot(df.kernel.adj, aes(x=q1_adap_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.ADJ.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[5,1]) +
    labs(caption=summaries[6,1]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1G.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.kernel.adj, aes(x=q2_oahs_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.ADJ.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[5,2]) +
    labs(caption=summaries[6,2]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1H.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(df.kernel.adj, aes(x=q3_support_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = F, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.ADJ.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[5,3]) +
    labs(caption=summaries[6,3]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0)) + 
    ylab(NULL); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_1I.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = ggplot(df.kernel.adj, aes(x=q3_support_loss, fill=medicaid, group=medicaid)) + geom_histogram(position='dodge', show.legend = T, bins = N.BINS.KERNEL) +
    scale_fill_manual(values = c(exp=RW.EXP.COLOR, nonexp=RW.NONEXP.COLOR), 
                      labels = c(exp="Medicaid Expansion States ", nonexp="Medicaid Non-Expansion States"),
                      name=NULL) +
    ylim(KERNEL.ADJ.YLIM) +
    theme_bw(base_size = PLOT.TEXT.SIZE+3) + 
    scale_x_continuous(labels=scales::percent, limits = KERNEL.XLIM, name = summaries[5,3]) +
    labs(caption=summaries[6,3]) +
    theme(plot.caption = element_text(hjust = 0.5, size = CAPTION.TEXT.SIZE, color = RW.NONEXP.COLOR, face = 'bold'),
          axis.title.x = element_text(size = CAPTION.TEXT.SIZE, color = RW.EXP.COLOR, face = 'bold', vjust = 0),
          legend.position = 'bottom') + 
    ylab(NULL); plot

legend = cowplot::get_plot_component(plot, 'guide-box-bottom', return_all = TRUE)

ggsave(plot = legend, 
       filename=file.path(PLOT.DIR, "Figure_1_legend.png"),
       height = 0.25, width = 4.1, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Quick summary of adjusted results
flattened.expansion = cbind(adjusted.RW.effect.values[1:3,], adjusted.RW.effect.values[4:6,])
round(100*cbind(rowMeans(flattened.expansion),
                apply(flattened.expansion, 1, quantile, probs=0.25),
                apply(flattened.expansion, 1, quantile, probs=0.75)))
