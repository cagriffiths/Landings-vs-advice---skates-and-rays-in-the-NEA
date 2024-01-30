###
# code to generate figures (1-3 and S1) for Batsleer et al. 2024
# authors:
# Christopher Griffiths (SLU)

###
# setup
rm(list=ls())
gc()

options(scipen = 99999)

###
# packages
require(tidyverse)
require(patchwork)
require(viridis)
require(rcartocolor)
require(ggsci)
require(forcats)
require(ggtext)

sessionInfo()
# tidyverse - tidyverse_2.0.0
# patchwork - patchwork_1.1.2
# viridis - viridis_0.6.2
# rcartocolor - rcartocolor_2.0.0
# ggsci - ggsci_2.9
# forcats - forcats_1.0.0

###
# data
#dat = read.csv('data/landings_vs_advice.csv')
#dat = dat[,2:19] # remove excel column

dat = read.csv('Edited data for figure 1 new.csv', sep = ';')

###
# Figure 1
# tidy up species names
dat$species[dat$species == "Blonde ray"] = "Blonde ray (Raja brachyura)"
dat$species[dat$species == "Cuckoo ray"] = "Cuckoo ray (Leucoraja naevus)"
dat$species[dat$species == "Sandy ray"] = "Sandy ray (Raja circularis)"
dat$species[dat$species == "Shagreen ray"] = "Shagreen ray (Leucoraja fullonica)"
dat$species[dat$species == "Small-eyed ray"] = "Small-eyed ray (Raja microocellata)"
dat$species[dat$species == "Spotted ray"] = "Spotted ray (Raja montagui)"
dat$species[dat$species == "Thornback ray"] = "Thornback ray (Raja clavata)"
dat$species[dat$species == "Undulate ray"] = "Undulate ray (Raja undulata)"

# generate species-specific colour palette
carto_pal(9, "Pastel")
display_carto_pal(9, 'Pastel')
cols = c("Blonde ray (Raja brachyura)" = "#66C5CC", 
         "Cuckoo ray (Leucoraja naevus)" = "#F6CF71", 
         "Sandy ray (Raja circularis)" = "#F89C74", 
         "Shagreen ray (Leucoraja fullonica)" = "#DCB0F2", 
         "Small-eyed ray (Raja microocellata)" = "#87C55F", 
         "Spotted ray (Raja montagui)" = "#9EB9F3", 
         "Thornback ray (Raja clavata)" = "#FE88B1", 
         "Undulate ray (Raja undulata)" = "#C9DB74")

dat$area = factor(dat$area, levels=c('Greater North Sea',
                                                 'Celtic Seas',
                                                 'Bay of Biscay and Iberian coast'))

# order stocks by species
dat1 = mutate(dat, stock = reorder(stock, as.factor(species)))
dat1$species1 = as.numeric(as.factor(dat1$species))

####
# Figure 1 edit
rel1 = ggplot(data = filter(dat1, scale %in% c('relative')), 
                aes(x = value, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 5, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -2, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species:", values = cols,
                    breaks = c("Blonde ray (Raja brachyura)", "Cuckoo ray (Leucoraja naevus)", 
                               "Sandy ray (Raja circularis)", "Shagreen ray (Leucoraja fullonica)",
                               "Small-eyed ray (Raja microocellata)", "Spotted ray (Raja montagui)", 
                               "Thornback ray (Raja clavata)", "Undulate ray (Raja undulata)"),
                    labels = c("Blonde ray (*Raja brachyura*)", "Cuckoo ray (*Leucoraja naevus*)", 
                               "Sandy ray (*Raja circularis*)", "Shagreen ray (*Leucoraja fullonica*)",
                               "Small-eyed ray (*Raja microocellata*)", "Spotted ray (*Raja montagui*)", 
                               "Thornback ray (*Raja clavata*)", "Undulate ray (*Raja undulata*)"))+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-1, 4), breaks = seq(-1, 4, 1))+
  facet_wrap(~area, ncol = 1, scales="free")+
  ylab('Stock')+
  xlab('Landings/Advice')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(size = 16),
        legend.text = ggtext::element_markdown())

rel2 = ggplot(data = filter(dat1, scale %in% c('absolute')), 
              aes(x = value, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 1100, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -1100, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species:", values = cols,
                    breaks = c("Blonde ray (Raja brachyura)", "Cuckoo ray (Leucoraja naevus)", 
                               "Sandy ray (Raja circularis)", "Shagreen ray (Leucoraja fullonica)",
                               "Small-eyed ray (Raja microocellata)", "Spotted ray (Raja montagui)", 
                               "Thornback ray (Raja clavata)", "Undulate ray (Raja undulata)"),
                    labels = c("Blonde ray (*Raja brachyura*)", "Cuckoo ray (*Leucoraja naevus*)", 
                               "Sandy ray (*Raja circularis*)", "Shagreen ray (*Leucoraja fullonica*)",
                               "Small-eyed ray (*Raja microocellata*)", "Spotted ray (*Raja montagui*)", 
                               "Thornback ray (*Raja clavata*)", "Undulate ray (*Raja undulata*)"))+
  scale_y_discrete(limits=rev)+
  #scale_x_continuous(limits=c(-1, 4), breaks = seq(-1, 4, 1))+
  facet_wrap(~area, ncol = 1, scales="free")+
  ylab('')+
  xlab('Landings-Advice (tonnes)')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(size = 16),
        legend.text = ggtext::element_markdown())

rel = (rel1 | rel2) + 
  plot_layout(guides = "collect") + 
  theme(legend.position = "right")
ggsave(rel, filename = 'Figure 1 - higher dpi.tiff', height = 11, width = 14, dpi = 500)

# Greater North Sea 
relGNS = ggplot(data = filter(dat1, area == 'Greater North Sea'), 
                aes(x = rel_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 5, ymin = 0, ymax = 10, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -2, xmax = 0, ymin = 0, ymax = 10, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-1, 4), breaks = seq(-1, 4, 1))+
  ylab('')+
  xlab('')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  ggtitle('Greater North Sea')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

relGNS2 = ggplot(data = filter(dat1, area == 'Greater North Sea'), 
                 aes(x = diff_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 600, ymin = 0, ymax = 10, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -600, xmax = 0, ymin = 0, ymax = 10, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-500, 500), breaks = seq(-500, 500, 100))+
  ylab('')+
  xlab('')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  ggtitle('Greater North Sea')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

# Celtic Seas
relCS = ggplot(data = filter(dat1, area == 'Celtic Seas'), 
               aes(x = rel_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 5, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -2, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-1, 4), breaks = seq(-1, 4, 1))+
  ylab('Stock')+
  xlab('')+
  ggtitle('Celtic Seas')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

relCS2 = ggplot(data = filter(dat1, area == 'Celtic Seas'), 
                aes(x = diff_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 1100, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -1100, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-1000, 1000), breaks = seq(-1000, 1000, 250))+
  ylab('')+
  xlab('')+
  ggtitle('Celtic Seas')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

# Bay of Biscay and Iberian coast
relBI = ggplot(data = filter(dat1, area == 'Bay of Biscay and Iberian coast'), 
               aes(x = rel_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 5, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -2, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-1.1, 4), breaks = seq(-1, 4, 1))+
  ylab('')+
  xlab('Landings/Advice')+
  ggtitle('Bay of Biscay and Iberian coast')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

relBI2 = ggplot(data = filter(dat1, area == 'Bay of Biscay and Iberian coast'), 
                aes(x = diff_advice, y = fct_reorder(stock, species1), fill = species))+
  geom_rect(xmin = 0, xmax = 900, ymin = 0, ymax = 16, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -900, xmax = 0, ymin = 0, ymax = 16, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Species", values = cols)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(-800, 800), breaks = seq(-800, 800, 200))+
  ylab('')+
  xlab('Landings-Advice (tonnes)')+
  ggtitle('Bay of Biscay and Iberian coast')+
  geom_vline(xintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = 'right')

# merge
rel = (((relGNS/relCS/relBI) | (relGNS2/relCS2/relBI2)) + 
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") + theme(legend.position = "right"))

# save
setwd(".../output")
ggsave(rel, filename = 'Figure 1 - higher dpi with legend.tiff', height = 11, width = 13, dpi = 500)

###
# Figure 2
# first rearrange data on the relative scale
test_plot = data.frame(matrix(ncol = 3, nrow=NROW(dat)))
colnames(test_plot) = c('rel_advice', 'Group', 'Area')
test_plot$rel_advice = dat$rel_advice
test_plot$group = dat$cat_group
test_plot$area = dat$area

# tidy label
test_plot$group[test_plot$group == "Category 5 and 6"] <- 'Category\n5 and 6'

# assign colours
cols = c("Category 3" = "#9ECAE1", 
         "Category\n5 and 6" = "#3182BD")

# make into factors
test_plot$group = factor(test_plot$group, levels=c('Category 3',
                                                   'Category\n5 and 6'))

test_plot$area = factor(test_plot$area, levels=c('Greater North Sea',
                                                 'Celtic Seas',
                                                 'Bay of Biscay and Iberian coast'))
# plot
rel_plot = ggplot(data = test_plot, 
             aes(y = rel_advice, x = group, fill = group))+
  geom_rect(xmin = -1, xmax = 3, ymin = 0, ymax = 5, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -1, xmax = 3, ymin = -2, ymax = 0, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge")+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "", values = cols)+
  scale_x_discrete()+
  scale_y_continuous(limits=c(-1.1, 4), breaks = seq(-1, 4, 1))+
  xlab('')+
  ylab('Landings/Advice')+
  facet_wrap(~area, ncol = 3)+
  geom_hline(yintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = '',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

# then rearrange data on the absolute scale
test_plot1 = data.frame(matrix(ncol = 3, nrow=NROW(dat)))
colnames(test_plot1) = c('diff_advice', 'group', 'area')
test_plot1$diff_advice = dat$diff_advice
test_plot1$group = dat$cat_group
test_plot1$area = dat$area

# tidy label
test_plot1$group[test_plot1$group == "Category 5 and 6"] <- 'Category\n5 and 6'

# make into factors
test_plot1$group = factor(test_plot1$group, levels=c('Category 3',
                                                   'Category\n5 and 6'))

test_plot1$area = factor(test_plot1$area, levels=c('Greater North Sea',
                                                 'Celtic Seas',
                                                 'Bay of Biscay and Iberian coast'))

# plot
diff_plot = ggplot(data = test_plot1, 
              aes(y = diff_advice, x = group, fill = group))+
  geom_rect(xmin = -1, xmax = 3, ymin = 0, ymax = 1100, fill = "grey80", alpha = 0.05)+
  geom_rect(xmin = -1, xmax = 3, ymin = -1100, ymax = 0, fill = "white", alpha = 0.2)+
  geom_bar(stat = "summary", fun = "mean", col = 'black', position="dodge", alpha = 0.9)+
  geom_point(shape = 21, size = 3.5, col='black', position = position_jitter(width = 0.1, height = 0.1))+
  theme_bw()+
  scale_fill_manual(name = "Group", values = cols)+
  scale_x_discrete()+
  scale_y_continuous(limits=c(-1000, 1000), breaks = seq(-1000, 1000, 250))+
  xlab('')+
  ylab('Landings-Advice (tonnes)')+
  facet_wrap(~area, ncol = 3)+
  geom_hline(yintercept = 0, col = 'black', linetype = 'dashed', linewidth = 1.2)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"),
        legend.position = '',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

# merge plots
ices_cat = rel_plot / diff_plot + plot_layout(guides = "collect")

# save
setwd(".../output")
ggsave(ices_cat, filename = 'Figure 2 - higher dpi.tiff', height = 7, width = 9, dpi = 500)

###
# Figure 3
# Linf - summarise by species, area and Linf
lmax = dat %>% group_by(species, area, Linf) %>% summarise(rel = mean(rel_advice, na.rm=T),
                                                             diff = mean(diff_advice, na.rm=T))
# make area into a factor
lmax$area = factor(lmax$area, levels=c('Greater North Sea',
                                       'Celtic Seas',
                                       'Bay of Biscay and Iberian coast'))

# assign colours
cols = c("Greater North Sea" = "#66C5CC", 
         "Bay of Biscay and Iberian coast" = "#F6CF71", 
         "Celtic Seas" = "#87C55F") 

# Linf plots
g1 = ggplot(data = lmax, aes(x = Linf, y = rel))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(75,130, 5), limits = c(75,130))+
  scale_y_continuous(breaks=seq(-0.5, 1.5, 0.5), limits = c(-0.5,1.5))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings/Advice')+
  xlab('Length at infinity (cm)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

g2 = ggplot(data = lmax, aes(x = Linf, y = diff))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(75,130, 5), limits = c(75,130))+
  scale_y_continuous(breaks=seq(-300, 400, 100), limits = c(-300,400))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings-Advice (tonnes)')+
  xlab('Length at infinity (cm)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

# Lmat - summarise by species, area and Lmat
lmat = dat %>% group_by(species, area, Lmat) %>% summarise(rel = mean(rel_advice, na.rm=T),
                                                             diff = mean(diff_advice, na.rm=T))
# make area into a factor
lmat$area = factor(lmat$area, levels=c('Greater North Sea',
                                       'Celtic Seas',
                                       'Bay of Biscay and Iberian coast'))

# Lmat plots
g3 = ggplot(data = lmat, aes(x = Lmat, y = rel))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(50,100,10), limits = c(50,100))+
  scale_y_continuous(breaks=seq(-0.5, 1.5, 0.5), limits = c(-0.5,1.5))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings/Advice')+
  xlab('Length at maturity (cm)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

g4 = ggplot(data = lmat, aes(x = Lmat, y = diff))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(50,100,10), limits = c(50,100))+
  scale_y_continuous(breaks=seq(-300, 400, 100), limits = c(-300,400))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings-Advice (tonnes)')+
  xlab('Length at maturity (cm)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

# Fecunidty - summarise by species, area and fecundity
fecundity = dat %>% group_by(species, area, Fecundity) %>% summarise(rel = mean(rel_advice, na.rm=T),
                                                                       diff = mean(diff_advice, na.rm=T))
# make area into a factor
fecundity$area = factor(fecundity$area, levels=c('Greater North Sea',
                                                 'Celtic Seas',
                                                 'Bay of Biscay and Iberian coast'))

# fecundity plots
g5 = ggplot(data = fecundity, aes(x = Fecundity, y = rel))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(40,105,15), limits = c(40,105))+
  scale_y_continuous(breaks=seq(-0.5, 1.5, 0.5), limits = c(-0.5,1.5))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings/Advice')+
  xlab('Fecundity (no of eggs)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

g6 = ggplot(data = fecundity, aes(x = Fecundity, y = diff))+
  geom_point(aes(fill = area), shape = 21, size = 4, alpha = 0.8, col = 'grey20')+
  theme_bw()+
  scale_x_continuous(breaks=seq(40,105,15), limits = c(40,105))+
  scale_y_continuous(breaks=seq(-300, 400, 100), limits = c(-300,400))+
  geom_smooth(se = TRUE, method = lm, linewidth = 1.2, alpha = 0.4, col = 'grey50')+
  scale_fill_manual(values = cols, name = 'Ecoregion:')+
  ylab('Landings-Advice (tonnes)')+
  xlab('Fecundity (no of eggs)')+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14),
        plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"))

# merge plots
traits_out = ((g1/g3/g5) | (g2/g4/g6)) + 
  plot_layout(guides = "collect") & theme(legend.position = 'top')

# save
setwd(".../output")
ggsave(traits_out, filename = 'Figure 3 - higher dpi.tiff', height = 10, width = 8, dpi = 500)

###
# Figure S1
tac = read.csv("data/TAC_2009-2022.csv", sep = ';') # load TACs by area
colnames(tac) = c('Year', '3a', '2a_and_4', "7d", "rju_7de", "6_and_7", '8_and_9') # rename columns
tac = tac[,1:7] # remove excel column
tac = filter(tac, Year > 2015) # select years 1016-2022
tac$GNS = tac$`3a`+tac$`2a_and_4`+tac$`7d` # calculate TAC in Greater North Sea
tac = tac[,c(1,6,7,8)] # remove unneeded columns
colnames(tac) = c('year','Celtic Seas', 'Bay of Biscay and Iberian coast', 'Greater North Sea') # relabel areas

# select only stocks that are considered in the TAC
test = filter(dat, stock %in% c("rjc.27.3a47d",
                                 "rjc.27.6",
                                 "rjc.27.7afg",
                                 "rjc.27.7e",
                                 "rjc.27.8",
                                 "rjc.27.9a",
                                 "rje.27.7de",
                                 "rje.27.7fg",
                                 "rjf.27.67",
                                 "rjh.27.4a6",
                                 "rjh.27.4bc7d",
                                 "rjh.27.7afg",
                                 "rjh.27.7e",
                                 "rjh.27.9a",
                                 "rji.27.67",
                                 "rjm.27.3a47d",
                                 "rjm.27.67bj",
                                 "rjm.27.7ae-h",
                                 "rjm.27.8",
                                 "rjm.27.9a",
                                 "rjn.27.3a4",
                                 "rjn.27.678abd",
                                 "rjn.27.8c",
                                 "rjn.27.9a"))

# summarise advice and landings by year and area
sum_test = test %>% group_by(year, area) %>% summarise(advice_sum = sum(advice_corrected, na.rm = T),
                                                     landings = sum(Landings, na.rm=T))
# gather TAC table
tac = gather(tac, area, TAC, -year)

# merge sum_test and tac
bla = merge(sum_test, tac)

# plot
p1 = ggplot(data = bla, aes(x = as.factor(year), y = ((advice_sum/TAC)-1), fill = area))+
  geom_bar(position="dodge", stat="identity", alpha = 0.6, col = 'grey90')+
  scale_y_continuous(breaks=seq(-1,1,0.2), limits=c(-1,1))+
  geom_hline(yintercept = 0, col = 'grey60', linewidth = 1, linetype = 'dashed')+
  theme_bw()+
  scale_fill_futurama(name = 'Area:')+
  xlab('Year')+
  ylab('ICES advice/Agreed TAC')+
  theme(text = element_text(size = 16),
        legend.position = 'right')

p2 = ggplot(data = bla, aes(x = as.factor(year), y = ((landings/TAC)-1), fill = area))+
  geom_bar(position="dodge", stat="identity", alpha = 0.6, col = 'grey90')+
  scale_y_continuous(breaks=seq(-1,1,0.2), limits=c(-1,1))+
  geom_hline(yintercept = 0, col = 'grey60', linewidth = 1, linetype = 'dashed')+
  theme_bw()+
  scale_fill_futurama(name = 'Area:')+
  xlab('Year')+
  ylab('Landings/Agreed TAC')+
  theme(text = element_text(size = 16),
        legend.position = 'right')

p3 = ggplot(data = bla, aes(x = as.factor(year), y = ((landings/advice_sum)-1), fill = area))+
  geom_bar(position="dodge", stat="identity", alpha = 0.6, col = 'grey90')+
  scale_y_continuous(breaks=seq(-1,1,0.2), limits=c(-1,1))+
  geom_hline(yintercept = 0, col = 'grey60', linewidth = 1, linetype = 'dashed')+
  theme_bw()+
  scale_fill_futurama(name = 'Area:')+
  xlab('Year')+
  ylab('Landings/ICES advice')+
  theme(text = element_text(size = 16),
        legend.position = 'right')

# merge
p = (p1 | p2 | p3) + plot_layout(guides = "collect")  & theme(legend.position = 'top')

# save
setwd(".../output")
ggsave(p, filename = 'Figure S1.png', height = 8, width = 12)

###
# Figure S2
price = read.csv("data/price.csv", sep = ';') # load price data
price = price[,2:8] # remove excel column

# plot
price_plot = ggplot(price, aes(x = as.factor(year), y = price_kg_mean, col = common_name, 
                       group = common_name, fill = common_name))+
  geom_line(linewidth = 1)+
  geom_point(shape = 21, size = 4, alpha = 0.5)+
  scale_color_carto_d(name = 'Species', palette = 'Pastel')+
  scale_fill_carto_d(name = 'Species', palette = 'Pastel')+
  theme_bw()+
  xlab('Year')+
  ylab(bquote("Average \u20AC kg"^-1))+
  theme(text = element_text(size = 16))+
  scale_y_continuous(breaks=seq(1,3,0.5), limits=c(1, 3))

# save
setwd(".../output")
ggsave(price_plot, filename = 'Figure S2.png', height = 8, width = 12)

