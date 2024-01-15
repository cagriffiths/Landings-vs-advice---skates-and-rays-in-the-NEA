###
# code to generate dataset used in Batsleer et al. 2024
# authors:
# Christopher Griffiths (SLU)
# Katinka Bleeker (WMR)

###
# setup
rm(list=ls())
gc()

options(scipen = 99999)

###
# packages
require(tidyverse)

sessionInfo()
# tidyverse - tidyverse_2.0.0

###
# data
land = read.csv("data/landings_2009-2022.csv", sep=';') # landings 
advice = read.csv('data/advice.csv', sep=';')
traits = read.csv('data/life_history_traits.csv', sep=';')

###
# allocate ICES fishing areas to ecoregions
# Greater North Sea
land_GNS = filter(land, fishing_area %in% c("27.2.b", "27.3.a.20",
                                             "27.3.a.21",
                                             "27.4.a", "27.4.b",
                                             "27.4.c",
                                             '27.2.a', "27.7.d",
                                             '27.4'))

land_GNS$area = 'Greater North Sea'

# Celtic Seas
land_CS = filter(land, fishing_area %in% c("27.6.a", "27.6.b", "27.7.a", "27.7.b", 
                                            "27.7.c", "27.7.e", "27.7.f",   
                                            "27.7.g", "27.7.h", "27.7.j", "27.7.k", '27.7',
                                            '27.6', "27.7.c.2", "27.7.j.2", "27.7.k.2",
                                            "27.6.b.1"))

land_CS$area = 'Celtic Seas'

# Bay of Biscay and Iberian coast
land_BI = filter(land, fishing_area %in% c("27.8.a", "27.8.b", "27.8.c", 
                                            "27.8.d", "27.9.a",   
                                            "27.8.e", "27.9.b",
                                            '27.8.abd','27.8.ce','27.8.d.2'))

land_BI$area = 'Bay of Biscay and Iberian coast'

# collate into land2
land2 = rbind(land_GNS, land_CS, land_BI)

###
# select years used in the analysis
land2 = filter(land2, year %in% 2016:2022) # remove years before ICES advice was given for most stocks

###
# calculate proportion of landings by stock in each ecoregion
land_prop = full_join(land2 %>% 
                  group_by(year, ICES_stock_code, area) %>% 
                  summarise(Landings = sum(landings)) %>% 
                  ungroup(),
                land2 %>% 
                  group_by(year, ICES_stock_code) %>% 
                  summarise(Landings_total = sum(landings)) %>% 
                  ungroup()) %>%
  mutate(proportion_landings = Landings/Landings_total) %>% 
  group_by(ICES_stock_code, area)

###
# merge landings and advice, and split advice into ecoregions based on landing proportions by stock and year
colnames(land_prop)[2] = 'stock'
test = merge(land_prop, advice) # merge
test$advice_corrected = test$advice * test$proportion_landings # multiple advice by landing proportions

###
# quick check on some stocks that straddle multiple ecoregions 
rje = filter(test, stock %in% c('rje.27.7de')) # split into Celtic Seas and Greater North Sea
rju = filter(test, stock %in% c('rju.27.7de')) # split into Celtic Seas and Greater North Sea
rjh = filter(test, stock %in% c('rjh.27.4a6')) # split into Celtic Seas and Greater North Sea
rjn = filter(test, stock %in% c('rjn.27.678abd')) # split into all 3 ecoregions

###
# calculate relative and absolute differences between landings and advice 
test$rel_advice = (test$Landings/test$advice_corrected)-1 # normalised to get equivalence on both scales
test$diff_advice = test$Landings-test$advice_corrected

###
# remove any NAs
test = filter(test, diff_advice != 'NA')
test = filter(test, diff_advice != 'NaN')

###
# add a 1 in instances when advice = 0 but landings still occur
test <- test %>% mutate(rel_advice = ifelse(is.finite(rel_advice), rel_advice, 1))

###
# select stocks used in the analysis
# see MS Table 1 for a list of stocks
test = filter(test, stock %in% c("rjc.27.3a47d",
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
                                   "rjn.27.9a",
                                   "rju.27.7bj",
                                   "rju.27.7de"))

###
# quick check of stocks and species
NROW(unique(test$stock)) # 26 Stocks
NROW(unique(test$species)) # 8 Species

###
# add ICES assessment category groups
test56 = filter(test, ICES_cat %in% c(5,6))
test56$cat_group = 'Category 5 and 6'
test3 = filter(test, ICES_cat == 3)
test3$cat_group = 'Category 3'
test = rbind(test3, test56)

###
# add species-specific life history traits
test = merge(test, traits)

###
# save dataset
write.csv(test, file = 'data/landings_vs_advice.csv')


