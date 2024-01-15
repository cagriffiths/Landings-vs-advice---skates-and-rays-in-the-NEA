###
# code to generate SI tables (S1 and S3) for Batsleer et al. 2024
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

sessionInfo()
# tidyverse - tidyverse_2.0.0

###
# data
dat = read.csv('data/landings_vs_advice.csv')
dat = dat[,2:19] # remove excel column

####
# Table S1
tableS1 = dat %>% group_by(stock, year, area) %>% summarise(landings = sum(Landings, na.rm=T), advice = sum(advice_corrected, na.rm = T))
write.csv(tableS1, file = 'output/Table S1.csv')

# Table S3
tableS3 = dat %>% group_by(stock, year) %>% summarise(advice = sum(advice, na.rm=T))
write.csv(tableS3, file = 'output/Table S3.csv')
