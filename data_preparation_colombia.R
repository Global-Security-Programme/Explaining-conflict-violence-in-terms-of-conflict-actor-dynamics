### Preamble ###############################################################
# Data preparation (Armed conflict in Colombia)
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 12 July 2023

rm(list= ls())

library(tidyverse)

ged <- read.csv("data/ged191.csv")

dyads <- c("AUC - FARC", 
           "Government of Colombia - FARC",
           "Government of Colombia - ELN",
           "Government of Colombia - EPL - Megateo",
           "Government of Colombia - EPL",
           "Cali Cartel - Medellín Cartel",
           "Medellín Cartel - PEPES",
           "AUC - ELN",
           "Bloque Central Bolívar - ELN, FARC",
           "Government of Colombia - FARC dissidents")

colombia <- ged %>%
  filter(dyad_name %in% dyads) %>% 
  mutate(deaths_total = deaths_a + deaths_b + deaths_civilians + deaths_unknown) %>% 
  filter(deaths_total >= 1)

saveRDS(colombia, "data/colombia.rds")
