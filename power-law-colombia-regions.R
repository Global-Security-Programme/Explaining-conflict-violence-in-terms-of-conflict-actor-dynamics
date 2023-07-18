### Preamble ###############################################################
# Fitting data to the power-law distribution: Regions in the armed conflict in Colombia
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 12 July 2023

rm(list= ls())

library(ggplot2)
library(poweRlaw)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgdal)
library(RColorBrewer)
library(tidyverse)

data <- readRDS("data/colombia.rds")

data <- data %>% filter(where_prec <= 4)


# Data preparation --------------------------------------------------------

data$dep_id[data$adm_1 == "Amazonas department"] <- 1
data$dep_id[data$adm_1 == "Antioquia department"] <- 2
data$dep_id[data$adm_1 == "Arauca department"] <- 3
data$dep_id[data$adm_1 == "Atlántico department"] <- 4
data$dep_id[data$adm_1 == "Bogotá department"] <- 5
data$dep_id[data$adm_1 == "Bolívar department"] <- 6
data$dep_id[data$adm_1 == "Boyacá department"] <- 7
data$dep_id[data$adm_1 == "Caldas department"] <- 9
data$dep_id[data$adm_1 == "Caquetá department"] <- 10

data$dep_id[data$adm_1 == "Casanare department"] <- 11
data$dep_id[data$adm_1 == "Cauca department"] <- 12
data$dep_id[data$adm_1 == "Cesar department"] <- 13
data$dep_id[data$adm_1 == "Chocó department"] <- 14
data$dep_id[data$adm_1 == "Cundinamarca department"] <- 15
data$dep_id[data$adm_1 == "Córdoba department"] <- 8
data$dep_id[data$adm_1 == "Guainía department"] <- 16
data$dep_id[data$adm_1 == "Guaviare department"] <- 17
data$dep_id[data$adm_1 == "Huila department"] <- 18
data$dep_id[data$adm_1 == "La Guajira department"] <-19

data$dep_id[data$adm_1 == "Magdalena department"] <- 20
data$dep_id[data$adm_1 == "Meta department"] <- 21
data$dep_id[data$adm_1 == "Nariño department"] <- 22
data$dep_id[data$adm_1 == "Norte de Santander department"] <- 23
data$dep_id[data$adm_1 == "Putumayo department"] <- 24
data$dep_id[data$adm_1 == "Quindío department"] <- 25
data$dep_id[data$adm_1 == "Risaralda department"] <- 26
data$dep_id[data$adm_1 == "Santander department"] <- 27
data$dep_id[data$adm_1 == "Sucre department"] <- 28
data$dep_id[data$adm_1 == "Táchira state"] <- 29
data$dep_id[data$adm_1 == "Tolima  department"] <- 30

data$dep_id[data$adm_1 == "Valle del Cauca  department"] <- 31
data$dep_id[data$adm_1 == "Vaupés department"] <- 32
data$dep_id[data$adm_1 == "Vichada department"] <- 33




# Region 1 ----------------------------------------------------------------

region.1 <- data %>% filter(dep_id == 19 |
                              dep_id == 13 |
                              dep_id == 20 |
                              dep_id == 4 |
                              dep_id == 6 |
                              dep_id == 28)

m_sp.1 = displ$new(region.1$deaths_total)
est_sp.1 = estimate_xmin(m_sp.1)
m_sp.1$setXmin(est_sp.1)

bs_p.1 = bootstrap_p(m_sp.1, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.1.a <- region.1 %>% filter(year <= 1999)

m_sp.1.a = displ$new(region.1.a$deaths_total)
est_sp.1.a = estimate_xmin(m_sp.1.a)
m_sp.1.a$setXmin(est_sp.1.a)

bs_p.1.a = bootstrap_p(m_sp.1.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.1.b <- region.1 %>% filter(year > 1999 & year <= 2009)

m_sp.1.b = displ$new(region.1.b$deaths_total)
est_sp.1.b = estimate_xmin(m_sp.1.b)
m_sp.1.b$setXmin(est_sp.1.b)

bs_p.1.b = bootstrap_p(m_sp.1.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.1.c <- region.1 %>% filter(year >= 2010)

m_sp.1.c = displ$new(region.1.c$deaths_total)
est_sp.1.c = estimate_xmin(m_sp.1.c)
m_sp.1.c$setXmin(est_sp.1.c)

bs_p.1.c = bootstrap_p(m_sp.1.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 2 ----------------------------------------------------------------

region.2 <- data %>% filter(dep_id == 8|
                              dep_id == 2 |
                              dep_id == 14)

m_sp.2 = displ$new(region.2$deaths_total)
est_sp.2 = estimate_xmin(m_sp.2)
m_sp.2$setXmin(est_sp.2)

bs_p.2 = bootstrap_p(m_sp.2, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.2.a <- region.2 %>% filter(year <= 1999)

m_sp.2.a = displ$new(region.2.a$deaths_total)
est_sp.2.a = estimate_xmin(m_sp.2.a)
m_sp.2.a$setXmin(est_sp.2.a)

bs_p.2.a = bootstrap_p(m_sp.2.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.2.b <- region.2 %>% filter(year > 1999 & year <= 2009)

m_sp.2.b = displ$new(region.2.b$deaths_total)
est_sp.2.b = estimate_xmin(m_sp.2.b)
m_sp.2.b$setXmin(est_sp.2.b)

bs_p.2.b = bootstrap_p(m_sp.2.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.2.c <- region.2 %>% filter(year >= 2010)

m_sp.2.c = displ$new(region.2.c$deaths_total)
est_sp.2.c = estimate_xmin(m_sp.2.c)
m_sp.2.c$setXmin(est_sp.2.c)

bs_p.2.c = bootstrap_p(m_sp.2.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 3 ----------------------------------------------------------------

region.3 <- data %>% filter(dep_id == 23|
                              dep_id == 27)   

m_sp.3 = displ$new(region.3$deaths_total)
est_sp.3 = estimate_xmin(m_sp.3)
m_sp.3$setXmin(est_sp.3)

bs_p.3 = bootstrap_p(m_sp.3, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.3.a <- region.3 %>% filter(year <= 1999)

m_sp.3.a = displ$new(region.3.a$deaths_total)
est_sp.3.a = estimate_xmin(m_sp.3.a)
m_sp.3.a$setXmin(est_sp.3.a)

bs_p.3.a = bootstrap_p(m_sp.3.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.3.b <- region.3 %>% filter(year > 1999 & year <= 2009)

m_sp.3.b = displ$new(region.3.b$deaths_total)
est_sp.3.b = estimate_xmin(m_sp.3.b)
m_sp.3.b$setXmin(est_sp.3.b)

bs_p.3.b = bootstrap_p(m_sp.3.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.3.c <- region.3 %>% filter(year >= 2010)

m_sp.3.c = displ$new(region.3.c$deaths_total)
est_sp.3.c = estimate_xmin(m_sp.3.c)
m_sp.3.c$setXmin(est_sp.3.c)

bs_p.3.c = bootstrap_p(m_sp.3.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 4 ----------------------------------------------------------------

region.4 <- data %>% filter(dep_id == 22|
                              dep_id == 12 |
                              dep_id == 31 |
                              dep_id == 26 |
                              dep_id == 9 |
                              dep_id == 25)

m_sp.4 = displ$new(region.4$deaths_total)
est_sp.4 = estimate_xmin(m_sp.4)
m_sp.4$setXmin(est_sp.4)

bs_p.4 = bootstrap_p(m_sp.4, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.4.a <- region.4 %>% filter(year <= 1999)

m_sp.4.a = displ$new(region.4.a$deaths_total)
est_sp.4.a = estimate_xmin(m_sp.4.a)
m_sp.4.a$setXmin(est_sp.4.a)

bs_p.4.a = bootstrap_p(m_sp.4.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.4.b <- region.4 %>% filter(year > 1999 & year <= 2009)

m_sp.4.b = displ$new(region.4.b$deaths_total)
est_sp.4.b = estimate_xmin(m_sp.4.b)
m_sp.4.b$setXmin(est_sp.4.b)

bs_p.4.b = bootstrap_p(m_sp.4.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.4.c <- region.4 %>% filter(year >= 2010)

m_sp.4.c = displ$new(region.4.c$deaths_total)
est_sp.4.c = estimate_xmin(m_sp.4.c)
m_sp.4.c$setXmin(est_sp.4.c)

bs_p.4.c = bootstrap_p(m_sp.4.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 5 ----------------------------------------------------------------

region.5 <- data %>% filter(dep_id == 15|
                              dep_id == 7 |
                              dep_id == 30 |
                              dep_id == 18 |
                              dep_id == 5)

m_sp.5 = displ$new(region.5$deaths_total)
est_sp.5 = estimate_xmin(m_sp.5)
m_sp.5$setXmin(est_sp.5)

bs_p.5 = bootstrap_p(m_sp.5, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.5.a <- region.5 %>% filter(year <= 1999)

m_sp.5.a = displ$new(region.5.a$deaths_total)
est_sp.5.a = estimate_xmin(m_sp.5.a)
m_sp.5.a$setXmin(est_sp.5.a)

bs_p.5.a = bootstrap_p(m_sp.5.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.5.b <- region.5 %>% filter(year > 1999 & year <= 2009)

m_sp.5.b = displ$new(region.5.b$deaths_total)
est_sp.5.b = estimate_xmin(m_sp.5.b)
m_sp.5.b$setXmin(est_sp.5.b)

bs_p.5.b = bootstrap_p(m_sp.5.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.5.c <- region.5 %>% filter(year >= 2010)

m_sp.5.c = displ$new(region.5.c$deaths_total)
est_sp.5.c = estimate_xmin(m_sp.5.c)
m_sp.5.c$setXmin(est_sp.5.c)

bs_p.5.c = bootstrap_p(m_sp.5.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 6 ----------------------------------------------------------------

region.6 <- data %>% filter(dep_id == 10|
                              dep_id == 24 |
                              dep_id == 1)

m_sp.6 = displ$new(region.6$deaths_total)
est_sp.6 = estimate_xmin(m_sp.6)
m_sp.6$setXmin(est_sp.6)
bs_p.6 = bootstrap_p(m_sp.6, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.6.a <- region.6 %>% filter(year <= 1999)

m_sp.6.a = displ$new(region.6.a$deaths_total)
est_sp.6.a = estimate_xmin(m_sp.6.a)
m_sp.6.a$setXmin(est_sp.6.a)
bs_p.6.a = bootstrap_p(m_sp.6.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.6.b <- region.6 %>% filter(year > 1999 & year <= 2009)

m_sp.6.b = displ$new(region.6.b$deaths_total)
est_sp.6.b = estimate_xmin(m_sp.6.b)
m_sp.6.b$setXmin(est_sp.6.b)
bs_p.6.b = bootstrap_p(m_sp.6.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.6.c <- region.6 %>% filter(year >= 2010)

m_sp.6.c = displ$new(region.6.c$deaths_total)
est_sp.6.c = estimate_xmin(m_sp.6.c)
m_sp.6.c$setXmin(est_sp.6.c)
bs_p.6.c = bootstrap_p(m_sp.6.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Region 7 ----------------------------------------------------------------

region.7 <- data %>% filter(dep_id == 3|
                              dep_id == 11 |
                              dep_id == 21 |
                              dep_id == 33 |
                              dep_id == 16 |
                              dep_id == 17 |
                              dep_id == 32)

m_sp.7 = displ$new(region.7$deaths_total)
est_sp.7 = estimate_xmin(m_sp.7)
m_sp.7$setXmin(est_sp.7)

bs_p.7 = bootstrap_p(m_sp.7, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.7.a <- region.7 %>% filter(year <= 1999)

m_sp.7.a = displ$new(region.7.a$deaths_total)
est_sp.7.a = estimate_xmin(m_sp.7.a)
m_sp.7.a$setXmin(est_sp.7.a)

bs_p.7.a = bootstrap_p(m_sp.7.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.7.b <- region.7 %>% filter(year > 1999 & year <= 2009)

m_sp.7.b = displ$new(region.7.b$deaths_total)
est_sp.7.b = estimate_xmin(m_sp.7.b)
m_sp.7.b$setXmin(est_sp.7.b)

bs_p.7.b = bootstrap_p(m_sp.7.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)

region.7.c <- region.7 %>% filter(year >= 2010)

m_sp.7.c = displ$new(region.7.c$deaths_total)
est_sp.7.c = estimate_xmin(m_sp.7.c)
m_sp.7.c$setXmin(est_sp.7.c)

bs_p.7.c = bootstrap_p(m_sp.7.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# Results for regions -----------------------------------------------------

region.1 <- c(1, "Region 1", est_sp.1[[2]], est_sp.1[[3]], est_sp.1[[4]], bs_p.1[[1]])
region.2 <- c(2, "Region 2", est_sp.2[[2]], est_sp.2[[3]], est_sp.2[[4]], bs_p.2[[1]])
region.3 <- c(3, "Region 3", est_sp.3[[2]], est_sp.3[[3]], est_sp.3[[4]], bs_p.3[[1]])
region.4 <- c(4, "Region 4", est_sp.4[[2]], est_sp.4[[3]], est_sp.4[[4]], bs_p.4[[1]])
region.5 <- c(5, "Region 5", est_sp.5[[2]], est_sp.5[[3]], est_sp.5[[4]], bs_p.5[[1]])
region.6 <- c(6, "Region 6", est_sp.6[[2]], est_sp.6[[3]], est_sp.6[[4]], bs_p.6[[1]])
region.7 <- c(7, "Region 7", est_sp.7[[2]], est_sp.7[[3]], est_sp.7[[4]], bs_p.7[[1]])

regions <- as.data.frame(rbind(region.1, region.2, region.3, region.4, region.5, region.6, region.7))

regions <- regions %>% rename(region_id=V1,
                              region=V2,
                              xmin=V3,
                              alpha=V4,
                              ntail=V5,
                              pKS=V6)
regions
write.csv(regions, "out/regions-results-1989-2018.csv")



region.1.a <- c(1, "Region 1", est_sp.1.a[[2]], est_sp.1.a[[3]], est_sp.1.a[[4]], bs_p.1.a[[1]])
region.2.a <- c(2, "Region 2", est_sp.2.a[[2]], est_sp.2.a[[3]], est_sp.2.a[[4]], bs_p.2.a[[1]])
region.3.a <- c(3, "Region 3", est_sp.3.a[[2]], est_sp.3.a[[3]], est_sp.3.a[[4]], bs_p.3.a[[1]])
region.4.a <- c(4, "Region 4", est_sp.4.a[[2]], est_sp.4.a[[3]], est_sp.4.a[[4]], bs_p.4.a[[1]])
region.5.a <- c(5, "Region 5", est_sp.5.a[[2]], est_sp.5.a[[3]], est_sp.5.a[[4]], bs_p.5.a[[1]])
region.6.a <- c(6, "Region 6", est_sp.6.a[[2]], est_sp.6.a[[3]], est_sp.6.a[[4]], bs_p.6.a[[1]])
region.7.a <- c(7, "Region 7", est_sp.7.a[[2]], est_sp.7.a[[3]], est_sp.7.a[[4]], bs_p.7.a[[1]])

regions.a <- as.data.frame(rbind(region.1.a, region.2.a, region.3.a, region.4.a, region.5.a, region.6.a, region.7.a))

regions.a <- regions.a %>% rename(region_id=V1,
                                  region=V2,
                                  xmin=V3,
                                  alpha=V4,
                                  ntail=V5,
                                  pKS=V6)
regions.a
write.csv(regions.a, "out/regions-results-1989-1999.csv")


region.1.b <- c(1, "Region 1", est_sp.1.b[[2]], est_sp.1.b[[3]], est_sp.1.b[[4]], bs_p.1.b[[1]])
region.2.b <- c(2, "Region 2", est_sp.2.b[[2]], est_sp.2.b[[3]], est_sp.2.b[[4]], bs_p.2.b[[1]])
region.3.b <- c(3, "Region 3", est_sp.3.b[[2]], est_sp.3.b[[3]], est_sp.3.b[[4]], bs_p.3.b[[1]])
region.4.b <- c(4, "Region 4", est_sp.4.b[[2]], est_sp.4.b[[3]], est_sp.4.b[[4]], bs_p.4.b[[1]])
region.5.b <- c(5, "Region 5", est_sp.5.b[[2]], est_sp.5.b[[3]], est_sp.5.b[[4]], bs_p.5.b[[1]])
region.6.b <- c(6, "Region 6", est_sp.6.b[[2]], est_sp.6.b[[3]], est_sp.6.b[[4]], bs_p.6.b[[1]])
region.7.b <- c(7, "Region 7", est_sp.7.b[[2]], est_sp.7.b[[3]], est_sp.7.b[[4]], bs_p.7.b[[1]])

regions.b <- as.data.frame(rbind(region.1.b, region.2.b, region.3.b, region.4.b, region.5.b, region.6.b, region.7.b))

regions.b <- regions.b %>% rename(region_id=V1,
                                  region=V2,
                                  xmin=V3,
                                  alpha=V4,
                                  ntail=V5,
                                  pKS=V6)
regions.b
write.csv(regions.b, "out/regions-results-2000-2009.csv")


region.1.c <- c(1, "Region 1", est_sp.1.c[[2]], est_sp.1.c[[3]], est_sp.1.c[[4]], bs_p.1.c[[1]])
region.2.c <- c(2, "Region 2", est_sp.2.c[[2]], est_sp.2.c[[3]], est_sp.2.c[[4]], bs_p.2.c[[1]])
region.3.c <- c(3, "Region 3", est_sp.3.c[[2]], est_sp.3.c[[3]], est_sp.3.c[[4]], bs_p.3.c[[1]])
region.4.c <- c(4, "Region 4", est_sp.4.c[[2]], est_sp.4.c[[3]], est_sp.4.c[[4]], bs_p.4.c[[1]])
region.5.c <- c(5, "Region 5", est_sp.5.c[[2]], est_sp.5.c[[3]], est_sp.5.c[[4]], bs_p.5.c[[1]])
region.6.c <- c(6, "Region 6", est_sp.6.c[[2]], est_sp.6.c[[3]], est_sp.6.c[[4]], bs_p.6.c[[1]])
region.7.c <- c(7, "Region 7", est_sp.7.c[[2]], est_sp.7.c[[3]], est_sp.7.c[[4]], bs_p.7.c[[1]])

regions.c <- as.data.frame(rbind(region.1.c, region.2.c, region.3.c, region.4.c, region.5.c, region.6.c, region.7.c))

regions.c <- regions.c %>% rename(region_id=V1,
                                  region=V2,
                                  xmin=V3,
                                  alpha=V4,
                                  ntail=V5,
                                  pKS=V6)
regions.c
write.csv(regions.c, "out/regions-results-2010-2018.csv")



# Maps preparation --------------------------------------------------------------------

col.adm1 <- st_read("data/col_adm1_ocha/col_admbnda_adm1_unodc_ocha.shp")
st_crs(col.adm1) <- 4326
col.adm1

col.adm1 %>% 
  select(admin1Name, admin1Pcod) %>%
  arrange(admin1Name, admin1Pcod) %>% 
  print(n = Inf)

col.adm1 <- col.adm1 %>% rename(dep_id = admin1Pcod)

col.adm1$dep_id[col.adm1$dep_id == "CO91"] <- 1
col.adm1$dep_id[col.adm1$dep_id == "CO05"] <- 2
col.adm1$dep_id[col.adm1$dep_id == "CO81"] <- 3
col.adm1$dep_id[col.adm1$dep_id == "CO08"] <- 4
col.adm1$dep_id[col.adm1$dep_id == "CO11"] <- 5
col.adm1$dep_id[col.adm1$dep_id == "CO13"] <- 6
col.adm1$dep_id[col.adm1$dep_id == "CO15"] <- 7
col.adm1$dep_id[col.adm1$dep_id == "CO17"] <- 9
col.adm1$dep_id[col.adm1$dep_id == "CO18"] <- 10
col.adm1$dep_id[col.adm1$dep_id == "CO85"] <- 11
col.adm1$dep_id[col.adm1$dep_id == "CO19"] <- 12
col.adm1$dep_id[col.adm1$dep_id == "CO20"] <- 13
col.adm1$dep_id[col.adm1$dep_id == "CO27"] <- 14
col.adm1$dep_id[col.adm1$dep_id == "CO23"] <- 8
col.adm1$dep_id[col.adm1$dep_id == "CO25"] <- 15
col.adm1$dep_id[col.adm1$dep_id == "CO94"] <- 16
col.adm1$dep_id[col.adm1$dep_id == "CO95"] <- 17
col.adm1$dep_id[col.adm1$dep_id == "CO41"] <- 18
col.adm1$dep_id[col.adm1$dep_id == "CO44"] <- 19
col.adm1$dep_id[col.adm1$dep_id == "CO47"] <- 20
col.adm1$dep_id[col.adm1$dep_id == "CO50"] <- 21
col.adm1$dep_id[col.adm1$dep_id == "CO52"] <- 22
col.adm1$dep_id[col.adm1$dep_id == "CO54"] <- 23
col.adm1$dep_id[col.adm1$dep_id == "CO86"] <- 24
col.adm1$dep_id[col.adm1$dep_id == "CO63"] <- 25
col.adm1$dep_id[col.adm1$dep_id == "CO66"] <- 26
col.adm1$dep_id[col.adm1$dep_id == "CO68"] <- 27
col.adm1$dep_id[col.adm1$dep_id == "CO70"] <- 28
col.adm1$dep_id[col.adm1$dep_id == "CO73"] <- 30
col.adm1$dep_id[col.adm1$dep_id == "CO76"] <- 31
col.adm1$dep_id[col.adm1$dep_id == "CO97"] <- 32
col.adm1$dep_id[col.adm1$dep_id == "CO99"] <- 33


# regions
col.adm1$region_id[col.adm1$dep_id == 19] <- 1
col.adm1$region_id[col.adm1$dep_id == 13] <- 1
col.adm1$region_id[col.adm1$dep_id == 20] <- 1
col.adm1$region_id[col.adm1$dep_id == 4] <- 1
col.adm1$region_id[col.adm1$dep_id == 6] <- 1
col.adm1$region_id[col.adm1$dep_id == 28] <- 1

col.adm1$region_id[col.adm1$dep_id == 2] <- 2
col.adm1$region_id[col.adm1$dep_id == 8] <- 2
col.adm1$region_id[col.adm1$dep_id == 14] <- 2

col.adm1$region_id[col.adm1$dep_id == 27] <- 3
col.adm1$region_id[col.adm1$dep_id == 23] <- 3

col.adm1$region_id[col.adm1$dep_id == 22] <- 4
col.adm1$region_id[col.adm1$dep_id == 12] <- 4
col.adm1$region_id[col.adm1$dep_id == 31] <- 4
col.adm1$region_id[col.adm1$dep_id == 26] <- 4
col.adm1$region_id[col.adm1$dep_id == 9] <- 4
col.adm1$region_id[col.adm1$dep_id == 25] <- 4

col.adm1$region_id[col.adm1$dep_id == 15] <- 5
col.adm1$region_id[col.adm1$dep_id == 7] <- 5
col.adm1$region_id[col.adm1$dep_id == 30] <- 5
col.adm1$region_id[col.adm1$dep_id == 18] <- 5
col.adm1$region_id[col.adm1$dep_id == 5] <- 5

col.adm1$region_id[col.adm1$dep_id == 10] <- 6
col.adm1$region_id[col.adm1$dep_id == 24] <- 6
col.adm1$region_id[col.adm1$dep_id == 1] <- 6

col.adm1$region_id[col.adm1$dep_id == 3] <- 7
col.adm1$region_id[col.adm1$dep_id == 11] <- 7
col.adm1$region_id[col.adm1$dep_id == 21] <- 7
col.adm1$region_id[col.adm1$dep_id == 33] <- 7
col.adm1$region_id[col.adm1$dep_id == 16] <- 7
col.adm1$region_id[col.adm1$dep_id == 17] <- 7
col.adm1$region_id[col.adm1$dep_id == 32] <- 7

col.adm1.2 <- col.adm1 %>% 
  group_by(region_id) %>% 
  summarise()

regions.2 <- merge(x=col.adm1.2, y=regions, by="region_id", all=T)
regions.2

regions.2.a <- merge(x=col.adm1.2, y=regions.a, by="region_id", all=T)
regions.2.a

regions.2.b <- merge(x=col.adm1.2, y=regions.b, by="region_id", all=T)
regions.2.b

regions.2.c <- merge(x=col.adm1.2, y=regions.c, by="region_id", all=T)
regions.2.c



# Maps - alpha ------------------------------------------------------------

regions.2$alpha <- as.numeric(regions.2$alpha)

pdf(file="figs/regions-1989-2018-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  ggtitle("Colombia regions, 1989-2018: Alpha") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.a$alpha <- as.numeric(regions.2.a$alpha)

pdf(file="figs/regions-1989-1999-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.a) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  ggtitle("Colombia regions, 1989-1999: Alpha") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.b$alpha <- as.numeric(regions.2.b$alpha)

pdf(file="figs/regions-2000-2009-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.b) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  ggtitle("Colombia regions, 2000-2009: Alpha") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.c$alpha <- as.numeric(regions.2.c$alpha)

pdf(file="figs/regions-2010-2018-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.c) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  ggtitle("Colombia regions, 2010-2018: Alpha") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()



# Maps - P-values for the Kolmogorov-Smirnov goodness of fit test ---------

regions.2$pKS <- as.numeric(regions.2$pKS)
regions.2 <- regions.2 %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/regions-1989-2018-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia regions, 1989-2018: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.a$pKS <- as.numeric(regions.2.a$pKS)
regions.2.a <- regions.2.a %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/regions-1989-1999-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.a) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia regions, 1989-1999: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.b$pKS <- as.numeric(regions.2.b$pKS)
regions.2.b <- regions.2.b %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/regions-2000-2009-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.b) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("#4682B4")) +
  ggtitle("Colombia regions, 2000-2009: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()

regions.2.c$pKS <- as.numeric(regions.2.c$pKS)
regions.2.c <- regions.2.c %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/regions-2010-2018-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = regions.2.c) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia regions, 2010-2018: P-value") + 
  labs(fill = "P-value") + 
  theme_bw() +
  coord_sf()
dev.off()


