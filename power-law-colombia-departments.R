## Preamble ###############################################################
# Fitting data to the power-law distribution: departments in the armed conflict in Colombia
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 12 July 2023
# Updated: 16 October 2023 (total N per department per period added)

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




# 1: Amazonas department --------------------------------------------------

amazonas.1 <- data %>% filter(dep_id == 1)
amazonas.1



# 2: Antioquia department  ------------------------------------------------

antioquia.2 <- data %>% filter(dep_id == 2)
n.2 <- length(antioquia.2$deaths_total)

m_sp.2 = displ$new(antioquia.2$deaths_total)
est_sp.2 = estimate_xmin(m_sp.2)
m_sp.2$setXmin(est_sp.2)
bs_p.2 = bootstrap_p(m_sp.2, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


antioquia.2.a <- data %>% filter(dep_id == 2 & year <= 1999)
n.2.a <- length(antioquia.2.a$deaths_total)


m_sp.2.a = displ$new(antioquia.2.a$deaths_total)
est_sp.2.a = estimate_xmin(m_sp.2.a)
m_sp.2.a$setXmin(est_sp.2.a)
bs_p.2.a = bootstrap_p(m_sp.2.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


antioquia.2.b <- data %>% filter(dep_id == 2 & year > 1999 & year <= 2009)
n.2.b <- length(antioquia.2.b$deaths_total)


m_sp.2.b = displ$new(antioquia.2.b$deaths_total)
est_sp.2.b = estimate_xmin(m_sp.2.b)
m_sp.2.b$setXmin(est_sp.2.b)
bs_p.2.b = bootstrap_p(m_sp.2.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


antioquia.2.c <- data %>% filter(dep_id == 2 & year > 2009)
n.2.c <- length(antioquia.2.c$deaths_total)

m_sp.2.c = displ$new(antioquia.2.c$deaths_total)
est_sp.2.c = estimate_xmin(m_sp.2.c)
m_sp.2.c$setXmin(est_sp.2.c)
bs_p.2.c = bootstrap_p(m_sp.2.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 3: Arauca department ----------------------------------------------------

arauca.3 <- data %>% filter(dep_id == 3)

n.3 <- length(arauca.3$deaths_total)

m_sp.3 = displ$new(arauca.3$deaths_total)
est_sp.3 = estimate_xmin(m_sp.3)
m_sp.3$setXmin(est_sp.3)
bs_p.3 = bootstrap_p(m_sp.3, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


arauca.3.a <- data %>% filter(dep_id == 3 & year <= 1999)
n.3.a <- length(arauca.3.a$deaths_total)

m_sp.3.a = displ$new(arauca.3.a$deaths_total)
est_sp.3.a = estimate_xmin(m_sp.3.a)
m_sp.3.a$setXmin(est_sp.3.a)
bs_p.3.a = bootstrap_p(m_sp.3.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


arauca.3.b <- data %>% filter(dep_id == 3 & year > 1999 & year <= 2009)
n.3.b <- length(arauca.3.b$deaths_total)

m_sp.3.b = displ$new(arauca.3.b$deaths_total)
est_sp.3.b = estimate_xmin(m_sp.3.b)
m_sp.3.b$setXmin(est_sp.3.b)
bs_p.3.b = bootstrap_p(m_sp.3.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


arauca.3.c <- data %>% filter(dep_id == 3 & year > 2009)
n.3.c <- length(arauca.3.c$deaths_total)

m_sp.3.c = displ$new(arauca.3.c$deaths_total)
est_sp.3.c = estimate_xmin(m_sp.3.c)
m_sp.3.c$setXmin(est_sp.3.c)
bs_p.3.c = bootstrap_p(m_sp.3.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 4: Atlantico department -------------------------------------------------

atlantico.4 <- data %>% filter(dep_id == 4)
atlantico.4



# 5: Bogota department ----------------------------------------------------

bogota.5 <- data %>% filter(dep_id == 5)

n.5 <- length(bogota.5$deaths_total)

m_sp.5 = displ$new(bogota.5$deaths_total)
est_sp.5 = estimate_xmin(m_sp.5)
m_sp.5$setXmin(est_sp.5)
bs_p.5 = bootstrap_p(m_sp.5, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bogota.5.a <- data %>% filter(dep_id == 5 & year <= 1999)
n.5.a <- length(bogota.5.a$deaths_total)

m_sp.5.a = displ$new(bogota.5.a$deaths_total)
est_sp.5.a = estimate_xmin(m_sp.5.a)
m_sp.5.a$setXmin(est_sp.5.a)
bs_p.5.a = bootstrap_p(m_sp.5.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bogota.5.b <- data %>% filter(dep_id == 5 & year > 1999 & year <= 2009)
n.5.b <- length(bogota.5.b$deaths_total)

m_sp.5.b = displ$new(bogota.5.b$deaths_total)
est_sp.5.b = estimate_xmin(m_sp.5.b)
m_sp.5.b$setXmin(est_sp.5.b)
bs_p.5.b = bootstrap_p(m_sp.5.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bogota.5.c <- data %>% filter(dep_id == 5 & year > 2009)
bogota.5.c



# 6: Bolivar department ---------------------------------------------------

bolivar.6 <- data %>% filter(dep_id == 6)
n.6 <- length(bolivar.6$deaths_total)

m_sp.6 = displ$new(bolivar.6$deaths_total)
est_sp.6 = estimate_xmin(m_sp.6)
m_sp.6$setXmin(est_sp.6)
bs_p.6 = bootstrap_p(m_sp.6, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bolivar.6.a <- data %>% filter(dep_id == 6 & year <= 1999)
n.6.a <- length(bolivar.6.a$deaths_total)

m_sp.6.a = displ$new(bolivar.6.a$deaths_total)
est_sp.6.a = estimate_xmin(m_sp.6.a)
m_sp.6.a$setXmin(est_sp.6.a)
bs_p.6.a = bootstrap_p(m_sp.6.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bolivar.6.b <- data %>% filter(dep_id == 6 & year > 1999 & year <= 2009)
n.6.b <- length(bolivar.6.b$deaths_total)

m_sp.6.b = displ$new(bolivar.6.b$deaths_total)
est_sp.6.b = estimate_xmin(m_sp.6.b)
m_sp.6.b$setXmin(est_sp.6.b)
bs_p.6.b = bootstrap_p(m_sp.6.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


bolivar.6.c <- data %>% filter(dep_id == 6 & year > 2009)
bolivar.6.c




# 7: Boyaca department ----------------------------------------------------

boyaca.7 <- data %>% filter(dep_id == 7)

n.7 <- length(boyaca.7$deaths_total)

m_sp.7 = displ$new(boyaca.7$deaths_total)
est_sp.7 = estimate_xmin(m_sp.7)
m_sp.7$setXmin(est_sp.7)
bs_p.7 = bootstrap_p(m_sp.7, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


boyaca.7.a <- data %>% filter(dep_id == 7 & year <= 1999)
n.7.a <- length(boyaca.7.a$deaths_total)

m_sp.7.a = displ$new(boyaca.7.a$deaths_total)
est_sp.7.a = estimate_xmin(m_sp.7.a)
m_sp.7.a$setXmin(est_sp.7.a)
bs_p.7.a = bootstrap_p(m_sp.7.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


boyaca.7.b <- data %>% filter(dep_id == 7 & year > 1999 & year <= 2009)
n.7.b <- length(boyaca.7.b$deaths_total)

m_sp.7.b = displ$new(boyaca.7.b$deaths_total)
est_sp.7.b = estimate_xmin(m_sp.7.b)
m_sp.7.b$setXmin(est_sp.7.b)

bs_p.7.b = bootstrap_p(m_sp.7.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1))


boyaca.7.c <- data %>% filter(dep_id == 7 & year > 2009)
boyaca.7.c


# 8: Córdoba department ---------------------------------------------------

cordoba.8 <- data %>% filter(dep_id == 8)

n.8 <- length(cordoba.8$deaths_total)

m_sp.8 = displ$new(cordoba.8$deaths_total)
est_sp.8 = estimate_xmin(m_sp.8)
m_sp.8$setXmin(est_sp.8)
bs_p.8 = bootstrap_p(m_sp.8, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cordoba.8.a <- data %>% filter(dep_id == 8 & year <= 1999)
n.8.a <- length(cordoba.8.a$deaths_total)

m_sp.8.a = displ$new(cordoba.8.a$deaths_total)
est_sp.8.a = estimate_xmin(m_sp.8.a)
m_sp.8.a$setXmin(est_sp.8.a)
bs_p.8.a = bootstrap_p(m_sp.8.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cordoba.8.b <- data %>% filter(dep_id == 8 & year > 1999 & year <= 2009)
n.8.b <- length(cordoba.8.b$deaths_total)

m_sp.8.b = displ$new(cordoba.8.b$deaths_total)
est_sp.8.b = estimate_xmin(m_sp.8.b)
m_sp.8.b$setXmin(est_sp.8.b)
bs_p.8.b = bootstrap_p(m_sp.8.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cordoba.8.c <- data %>% filter(dep_id == 8 & year > 2009)
cordoba.8.c



# 9: Caldas department ----------------------------------------------------

caldas.9 <- data %>% filter(dep_id == 9)

n.9<- length(caldas.9$deaths_total)

m_sp.9 = displ$new(caldas.9$deaths_total)
est_sp.9 = estimate_xmin(m_sp.9)
m_sp.9$setXmin(est_sp.9)
bs_p.9 = bootstrap_p(m_sp.9, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


caldas.9.a <- data %>% filter(dep_id == 9 & year <= 1999)
caldas.9.a


caldas.9.b <- data %>% filter(dep_id == 9 & year > 1999 & year <= 2009)
n.9.b <- length(caldas.9.b$deaths_total)

m_sp.9.b = displ$new(caldas.9.b$deaths_total)
est_sp.9.b = estimate_xmin(m_sp.9.b)
m_sp.9.b$setXmin(est_sp.9.b)
bs_p.9.b = bootstrap_p(m_sp.9.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


caldas.9.c <- data %>% filter(dep_id == 9 & year > 2009)
caldas.9.c


# 10: Caquetá department --------------------------------------------------

caqueta.10 <- data %>% filter(dep_id == 10)

n.10 <- length(caqueta.10$deaths_total)

m_sp.10 = displ$new(caqueta.10$deaths_total)
est_sp.10 = estimate_xmin(m_sp.10)
m_sp.10$setXmin(est_sp.10)
bs_p.10 = bootstrap_p(m_sp.10, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


caqueta.10.a <- data %>% filter(dep_id == 10 & year <= 1999)
n.10.a <- length(caqueta.10.a$deaths_total)

m_sp.10.a = displ$new(caqueta.10.a$deaths_total)
est_sp.10.a = estimate_xmin(m_sp.10.a)
m_sp.10.a$setXmin(est_sp.10.a)
bs_p.10.a = bootstrap_p(m_sp.10.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


caqueta.10.b <- data %>% filter(dep_id == 10 & year > 1999 & year <= 2009)
n.10.b <- length(caqueta.10.b$deaths_total)

m_sp.10.b = displ$new(caqueta.10.b$deaths_total)
est_sp.10.b = estimate_xmin(m_sp.10.b)
m_sp.10.b$setXmin(est_sp.10.b)
bs_p.10.b = bootstrap_p(m_sp.10.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


caqueta.10.c <- data %>% filter(dep_id == 10 & year > 2009)
n.10.c <- length(caqueta.10.c$deaths_total)

m_sp.10.c = displ$new(caqueta.10.c$deaths_total)
est_sp.10.c = estimate_xmin(m_sp.10.c)
m_sp.10.c$setXmin(est_sp.10.c)
bs_p.10.c = bootstrap_p(m_sp.10.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 11: Casanare department -------------------------------------------------

casanare.11 <- data %>% filter(dep_id == 11)

n.11 <- length(casanare.11$deaths_total)

m_sp.11 = displ$new(casanare.11$deaths_total)
est_sp.11 = estimate_xmin(m_sp.11)
m_sp.11$setXmin(est_sp.11)
bs_p.11 = bootstrap_p(m_sp.11, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


casanare.11.a <- data %>% filter(dep_id == 11 & year <= 1999)
n.11.a <- length(casanare.11.a$deaths_total)

m_sp.11.a = displ$new(casanare.11.a$deaths_total)
est_sp.11.a = estimate_xmin(m_sp.11.a)
m_sp.11.a$setXmin(est_sp.11.a)
bs_p.11.a = bootstrap_p(m_sp.11.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


casanare.11.b <- data %>% filter(dep_id == 11 & year > 1999 & year <= 2009)
n.11.b <- length(casanare.11.b$deaths_total)

m_sp.11.b = displ$new(casanare.11.b$deaths_total)
est_sp.11.b = estimate_xmin(m_sp.11.b)
m_sp.11.b$setXmin(est_sp.11.b)
bs_p.11.b = bootstrap_p(m_sp.11.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


casanare.11.c <- data %>% filter(dep_id == 11 & year > 2009)
casanare.11.c



# 12: Cauca department --------------------------------------------------------

cauca.12 <- data %>% filter(dep_id == 12)

n.12 <- length(cauca.12$deaths_total)

m_sp.12 = displ$new(cauca.12$deaths_total)
est_sp.12 = estimate_xmin(m_sp.12)
m_sp.12$setXmin(est_sp.12)
bs_p.12 = bootstrap_p(m_sp.12, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cauca.12.a <- data %>% filter(dep_id == 12 & year <= 1999)
n.12.a <- length(cauca.12.a$deaths_total)

m_sp.12.a = displ$new(cauca.12.a$deaths_total)
est_sp.12.a = estimate_xmin(m_sp.12.a)
m_sp.12.a$setXmin(est_sp.12.a)
bs_p.12.a = bootstrap_p(m_sp.12.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cauca.12.b <- data %>% filter(dep_id == 12 & year > 1999 & year <= 2009)
n.12.b <- length(cauca.12.b$deaths_total)

m_sp.12.b = displ$new(cauca.12.b$deaths_total)
est_sp.12.b = estimate_xmin(m_sp.12.b)
m_sp.12.b$setXmin(est_sp.12.b)
bs_p.12.b = bootstrap_p(m_sp.12.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cauca.12.c <- data %>% filter(dep_id == 12 & year > 2009)
n.12.c <- length(cauca.12.c$deaths_total)

m_sp.12.c = displ$new(cauca.12.c$deaths_total)
est_sp.12.c = estimate_xmin(m_sp.12.c)
m_sp.12.c$setXmin(est_sp.12.c)
bs_p.12.c = bootstrap_p(m_sp.12.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 13: Cesar department ----------------------------------------------------

cesar.13 <- data %>% filter(dep_id == 13)
n.13 <- length(cesar.13$deaths_total)

m_sp.13 = displ$new(cesar.13$deaths_total)
est_sp.13 = estimate_xmin(m_sp.13)
m_sp.13$setXmin(est_sp.13)
bs_p.13 = bootstrap_p(m_sp.13, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cesar.13.a <- data %>% filter(dep_id == 13 & year <= 1999)
n.13.a <- length(cesar.13.a$deaths_total)

m_sp.13.a = displ$new(cesar.13.a$deaths_total)
est_sp.13.a = estimate_xmin(m_sp.13.a)
m_sp.13.a$setXmin(est_sp.13.a)
bs_p.13.a = bootstrap_p(m_sp.13.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cesar.13.b <- data %>% filter(dep_id == 13 & year > 1999 & year <= 2009)
n.13.b <- length(cesar.13.b$deaths_total)

m_sp.13.b = displ$new(cesar.13.b$deaths_total)
est_sp.13.b = estimate_xmin(m_sp.13.b)
m_sp.13.b$setXmin(est_sp.13.b)
bs_p.13.b = bootstrap_p(m_sp.13.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cesar.13.c <- data %>% filter(dep_id == 13 & year > 2009)
cesar.13.c



# 14: Chocó department ----------------------------------------------------

choco.14 <- data %>% filter(dep_id == 14)
n.14 <- length(choco.14$deaths_total)

m_sp.14 = displ$new(choco.14$deaths_total)
est_sp.14 = estimate_xmin(m_sp.14)
m_sp.14$setXmin(est_sp.14)
bs_p.14 = bootstrap_p(m_sp.14, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


choco.14.a <- data %>% filter(dep_id == 14 & year <= 1999)
n.14.a <- length(choco.14.a$deaths_total)

m_sp.14.a = displ$new(choco.14.a$deaths_total)
est_sp.14.a = estimate_xmin(m_sp.14.a)
m_sp.14.a$setXmin(est_sp.14.a)
bs_p.14.a = bootstrap_p(m_sp.14.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


choco.14.b <- data %>% filter(dep_id == 14 & year > 1999 & year <= 2009)
n.14.b <- length(choco.14.b$deaths_total)

m_sp.14.b = displ$new(choco.14.b$deaths_total)
est_sp.14.b = estimate_xmin(m_sp.14.b)
m_sp.14.b$setXmin(est_sp.14.b)
bs_p.14.b = bootstrap_p(m_sp.14.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


choco.14.c <- data %>% filter(dep_id == 14 & year > 2009)
n.14.c <- length(choco.14.c$deaths_total)

m_sp.14.c = displ$new(choco.14.c$deaths_total)
est_sp.14.c = estimate_xmin(m_sp.14.c)
m_sp.14.c$setXmin(est_sp.14.c)
bs_p.14.c = bootstrap_p(m_sp.14.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 15: Cundinamarca department ---------------------------------------------

cundinamarca.15 <- data %>% filter(dep_id == 15)
n.15 <- length(cundinamarca.15$deaths_total)

m_sp.15 = displ$new(cundinamarca.15$deaths_total)
est_sp.15 = estimate_xmin(m_sp.15)
m_sp.15$setXmin(est_sp.15)
bs_p.15 = bootstrap_p(m_sp.15, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cundinamarca.15.a <- data %>% filter(dep_id == 15 & year <= 1999)
n.15.a <- length(cundinamarca.15.a$deaths_total)

m_sp.15.a = displ$new(cundinamarca.15.a$deaths_total)
est_sp.15.a = estimate_xmin(m_sp.15.a)
m_sp.15.a$setXmin(est_sp.15.a)
bs_p.15.a = bootstrap_p(m_sp.15.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cundinamarca.15.b <- data %>% filter(dep_id == 15 & year > 1999 & year <= 2009)
n.15.b <- length(cundinamarca.15.b$deaths_total)

m_sp.15.b = displ$new(cundinamarca.15.b$deaths_total)
est_sp.15.b = estimate_xmin(m_sp.15.b)
m_sp.15.b$setXmin(est_sp.15.b)
bs_p.15.b = bootstrap_p(m_sp.15.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


cundinamarca.15.c <- data %>% filter(dep_id == 15 & year > 2009)
cundinamarca.15.c



# 16: Guainia department --------------------------------------------------

guainia.16 <- data %>% filter(dep_id == 16)
guainia.16



# 17: Guaviare department -------------------------------------------------

guaviare.17 <- data %>% filter(dep_id == 17)
n.17 <- length(guaviare.17$deaths_total)


m_sp.17 = displ$new(guaviare.17$deaths_total)
est_sp.17 = estimate_xmin(m_sp.17)
m_sp.17$setXmin(est_sp.17)
bs_p.17 = bootstrap_p(m_sp.17, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


guaviare.17.a <- data %>% filter(dep_id == 17 & year <= 1999)
n.17.a <- length(guaviare.17.a$deaths_total)

m_sp.17.a = displ$new(guaviare.17.a$deaths_total)
est_sp.17.a = estimate_xmin(m_sp.17.a)
m_sp.17.a$setXmin(est_sp.17.a)
bs_p.17.a = bootstrap_p(m_sp.17.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


guaviare.17.b <- data %>% filter(dep_id == 17 & year > 1999 & year <= 2009)
n.17.b <- length(guaviare.17.b$deaths_total)

m_sp.17.b = displ$new(guaviare.17.b$deaths_total)
est_sp.17.b = estimate_xmin(m_sp.17.b)
m_sp.17.b$setXmin(est_sp.17.b)
bs_p.17.b = bootstrap_p(m_sp.17.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


guaviare.17.c <- data %>% filter(dep_id == 17 & year > 2009)
n.17.c <- length(guaviare.17.c$deaths_total)

m_sp.17.c = displ$new(guaviare.17.c$deaths_total)
est_sp.17.c = estimate_xmin(m_sp.17.c)
m_sp.17.c$setXmin(est_sp.17.c)
bs_p.17.c = bootstrap_p(m_sp.17.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 18: Huila department ----------------------------------------------------

huila.18 <- data %>% filter(dep_id == 18)
n.18 <- length(huila.18$deaths_total)

m_sp.18 = displ$new(huila.18$deaths_total)
est_sp.18 = estimate_xmin(m_sp.18)
m_sp.18$setXmin(est_sp.18)
bs_p.18 = bootstrap_p(m_sp.18, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


huila.18.a <- data %>% filter(dep_id == 18 & year <= 1999)
n.18.a <- length(huila.18.a$deaths_total)

m_sp.18.a = displ$new(huila.18.a$deaths_total)
est_sp.18.a = estimate_xmin(m_sp.18.a)
m_sp.18.a$setXmin(est_sp.18.a)
bs_p.18.a = bootstrap_p(m_sp.18.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


huila.18.b <- data %>% filter(dep_id == 18 & year > 1999 & year <= 2009)
n.18.b <- length(huila.18.b$deaths_total)

m_sp.18.b = displ$new(huila.18.b$deaths_total)
est_sp.18.b = estimate_xmin(m_sp.18.b)
m_sp.18.b$setXmin(est_sp.18.b)
bs_p.18.b = bootstrap_p(m_sp.18.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


huila.18.c <- data %>% filter(dep_id == 18 & year > 2009)
n.18.c <- length(huila.18.c$deaths_total)

m_sp.18.c = displ$new(huila.18.c$deaths_total)
est_sp.18.c = estimate_xmin(m_sp.18.c)
m_sp.18.c$setXmin(est_sp.18.c)
bs_p.18.c = bootstrap_p(m_sp.18.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 19: La Guajira department -----------------------------------------------

laguajira.19 <- data %>% filter(dep_id == 19)
n.19 <- length(laguajira.19$deaths_total)

m_sp.19 = displ$new(laguajira.19$deaths_total)
est_sp.19 = estimate_xmin(m_sp.19)
m_sp.19$setXmin(est_sp.19)
bs_p.19 = bootstrap_p(m_sp.19, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


laguajira.19.a <- data %>% filter(dep_id == 19 & year <= 1999)
laguajira.19.a


laguajira.19.b <- data %>% filter(dep_id == 19 & year > 1999 & year <= 2009)
n.19.b <- length(laguajira.19.b$deaths_total)

m_sp.19.b = displ$new(laguajira.19.b$deaths_total)
est_sp.19.b = estimate_xmin(m_sp.19.b)
m_sp.19.b$setXmin(est_sp.19.b)
bs_p.19.b = bootstrap_p(m_sp.19.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


laguajira.19.c <- data %>% filter(dep_id == 19 & year > 2009)
laguajira.19.c



# 20: Magdalena department ------------------------------------------------

magdalena.20 <- data %>% filter(dep_id == 20)
n.20 <- length(magdalena.20$deaths_total)

m_sp.20 = displ$new(magdalena.20$deaths_total)
est_sp.20 = estimate_xmin(m_sp.20)
m_sp.20$setXmin(est_sp.20)
bs_p.20 = bootstrap_p(m_sp.20, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


magdalena.20.a <- data %>% filter(dep_id == 20 & year <= 1999)
n.20.a <- length(magdalena.20.a$deaths_total)

m_sp.20.a = displ$new(magdalena.20.a$deaths_total)
est_sp.20.a = estimate_xmin(m_sp.20.a)
m_sp.20.a$setXmin(est_sp.20.a)
bs_p.20.a = bootstrap_p(m_sp.20.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


magdalena.20.b <- data %>% filter(dep_id == 20 & year > 1999 & year <= 2009)
n.20.b <- length(magdalena.20.b$deaths_total)

m_sp.20.b = displ$new(magdalena.20.b$deaths_total)
est_sp.20.b = estimate_xmin(m_sp.20.b)
m_sp.20.b$setXmin(est_sp.20.b)
bs_p.20.b = bootstrap_p(m_sp.20.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


magdalena.20.c <- data %>% filter(dep_id == 20 & year > 2009)
magdalena.20.c




# 21: Meta department -----------------------------------------------------

meta.21 <- data %>% filter(dep_id == 21)
n.21 <- length(meta.21$deaths_total)

m_sp.21 = displ$new(meta.21$deaths_total)
est_sp.21 = estimate_xmin(m_sp.21)
m_sp.21$setXmin(est_sp.21)
bs_p.21 = bootstrap_p(m_sp.21, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


meta.21.a <- data %>% filter(dep_id == 21 & year <= 1999)
n.21.a <- length(meta.21.a$deaths_total)

m_sp.21.a = displ$new(meta.21.a$deaths_total)
est_sp.21.a = estimate_xmin(m_sp.21.a)
m_sp.21.a$setXmin(est_sp.21.a)
bs_p.21.a = bootstrap_p(m_sp.21.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


meta.21.b <- data %>% filter(dep_id == 21 & year > 1999 & year <= 2009)
n.21.b <- length(meta.21.b$deaths_total)

m_sp.21.b = displ$new(meta.21.b$deaths_total)
est_sp.21.b = estimate_xmin(m_sp.21.b)
m_sp.21.b$setXmin(est_sp.21.b)
bs_p.21.b = bootstrap_p(m_sp.21.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


meta.21.c <- data %>% filter(dep_id == 21 & year > 2009)
n.21.c <- length(meta.21.c$deaths_total)

m_sp.21.c = displ$new(meta.21.c$deaths_total)
est_sp.21.c = estimate_xmin(m_sp.21.c)
m_sp.21.c$setXmin(est_sp.21.c)
bs_p.21.c = bootstrap_p(m_sp.21.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)




# 22: Nariño department ---------------------------------------------------

narino.22 <- data %>% filter(dep_id == 22)
n.22 <- length(narino.22$deaths_total)

m_sp.22 = displ$new(narino.22$deaths_total)
est_sp.22 = estimate_xmin(m_sp.22)
m_sp.22$setXmin(est_sp.22)
bs_p.22 = bootstrap_p(m_sp.22, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


narino.22.a <- data %>% filter(dep_id == 22 & year <= 1999)
n.22.a <- length(narino.22.a$deaths_total)

m_sp.22.a = displ$new(narino.22.a$deaths_total)
est_sp.22.a = estimate_xmin(m_sp.22.a)
m_sp.22.a$setXmin(est_sp.22.a)
bs_p.22.a = bootstrap_p(m_sp.22.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


narino.22.b <- data %>% filter(dep_id == 22 & year > 1999 & year <= 2009)
n.22.b <- length(narino.22.b$deaths_total)

m_sp.22.b = displ$new(narino.22.b$deaths_total)
est_sp.22.b = estimate_xmin(m_sp.22.b)
m_sp.22.b$setXmin(est_sp.22.b)
bs_p.22.b = bootstrap_p(m_sp.22.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


narino.22.c <- data %>% filter(dep_id == 22 & year > 2009)
n.22.c <- length(narino.22.c$deaths_total)

m_sp.22.c = displ$new(narino.22.c$deaths_total)
est_sp.22.c = estimate_xmin(m_sp.22.c)
m_sp.22.c$setXmin(est_sp.22.c)
bs_p.22.c = bootstrap_p(m_sp.22.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)




# 23: Norte de Santander department ---------------------------------------

nortesantander.23 <- data %>% filter(dep_id == 23)
n.23 <- length(nortesantander.23$deaths_total)

m_sp.23 = displ$new(nortesantander.23$deaths_total)
est_sp.23 = estimate_xmin(m_sp.23)
m_sp.23$setXmin(est_sp.23)
bs_p.23 = bootstrap_p(m_sp.23, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


nortesantander.23.a <- data %>% filter(dep_id == 23 & year <= 1999)
n.23.a <- length(nortesantander.23.a$deaths_total)

m_sp.23.a = displ$new(nortesantander.23.a$deaths_total)
est_sp.23.a = estimate_xmin(m_sp.23.a)
m_sp.23.a$setXmin(est_sp.23.a)
bs_p.23.a = bootstrap_p(m_sp.23.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


nortesantander.23.b <- data %>% filter(dep_id == 23 & year > 1999 & year <= 2009)
n.23.b <- length(nortesantander.23.b$deaths_total)

m_sp.23.b = displ$new(nortesantander.23.b$deaths_total)
est_sp.23.b = estimate_xmin(m_sp.23.b)
m_sp.23.b$setXmin(est_sp.23.b)
bs_p.23.b = bootstrap_p(m_sp.23.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


nortesantander.23.c <- data %>% filter(dep_id == 23 & year > 2009)
n.23.c <- length(nortesantander.23.c$deaths_total)

m_sp.23.c = displ$new(nortesantander.23.c$deaths_total)
est_sp.23.c = estimate_xmin(m_sp.23.c)
m_sp.23.c$setXmin(est_sp.23.c)
bs_p.23.c = bootstrap_p(m_sp.23.c, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)



# 24: Putumayo department -------------------------------------------------

putumayo.24 <- data %>% filter(dep_id == 24)
n.24 <- length(putumayo.24$deaths_total)


m_sp.24 = displ$new(putumayo.24$deaths_total)
est_sp.24 = estimate_xmin(m_sp.24)
m_sp.24$setXmin(est_sp.24)
bs_p.24 = bootstrap_p(m_sp.24, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


putumayo.24.a <- data %>% filter(dep_id == 24 & year <= 1999)
n.24.a <- length(putumayo.24.a$deaths_total)

m_sp.24.a = displ$new(putumayo.24.a$deaths_total)
est_sp.24.a = estimate_xmin(m_sp.24.a)
m_sp.24.a$setXmin(est_sp.24.a)
bs_p.24.a = bootstrap_p(m_sp.24.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


putumayo.24.b <- data %>% filter(dep_id == 24 & year > 1999 & year <= 2009)
n.24.b <- length(putumayo.24.b$deaths_total)

m_sp.24.b = displ$new(putumayo.24.b$deaths_total)
est_sp.24.b = estimate_xmin(m_sp.24.b)
m_sp.24.b$setXmin(est_sp.24.b)
bs_p.24.b = bootstrap_p(m_sp.24.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


putumayo.24.c <- data %>% filter(dep_id == 24 & year > 2009)
putumayo.24.c



# 25: Quindio department --------------------------------------------------

quindio.25 <- data %>% filter(dep_id == 25)
n.25 <- length(quindio.25$deaths_total)


m_sp.25 = displ$new(quindio.25$deaths_total)
est_sp.25 = estimate_xmin(m_sp.25)
m_sp.25$setXmin(est_sp.25)
bs_p.25 = bootstrap_p(m_sp.25, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


quindio.25.a <- data %>% filter(dep_id == 25 & year <= 1999)
quindio.25.a


quindio.25.b <- data %>% filter(dep_id == 25 & year > 1999 & year <= 2009)
n.25.b <- length(quindio.25.b$deaths_total)

m_sp.25.b = displ$new(quindio.25.b$deaths_total)
est_sp.25.b = estimate_xmin(m_sp.25.b)
m_sp.25.b$setXmin(est_sp.25.b)
bs_p.25.b = bootstrap_p(m_sp.25.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


quindio.25.c <- data %>% filter(dep_id == 25 & year > 2009)
quindio.25.c




# 26: Risaralda department ------------------------------------------------

risaralda.26 <- data %>% filter(dep_id == 26)
n.26 <- length(risaralda.26$deaths_total)


m_sp.26 = displ$new(risaralda.26$deaths_total)
est_sp.26 = estimate_xmin(m_sp.26)
m_sp.26$setXmin(est_sp.26)
bs_p.26 = bootstrap_p(m_sp.26, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


risalda.26.a <- data %>% filter(dep_id == 26 & year <= 1999)
risalda.26.a


risalda.26.b <- data %>% filter(dep_id == 26 & year > 1999 & year <= 2009)
n.26.b <- length(risalda.26.b$deaths_total)

m_sp.26.b = displ$new(risalda.26.b$deaths_total)
est_sp.26.b = estimate_xmin(m_sp.26.b)
m_sp.26.b$setXmin(est_sp.26.b)
bs_p.26.b = bootstrap_p(m_sp.26.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


risalda.26.c <- data %>% filter(dep_id == 26 & year > 2009)
risalda.26.c



# 27: Santander department ------------------------------------------------

santander.27 <- data %>% filter(dep_id == 27)
n.27 <- length(santander.27$deaths_total)


m_sp.27 = displ$new(santander.27$deaths_total)
est_sp.27 = estimate_xmin(m_sp.27)
m_sp.27$setXmin(est_sp.27)
bs_p.27 = bootstrap_p(m_sp.27, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


santander.27.a <- data %>% filter(dep_id == 27 & year <= 1999)
n.27.a <- length(santander.27.a$deaths_total)

m_sp.27.a = displ$new(santander.27.a$deaths_total)
est_sp.27.a = estimate_xmin(m_sp.27.a)
m_sp.27.a$setXmin(est_sp.27.a)
bs_p.27.a = bootstrap_p(m_sp.27.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


santander.27.b <- data %>% filter(dep_id == 27 & year > 1999 & year <= 2009)
n.27.b <- length(santander.27.b$deaths_total)

m_sp.27.b = displ$new(santander.27.b$deaths_total)
est_sp.27.b = estimate_xmin(m_sp.27.b)
m_sp.27.b$setXmin(est_sp.27.b)
bs_p.27.b = bootstrap_p(m_sp.27.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


santander.27.c <- data %>% filter(dep_id == 27 & year > 2009)
santander.27.c


# 28: Sucre department ----------------------------------------------------

sucre.28 <- data %>% filter(dep_id == 28)
n.28 <- length(sucre.28$deaths_total)

m_sp.28 = displ$new(sucre.28$deaths_total)
est_sp.28 = estimate_xmin(m_sp.28)
m_sp.28$setXmin(est_sp.28)
bs_p.28 = bootstrap_p(m_sp.28, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


sucre.28.a <- data %>% filter(dep_id == 28 & year <= 1999)
n.28.a <- length(sucre.28.a$deaths_total)

m_sp.28.a = displ$new(sucre.28.a$deaths_total)
est_sp.28.a = estimate_xmin(m_sp.28.a)
m_sp.28.a$setXmin(est_sp.28.a)
bs_p.28.a = bootstrap_p(m_sp.28.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


sucre.28.b <- data %>% filter(dep_id == 28 & year > 1999 & year <= 2009)
n.28.b <- length(sucre.28.b$deaths_total)

m_sp.28.b = displ$new(sucre.28.b$deaths_total)
est_sp.28.b = estimate_xmin(m_sp.28.b)
m_sp.28.b$setXmin(est_sp.28.b)
bs_p.28.b = bootstrap_p(m_sp.28.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


sucre.28.c <- data %>% filter(dep_id == 28 & year >  2009)
sucre.28.c



# 30: Tolima  department --------------------------------------------------

tolima.30 <- data %>% filter(dep_id == 30)
n.30 <- length(tolima.30$deaths_total)


m_sp.30 = displ$new(tolima.30$deaths_total)
est_sp.30 = estimate_xmin(m_sp.30)
m_sp.30$setXmin(est_sp.30)
bs_p.30 = bootstrap_p(m_sp.30, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


tolima.30.a <- data %>% filter(dep_id == 30 & year <= 1999)
n.30.a <- length(tolima.30.a$deaths_total)

m_sp.30.a = displ$new(tolima.30.a$deaths_total)
est_sp.30.a = estimate_xmin(m_sp.30.a)
m_sp.30.a$setXmin(est_sp.30.a)
bs_p.30.a = bootstrap_p(m_sp.30.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


tolima.30.b <- data %>% filter(dep_id == 30 & year > 1999 & year <= 2009)
n.30.b <- length(tolima.30.b$deaths_total)

m_sp.30.b = displ$new(tolima.30.b$deaths_total)
est_sp.30.b = estimate_xmin(m_sp.30.b)
m_sp.30.b$setXmin(est_sp.30.b)
bs_p.30.b = bootstrap_p(m_sp.30.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


tolima.30.c <- data %>% filter(dep_id == 30 & year > 2009)
tolima.30.c



# 31: Valle del Cauca  department -----------------------------------------

valledelcauca.31 <- data %>% filter(dep_id == 31)
n.31 <- length(valledelcauca.31$deaths_total)

m_sp.31 = displ$new(valledelcauca.31$deaths_total)
est_sp.31 = estimate_xmin(m_sp.31)
m_sp.31$setXmin(est_sp.31)
bs_p.31 = bootstrap_p(m_sp.31, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


valledelcauca.31.a <- data %>% filter(dep_id == 31 & year <= 1999)
n.31.a <- length(valledelcauca.31.a$deaths_total)

m_sp.31.a = displ$new(valledelcauca.31.a$deaths_total)
est_sp.31.a = estimate_xmin(m_sp.31.a)
m_sp.31.a$setXmin(est_sp.31.a)
bs_p.31.a = bootstrap_p(m_sp.31.a, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


valledelcauca.31.b <- data %>% filter(dep_id == 31 & year > 1999 & year <= 2009)
n.31.b <- length(valledelcauca.31.b$deaths_total)

m_sp.31.b = displ$new(valledelcauca.31.b$deaths_total)
est_sp.31.b = estimate_xmin(m_sp.31.b)
m_sp.31.b$setXmin(est_sp.31.b)
bs_p.31.b = bootstrap_p(m_sp.31.b, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


valledelcauca.31.c <- data %>% filter(dep_id == 31 & year > 2009)
valledelcauca.31.c



# 32: Vaupés department ---------------------------------------------------

vaupes.32 <- data %>% filter(dep_id == 32)
vaupes.32



# 33: Vichada department --------------------------------------------------

vichada.33 <- data %>% filter(dep_id == 33)
n.33 <- length(vichada.33$deaths_total)


m_sp.33 = displ$new(vichada.33$deaths_total)
est_sp.33 = estimate_xmin(m_sp.33)
m_sp.33$setXmin(est_sp.33)
bs_p.33 = bootstrap_p(m_sp.33, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1), seed=2012)


vichada.33.a <- data %>% filter(dep_id == 33 & year <= 1999)
vichada.33.a


vichada.33.b <- data %>% filter(dep_id == 33 & year > 1999 & year <= 2009)
vichada.33.b


vichada.33.c <- data %>% filter(dep_id == 33 & year > 2009)
vichada.33.c




# Results for departments -----------------------------------------------------
  
dep.2 <- c(2, n.2, est_sp.2[[2]], est_sp.2[[3]], est_sp.2[[4]], bs_p.2[[1]])
dep.3 <- c(3, n.3, est_sp.3[[2]], est_sp.3[[3]], est_sp.3[[4]], bs_p.3[[1]])
dep.5 <- c(5, n.5, est_sp.5[[2]], est_sp.5[[3]], est_sp.5[[4]], bs_p.5[[1]])
dep.6 <- c(6, n.6, est_sp.6[[2]], est_sp.6[[3]], est_sp.6[[4]], bs_p.6[[1]])
dep.7 <- c(7, n.7, est_sp.7[[2]], est_sp.7[[3]], est_sp.7[[4]], bs_p.7[[1]])
dep.8 <- c(8, n.8, est_sp.8[[2]], est_sp.8[[3]], est_sp.8[[4]], bs_p.8[[1]])
dep.9 <- c(9, n.9, est_sp.9[[2]], est_sp.9[[3]], est_sp.9[[4]], bs_p.9[[1]])
dep.10 <- c(10, n.10, est_sp.10[[2]], est_sp.10[[3]], est_sp.10[[4]], bs_p.10[[1]])
dep.11 <- c(11, n.11, est_sp.11[[2]], est_sp.11[[3]], est_sp.11[[4]], bs_p.11[[1]])
dep.12 <- c(12, n.12, est_sp.12[[2]], est_sp.12[[3]], est_sp.12[[4]], bs_p.12[[1]])
dep.13 <- c(13, n.13, est_sp.13[[2]], est_sp.13[[3]], est_sp.13[[4]], bs_p.13[[1]])
dep.14 <- c(14, n.14, est_sp.14[[2]], est_sp.14[[3]], est_sp.14[[4]], bs_p.14[[1]])
dep.15 <- c(15, n.15, est_sp.15[[2]], est_sp.15[[3]], est_sp.15[[4]], bs_p.15[[1]])
dep.17 <- c(17, n.17, est_sp.17[[2]], est_sp.17[[3]], est_sp.17[[4]], bs_p.17[[1]])
dep.18 <- c(18, n.18, est_sp.18[[2]], est_sp.18[[3]], est_sp.18[[4]], bs_p.18[[1]])
dep.19 <- c(19, n.19, est_sp.19[[2]], est_sp.19[[3]], est_sp.19[[4]], bs_p.19[[1]])
dep.20 <- c(20, n.20, est_sp.20[[2]], est_sp.20[[3]], est_sp.20[[4]], bs_p.20[[1]])
dep.21 <- c(21, n.21, est_sp.21[[2]], est_sp.21[[3]], est_sp.21[[4]], bs_p.21[[1]])
dep.22 <- c(22, n.22, est_sp.22[[2]], est_sp.22[[3]], est_sp.22[[4]], bs_p.22[[1]])
dep.23 <- c(23, n.23, est_sp.23[[2]], est_sp.23[[3]], est_sp.23[[4]], bs_p.23[[1]])
dep.24 <- c(24, n.24, est_sp.24[[2]], est_sp.24[[3]], est_sp.24[[4]], bs_p.24[[1]])
dep.25 <- c(25, n.25, est_sp.25[[2]], est_sp.25[[3]], est_sp.25[[4]], bs_p.25[[1]])
dep.26 <- c(26, n.26, est_sp.26[[2]], est_sp.26[[3]], est_sp.26[[4]], bs_p.26[[1]])
dep.27 <- c(27, n.27, est_sp.27[[2]], est_sp.27[[3]], est_sp.27[[4]], bs_p.27[[1]])
dep.28 <- c(28, n.28, est_sp.28[[2]], est_sp.28[[3]], est_sp.28[[4]], bs_p.28[[1]])
dep.30 <- c(30, n.30, est_sp.30[[2]], est_sp.30[[3]], est_sp.30[[4]], bs_p.30[[1]])
dep.31 <- c(31, n.31, est_sp.31[[2]], est_sp.31[[3]], est_sp.31[[4]], bs_p.31[[1]])
dep.33 <- c(33, n.33, est_sp.33[[2]], est_sp.33[[3]], est_sp.33[[4]], bs_p.33[[1]])

departments <- as.data.frame(rbind(dep.2, dep.3, dep.5, dep.6, dep.7, dep.8,
                                   dep.9, dep.10, dep.11, dep.12, dep.13,
                                   dep.14, dep.15, dep.17, dep.18, dep.19,
                                   dep.20, dep.21, dep.22, dep.23, dep.24, dep.25,
                                   dep.26, dep.27, dep.28, dep.30, dep.31, dep.33))

departments <- departments %>% rename(dep_id=V1,
                                      n=V2,
                                      xmin=V3,
                                      alpha=V4,
                                      ntail=V5,
                                      pKS=V6)
departments
write.csv(departments, "out/departments-results-1989-2018.csv")


dep.2.a <- c(2, n.2.a, est_sp.2.a[[2]], est_sp.2.a[[3]], est_sp.2.a[[4]], bs_p.2.a[[1]])
dep.3.a <- c(3, n.3.a, est_sp.3.a[[2]], est_sp.3.a[[3]], est_sp.3.a[[4]], bs_p.3.a[[1]])
dep.5.a <- c(5, n.5.a, est_sp.5.a[[2]], est_sp.5.a[[3]], est_sp.5.a[[4]], bs_p.5.a[[1]])
dep.6.a <- c(6, n.6.a, est_sp.6.a[[2]], est_sp.6.a[[3]], est_sp.6.a[[4]], bs_p.6.a[[1]])
dep.7.a <- c(7, n.7.a, est_sp.7.a[[2]], est_sp.7.a[[3]], est_sp.7.a[[4]], bs_p.7.a[[1]])
dep.8.a <- c(8, n.8.a, est_sp.8.a[[2]], est_sp.8.a[[3]], est_sp.8.a[[4]], bs_p.8.a[[1]])
dep.10.a <- c(10, n.10.a, est_sp.10.a[[2]], est_sp.10.a[[3]], est_sp.10.a[[4]], bs_p.10.a[[1]])
dep.11.a <- c(11, n.11.a, est_sp.11.a[[2]], est_sp.11.a[[3]], est_sp.11.a[[4]], bs_p.11.a[[1]])
dep.12.a <- c(12, n.12.a, est_sp.12.a[[2]], est_sp.12.a[[3]], est_sp.12.a[[4]], bs_p.12.a[[1]])
dep.13.a <- c(13, n.13.a, est_sp.13.a[[2]], est_sp.13.a[[3]], est_sp.13.a[[4]], bs_p.13.a[[1]])
dep.14.a <- c(14, n.14.a, est_sp.14.a[[2]], est_sp.14.a[[3]], est_sp.14.a[[4]], bs_p.14.a[[1]])
dep.15.a <- c(15, n.15.a, est_sp.15.a[[2]], est_sp.15.a[[3]], est_sp.15.a[[4]], bs_p.15.a[[1]])
dep.17.a <- c(17, n.17.a, est_sp.17.a[[2]], est_sp.17.a[[3]], est_sp.17.a[[4]], bs_p.17.a[[1]])
dep.18.a <- c(18, n.18.a, est_sp.18.a[[2]], est_sp.18.a[[3]], est_sp.18.a[[4]], bs_p.18.a[[1]])
dep.20.a <- c(20, n.20.a, est_sp.20.a[[2]], est_sp.20.a[[3]], est_sp.20.a[[4]], bs_p.20.a[[1]])
dep.21.a <- c(21, n.21.a, est_sp.21.a[[2]], est_sp.21.a[[3]], est_sp.21.a[[4]], bs_p.21.a[[1]])
dep.22.a <- c(22, n.22.a, est_sp.22.a[[2]], est_sp.22.a[[3]], est_sp.22.a[[4]], bs_p.22.a[[1]])
dep.23.a <- c(23, n.23.a, est_sp.23.a[[2]], est_sp.23.a[[3]], est_sp.23.a[[4]], bs_p.23.a[[1]])
dep.24.a <- c(24, n.24.a, est_sp.24.a[[2]], est_sp.24.a[[3]], est_sp.24.a[[4]], bs_p.24.a[[1]])
dep.27.a <- c(27, n.27.a, est_sp.27.a[[2]], est_sp.27.a[[3]], est_sp.27.a[[4]], bs_p.27.a[[1]])
dep.28.a <- c(28, n.28.a, est_sp.28.a[[2]], est_sp.28.a[[3]], est_sp.28.a[[4]], bs_p.28.a[[1]])
dep.30.a <- c(30, n.30.a, est_sp.30.a[[2]], est_sp.30.a[[3]], est_sp.30.a[[4]], bs_p.30.a[[1]])
dep.31.a <- c(31, n.31.a, est_sp.31.a[[2]], est_sp.31.a[[3]], est_sp.31.a[[4]], bs_p.31.a[[1]])

departments.a <- as.data.frame(rbind(dep.2.a, dep.3.a, dep.5.a, dep.6.a, dep.7.a, dep.8.a,
                                     dep.10.a, dep.11.a, dep.12.a, dep.13.a,
                                     dep.14.a, dep.15.a, dep.17.a, dep.18.a, dep.20.a, 
                                     dep.21.a, dep.22.a, dep.23.a, dep.24.a, 
                                     dep.27.a, dep.28.a, dep.30.a, dep.31.a))

departments.a <- departments.a %>% rename(dep_id=V1,
                                          n=V2,
                                          xmin=V3,
                                          alpha=V4,
                                          ntail=V5,
                                          pKS=V6)
departments.a
write.csv(departments.a, "out/departments-results-1989-1999.csv")


dep.2.b <- c(2, n.2.b, est_sp.2.b[[2]], est_sp.2.b[[3]], est_sp.2.b[[4]], bs_p.2.b[[1]])
dep.3.b <- c(3, n.3.b, est_sp.3.b[[2]], est_sp.3.b[[3]], est_sp.3.b[[4]], bs_p.3.b[[1]])
dep.5.b <- c(5, n.5.b, est_sp.5.b[[2]], est_sp.5.b[[3]], est_sp.5.b[[4]], bs_p.5.b[[1]])
dep.6.b <- c(6, n.6.b, est_sp.6.b[[2]], est_sp.6.b[[3]], est_sp.6.b[[4]], bs_p.6.b[[1]])
dep.7.b <- c(7, n.7.b, est_sp.7.b[[2]], est_sp.7.b[[3]], est_sp.7.b[[4]], bs_p.7.b[[1]])
dep.8.b <- c(8, n.8.b, est_sp.8.b[[2]], est_sp.8.b[[3]], est_sp.8.b[[4]], bs_p.8.b[[1]])
dep.9.b <- c(9, n.9.b, est_sp.9.b[[2]], est_sp.9.b[[3]], est_sp.9.b[[4]], bs_p.9.b[[1]])
dep.10.b <- c(10, n.10.b, est_sp.10.b[[2]], est_sp.10.b[[3]], est_sp.10.b[[4]], bs_p.10.b[[1]])
dep.11.b <- c(11, n.11.b, est_sp.11.b[[2]], est_sp.11.b[[3]], est_sp.11.b[[4]], bs_p.11.b[[1]])
dep.12.b <- c(12, n.12.b, est_sp.12.b[[2]], est_sp.12.b[[3]], est_sp.12.b[[4]], bs_p.12.b[[1]])
dep.13.b <- c(13, n.13.b, est_sp.13.b[[2]], est_sp.13.b[[3]], est_sp.13.b[[4]], bs_p.13.b[[1]])
dep.14.b <- c(14, n.14.b, est_sp.14.b[[2]], est_sp.14.b[[3]], est_sp.14.b[[4]], bs_p.14.b[[1]])
dep.15.b <- c(15, n.15.b, est_sp.15.b[[2]], est_sp.15.b[[3]], est_sp.15.b[[4]], bs_p.15.b[[1]])
dep.17.b <- c(17, n.17.b, est_sp.17.b[[2]], est_sp.17.b[[3]], est_sp.17.b[[4]], bs_p.17.b[[1]])
dep.18.b <- c(18, n.18.b, est_sp.18.b[[2]], est_sp.18.b[[3]], est_sp.18.b[[4]], bs_p.18.b[[1]])
dep.19.b <- c(19, n.19.b, est_sp.19.b[[2]], est_sp.19.b[[3]], est_sp.19.b[[4]], bs_p.19.b[[1]])
dep.20.b <- c(20, n.20.b, est_sp.20.b[[2]], est_sp.20.b[[3]], est_sp.20.b[[4]], bs_p.20.b[[1]])
dep.21.b <- c(21, n.21.b, est_sp.21.b[[2]], est_sp.21.b[[3]], est_sp.21.b[[4]], bs_p.21.b[[1]])
dep.22.b <- c(22, n.22.b, est_sp.22.b[[2]], est_sp.22.b[[3]], est_sp.22.b[[4]], bs_p.22.b[[1]])
dep.23.b <- c(23, n.23.b, est_sp.23.b[[2]], est_sp.23.b[[3]], est_sp.23.b[[4]], bs_p.23.b[[1]])
dep.24.b <- c(24, n.24.b, est_sp.24.b[[2]], est_sp.24.b[[3]], est_sp.24.b[[4]], bs_p.24.b[[1]])
dep.25.b <- c(25, n.25.b, est_sp.25.b[[2]], est_sp.25.b[[3]], est_sp.25.b[[4]], bs_p.25.b[[1]])
dep.26.b <- c(26, n.26.b, est_sp.26.b[[2]], est_sp.26.b[[3]], est_sp.26.b[[4]], bs_p.26.b[[1]])
dep.27.b <- c(27, n.27.b, est_sp.27.b[[2]], est_sp.27.b[[3]], est_sp.27.b[[4]], bs_p.27.b[[1]])
dep.28.b <- c(28, n.28.b, est_sp.28.b[[2]], est_sp.28.b[[3]], est_sp.28.b[[4]], bs_p.28.b[[1]])
dep.30.b <- c(30, n.30.b, est_sp.30.b[[2]], est_sp.30.b[[3]], est_sp.30.b[[4]], bs_p.30.b[[1]])
dep.31.b <- c(31, n.31.b, est_sp.31.b[[2]], est_sp.31.b[[3]], est_sp.31.b[[4]], bs_p.31.b[[1]])


departments.b <- as.data.frame(rbind(dep.2.b, dep.3.b, dep.5.b, dep.6.b, dep.7.b, dep.8.b,
                                     dep.9.b, dep.10.b, dep.11.b, dep.12.b, dep.13.b,
                                     dep.14.b, dep.15.b, dep.17.b, dep.18.b, dep.19.b,
                                     dep.20.b, dep.21.b, dep.22.b, dep.23.b, dep.24.b, dep.25.b,
                                     dep.26.b, dep.27.b, dep.28.b, dep.30.b, dep.31.b))

departments.b <- departments.b %>% rename(dep_id=V1,
                                          n=V2,
                                          xmin=V3,
                                          alpha=V4,
                                          ntail=V5,
                                          pKS=V6)
departments.b
write.csv(departments.b, "out/departments-results-2000-2009.csv")


dep.2.c <- c(2, n.2.c, est_sp.2.c[[2]], est_sp.2.c[[3]], est_sp.2.c[[4]], bs_p.2.c[[1]])
dep.3.c <- c(3, n.3.c, est_sp.3.c[[2]], est_sp.3.c[[3]], est_sp.3.c[[4]], bs_p.3.c[[1]])
dep.10.c <- c(10, n.10.c, est_sp.10.c[[2]], est_sp.10.c[[3]], est_sp.10.c[[4]], bs_p.10.c[[1]])
dep.12.c <- c(12, n.12.c, est_sp.12.c[[2]], est_sp.12.c[[3]], est_sp.12.c[[4]], bs_p.12.c[[1]])
dep.14.c <- c(14, n.14.c, est_sp.14.c[[2]], est_sp.14.c[[3]], est_sp.14.c[[4]], bs_p.14.c[[1]])
dep.17.c <- c(17, n.17.c, est_sp.17.c[[2]], est_sp.17.c[[3]], est_sp.17.c[[4]], bs_p.17.c[[1]])
dep.18.c <- c(18, n.18.c, est_sp.18.c[[2]], est_sp.18.c[[3]], est_sp.18.c[[4]], bs_p.18.c[[1]])
dep.21.c <- c(21, n.21.c, est_sp.21.c[[2]], est_sp.21.c[[3]], est_sp.21.c[[4]], bs_p.21.c[[1]])
dep.22.c <- c(22, n.22.c, est_sp.22.c[[2]], est_sp.22.c[[3]], est_sp.22.c[[4]], bs_p.22.c[[1]])
dep.23.c <- c(23, n.23.c, est_sp.23.c[[2]], est_sp.23.c[[3]], est_sp.23.c[[4]], bs_p.23.c[[1]])


departments.c <- as.data.frame(rbind(dep.2.c, dep.3.c, dep.10.c, dep.12.c,
                                     dep.14.c, dep.17.c, dep.18.c,
                                     dep.21.c, dep.22.c, dep.23.c))

departments.c <- departments.c %>% rename(dep_id=V1,
                                          n=V2,
                                          xmin=V3,
                                          alpha=V4,
                                          ntail=V5,
                                          pKS=V6)
departments.c
write.csv(departments.c, "out/departments-results-2010-2018.csv")



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

departments.2 <- merge(x=col.adm1, y=departments, by="dep_id", all=T)
departments.2

departments.2.a <- merge(x=col.adm1, y=departments.a, by="dep_id", all=T)
departments.2.a

departments.2.b <- merge(x=col.adm1, y=departments.b, by="dep_id", all=T)
departments.2.b

departments.2.c <- merge(x=col.adm1, y=departments.c, by="dep_id", all=T)
departments.2.c



# Maps - alpha ------------------------------------------------------------

pdf(file="figs/department-1989-2018-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  ggtitle("Colombia departments, 1989-2018: Alpha") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()



departments.2.a.sel <- departments.2.a %>% filter(pKS < 0.1)

pdf(file="figs/department-1989-1999-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2.a) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  geom_sf(data = departments.2.a.sel, alpha = 0.99,  fill = "grey") +
  ggtitle("Colombia departments, 1989-1999") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()


departments.2.b.sel <- departments.2.b %>% filter(pKS < 0.1)

pdf(file="figs/department-2000-2009-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2.b) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  geom_sf(data = departments.2.b.sel, alpha = 0.99,  fill = "grey") +
  ggtitle("Colombia departments, 2000-2009") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()



departments.2.c.sel <- departments.2.c %>% filter(pKS < 0.1)

pdf(file="figs/department-2010-2018-alpha.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2.c) + 
  geom_sf(aes(fill=alpha)) + 
  scale_fill_gradientn(colours=brewer.pal(6, "Spectral"),
                       na.value = "transparent",
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       labels=c(1.5, 2.5, 3.5, 4.5, 5.5),
                       limits=c(1.5,5.5)) +
  geom_sf(data = departments.2.c.sel, alpha = 0.99,  fill = "grey") +
  ggtitle("Colombia departments, 2010-2018") + 
  labs(fill = "Alpha") +
  theme_bw() +
  coord_sf()
dev.off()



# Maps - P-values for the Kolmogorov-Smirnov goodness of fit test ---------

departments.2 <- departments.2 %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/departments-1989-2018-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia departments, 1989-2018: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()


departments.2.a <- departments.2.a %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/departments-1989-1999-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia departments, 1989-1999: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()


departments.2.b <- departments.2.b %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/departments-2000-2009-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia departments, 2000-2009: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()

departments.2.c <- departments.2.c %>% mutate(pKS.cat = ifelse(pKS >= 0.1, "P >= 0.1", "P < 0.1"))

pdf(file="figs/departments-2010-2018-p-values.pdf", 
    width=4.5, height=4.5)
ggplot(data = departments.2) + 
  geom_sf(aes(fill=pKS.cat)) + 
  scale_fill_manual(values = c("gold", "#4682B4")) +
  ggtitle("Colombia departments, 2010-2018: P-value") + 
  labs(fill = "P-value") +
  theme_bw() +
  coord_sf()
dev.off()

# Representative CCDF -----------------------------------------------------

# 1989-1999: Choco department
pdf(file="figs/ccdf-department14-1989-1999.pdf")
plot(m_sp.14.a, pch=1, bg=1, panel.first=grid(col="grey80"),
     xlab="x", ylab="P(x)")
title(main="Choco department: 1989-1999")
lines(m_sp.14.a, col=2, lwd=1)
text(20, 0.4, paste("alpha =", round(est_sp.14.a$pars,2)), col = "red")
text(20, 0.6, paste("p(KS) =", round(bs_p.14.a$p,2)), srt=0.2, col = "grey40")
dev.off()

# 2000-2009: Cesar department
pdf(file="figs/ccdf-department13-2000-2009.pdf")
plot(m_sp.13.b, pch=1, bg=1, panel.first=grid(col="grey80"),
     xlab="x", ylab="P(x)")
title(main="Cesar department: 2000-2009")
lines(m_sp.13.b, col=2, lwd=1)
text(10, 0.2, paste("alpha =", round(est_sp.13.b$pars,2)), col = "red")
text(10, 0.5, paste("p(KS) =", round(bs_p.13.b$p,2)), srt=0.2, col = "grey40")
dev.off()

# 2010-2018: Norte de Santander department
pdf(file="figs/ccdf-department23-2010-2018.pdf")
plot(m_sp.23.c, pch=1, bg=1, panel.first=grid(col="grey80"),
     xlab="x", ylab="P(x)")
title(main="Norte de Santander department: 2010-2018")
lines(m_sp.23.c, col=2, lwd=1)
text(1.5, 0.25, paste("alpha =", round(est_sp.23.c$pars,2)), col = "red")
text(1.5, 0.5, paste("p(KS) =", round(bs_p.23.c$p,2)), srt=0.2, col = "grey40")
dev.off()

