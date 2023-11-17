### Preamble ###############################################################
# Fitting data to the power-law distribution: Random sampling - regions
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 18 October

library(poweRlaw)
library(tidyverse)


# Data preparation A: 1989-1999 -------------------------------------------

rm(list= ls())

data <- readRDS("data/colombia.rds")
data <- data %>% filter(where_prec <= 4)
data <- data %>% select(id, year, deaths_total, adm_1)

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

data.a <- data %>% filter(year <= 1999)


antioquia.2.a <- data.a %>% filter(dep_id == 2)
n.2.a <- length(antioquia.2.a$deaths_total)

arauca.3.a <- data.a %>% filter(dep_id == 3)
n.3.a <- length(arauca.3.a$deaths_total)

bogota.5.a <- data.a %>% filter(dep_id == 5)
n.5.a <- length(bogota.5.a$deaths_total)

bolivar.6.a <- data.a %>% filter(dep_id == 6)
n.6.a <- length(bolivar.6.a$deaths_total)

boyaca.7.a <- data.a %>% filter(dep_id == 7)
n.7.a <- length(boyaca.7.a$deaths_total)

cordoba.8.a <- data.a %>% filter(dep_id == 8)
n.8.a <- length(cordoba.8.a$deaths_total)

caqueta.10.a <- data.a %>% filter(dep_id == 10)
n.10.a <- length(caqueta.10.a$deaths_total)

casanare.11.a <- data.a %>% filter(dep_id == 11)
n.11.a <- length(casanare.11.a$deaths_total)

cauca.12.a <- data.a %>% filter(dep_id == 12)
n.12.a <- length(cauca.12.a$deaths_total)

cesar.13.a <- data.a %>% filter(dep_id == 13)
n.13.a <- length(cesar.13.a$deaths_total)

choco.14.a <- data.a %>% filter(dep_id == 14)
n.14.a <- length(choco.14.a$deaths_total)

cundinamarca.15.a <- data.a %>% filter(dep_id == 15)
n.15.a <- length(cundinamarca.15.a$deaths_total)

guaviare.17.a <- data.a %>% filter(dep_id == 17)
n.17.a <- length(guaviare.17.a$deaths_total)

huila.18.a <- data.a %>% filter(dep_id == 18)
n.18.a <- length(huila.18.a$deaths_total)

magdalena.20.a <- data.a %>% filter(dep_id == 20)
n.20.a <- length(magdalena.20.a$deaths_total)

meta.21.a <- data.a %>% filter(dep_id == 21)
n.21.a <- length(meta.21.a$deaths_total)

narino.22.a <- data.a %>% filter(dep_id == 22)
n.22.a <- length(narino.22.a$deaths_total)

nortesantander.23.a <- data.a %>% filter(dep_id == 23)
n.23.a <- length(nortesantander.23.a$deaths_total)

putumayo.24.a <- data.a %>% filter(dep_id == 24)
n.24.a <- length(putumayo.24.a$deaths_total)

santander.27.a <- data.a %>% filter(dep_id == 27)
n.27.a <- length(santander.27.a$deaths_total)

sucre.28.a <- data.a %>% filter(dep_id == 28)
n.28.a <- length(sucre.28.a$deaths_total)

tolima.30.a <- data.a %>% filter(dep_id == 30)
n.30.a <- length(tolima.30.a$deaths_total)

valledelcauca.31.a <- data.a %>% filter(dep_id == 31)
n.31.a <- length(valledelcauca.31.a$deaths_total)


sizes.a <- c(n.2.a, n.3.a, n.5.a, n.6.a, n.6.a, n.8.a, n.10.a, n.11.a, n.12.a,
             n.13.a, n.14.a, n.15.a, n.17.a, n.18.a, n.20.a, n.21.a, n.22.a,
             n.23.a, n.24.a, n.27.a, n.28.a, n.30.a, n.31.a)



# Sub-sampling A: 1989-1999 -----------------------------------------------

set.seed(18)

create_subsample <- function(i, data.a) {
  sample_data <- data[sample(nrow(data.a), i, replace = TRUE), ]
  sample_data$identifier <- paste("Sample", i)
  return(sample_data)
}

num_replications <- 50

subsample_results <- list()

for (replication in 1:num_replications) {
  results <- lapply(sizes.a, function(i) create_subsample(i, data))
  subsample_results[[replication]] <- results
}

print(subsample_results)

combined_df <- bind_rows(subsample_results, .id = "ID_sample")

print(combined_df)

trans_table <- combined_df %>% 
  select(ID_sample, identifier) %>% 
  distinct(ID_sample, identifier)
  
print(trans_table)
write.csv(trans_table, "out/translation-table-departments-1989-1999.csv", row.names = F)



# Fitting to power-law A: 1989-1999 ---------------------------------------

matrix.data <- split(combined_df$deaths_total, combined_df$ID_sample) 
rm(list=setdiff(ls(), "matrix.data"))

subsample <- names(matrix.data)
l <- as.vector(do.call(rbind, lapply(matrix.data, length)))
N <- length(matrix.data)
M <- list()
B <- list()

xmin <- vector(mode = "numeric", length=0)
alpha <- vector(mode = "numeric", length=0)
ntail <- vector(mode = "numeric", length=0)
p <- vector(mode = "numeric", length=0)

for(i in 1:N){
  print(i)
  
  powerl <- displ$new(matrix.data[[i]]) 
  
  bootsr <- bootstrap_p(powerl, no_of_sims = 1000, xmins = seq(1, 30, 1), threads = 2, seed = 18)
  
  M[[i]]<-powerl
  B[[i]]<-bootsr
  
  alpha[i]<-mean(bootsr$bootstraps$pars,na.rm=T)
  xmin[i]<-mean(bootsr$bootstraps$xmin,na.rm=T)
  ntail[i]<-mean(bootsr$bootstraps$ntail,na.rm=T)
  p[i]<-bootsr$p
  
}

results <- data.frame(subsample = subsample, 
                      n_total = l, 
                      alpha = alpha, 
                      xmin = xmin, 
                      p = p, 
                      n_tail = ntail)

write.csv(results, "out/subsamples-results-departments-1989-1999.csv", row.names = F)
save.image(file = "out/subsamples-results-departments-1989-1999.RData")



# Data preparation B: 2000-2009 -------------------------------------------

rm(list= ls())

data <- readRDS("data/colombia.rds")
data <- data %>% filter(where_prec <= 4)
data <- data %>% select(id, year, deaths_total, adm_1)

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

data.b <- data %>% filter(year >= 2000 & year <= 2009)


antioquia.2.b <- data.b %>% filter(dep_id == 2)
n.2.b <- length(antioquia.2.b$deaths_total)

arauca.3.b <- data.b %>% filter(dep_id == 3)
n.3.b <- length(arauca.3.b$deaths_total)

bogota.5.b <- data.b %>% filter(dep_id == 5)
n.5.b <- length(bogota.5.b$deaths_total)

bolivar.6.b <- data.b %>% filter(dep_id == 6)
n.6.b <- length(bolivar.6.b$deaths_total)

boyaca.7.b <- data.b %>% filter(dep_id == 7)
n.7.b <- length(boyaca.7.b$deaths_total)

cordoba.8.b <- data.b %>% filter(dep_id == 8)
n.8.b <- length(cordoba.8.b$deaths_total)

caldas.9.b <- data.b %>% filter(dep_id == 9)
n.9.b <- length(caldas.9.b$deaths_total)

caqueta.10.b <- data.b %>% filter(dep_id == 10)
n.10.b <- length(caqueta.10.b$deaths_total)

casanare.11.b <- data.b %>% filter(dep_id == 11)
n.11.b <- length(casanare.11.b$deaths_total)

cauca.12.b <- data.b %>% filter(dep_id == 12)
n.12.b <- length(cauca.12.b$deaths_total)

cesar.13.b <- data.b %>% filter(dep_id == 13)
n.13.b <- length(cesar.13.b$deaths_total)

choco.14.b <- data.b %>% filter(dep_id == 14)
n.14.b <- length(choco.14.b$deaths_total)

cundinamarca.15.b <- data.b %>% filter(dep_id == 15)
n.15.b <- length(cundinamarca.15.b$deaths_total)

guaviare.17.b <- data.b %>% filter(dep_id == 17)
n.17.b <- length(guaviare.17.b$deaths_total)

huila.18.b <- data.b %>% filter(dep_id == 18)
n.18.b <- length(huila.18.b$deaths_total)

laguajira.19.b <- data.b %>% filter(dep_id == 19)
n.19.b <- length(laguajira.19.b$deaths_total)

magdalena.20.b <- data.b %>% filter(dep_id == 20)
n.20.b <- length(magdalena.20.b$deaths_total)

meta.21.b <- data.b %>% filter(dep_id == 21)
n.21.b <- length(meta.21.b$deaths_total)

narino.22.b <- data.b %>% filter(dep_id == 22)
n.22.b <- length(narino.22.b$deaths_total)

nortesantander.23.b <- data.b %>% filter(dep_id == 23)
n.23.b <- length(nortesantander.23.b$deaths_total)

putumayo.24.b <- data.b %>% filter(dep_id == 24)
n.24.b <- length(putumayo.24.b$deaths_total)

quindio.25.b <- data.b %>% filter(dep_id == 25)
n.25.b <- length(quindio.25.b$deaths_total)

risalda.26.b <- data.b %>% filter(dep_id == 26)
n.26.b <- length(risalda.26.b$deaths_total)

santander.27.b <- data.b %>% filter(dep_id == 27)
n.27.b <- length(santander.27.b$deaths_total)

sucre.28.b <- data.b %>% filter(dep_id == 28)
n.28.b <- length(sucre.28.b$deaths_total)

tolima.30.b <- data.b %>% filter(dep_id == 30)
n.30.b <- length(tolima.30.b$deaths_total)

valledelcauca.31.b <- data.b %>% filter(dep_id == 31)
n.31.b <- length(valledelcauca.31.b$deaths_total)


sizes.b <- c(n.2.b, n.3.b, n.5.b, n.6.b, n.7.b, n.8.b, n.9.b, n.10.b, n.11.b, n.12.b,
             n.13.b, n.14.b, n.15.b, n.17.b, n.18.b, n.19.b, n.20.b, n.21.b, n.22.b,
             n.23.b, n.24.b, n.25.b, n.26.b, n.27.b, n.28.b, n.30.b, n.31.b)


# Sub-sampling B: 2000-2009 -----------------------------------------------

set.seed(18)

create_subsample <- function(i, data.b) {
  sample_data <- data[sample(nrow(data.b), i, replace = TRUE), ]
  sample_data$identifier <- paste("Sample", i)
  return(sample_data)
}

num_replications <- 50

subsample_results <- list()

for (replication in 1:num_replications) {
  results <- lapply(sizes.b, function(i) create_subsample(i, data))
  subsample_results[[replication]] <- results
}

print(subsample_results)

combined_df <- bind_rows(subsample_results, .id = "ID_sample")

print(combined_df)

trans_table <- combined_df %>% 
  select(ID_sample, identifier) %>% 
  distinct(ID_sample, identifier)

print(trans_table)
write.csv(trans_table, "out/translation-table-departments-2000-2009.csv", row.names = F)



# Fitting to power-law B: 2000-2009 ---------------------------------------

matrix.data <- split(combined_df$deaths_total, combined_df$ID_sample) 
rm(list=setdiff(ls(), "matrix.data"))

subsample <- names(matrix.data)
l <- as.vector(do.call(rbind, lapply(matrix.data, length)))
N <- length(matrix.data)
M <- list()
B <- list()

xmin <- vector(mode = "numeric", length=0)
alpha <- vector(mode = "numeric", length=0)
ntail <- vector(mode = "numeric", length=0)
p <- vector(mode = "numeric", length=0)

for(i in 1:N){
  print(i)
  
  powerl <- displ$new(matrix.data[[i]]) 
  
  bootsr <- bootstrap_p(powerl, no_of_sims = 1000, xmins = seq(1, 30, 1), threads = 2, seed = 18)
  
  M[[i]]<-powerl
  B[[i]]<-bootsr
  
  alpha[i]<-mean(bootsr$bootstraps$pars,na.rm=T)
  xmin[i]<-mean(bootsr$bootstraps$xmin,na.rm=T)
  ntail[i]<-mean(bootsr$bootstraps$ntail,na.rm=T)
  p[i]<-bootsr$p
  
}

results <- data.frame(subsample = subsample, 
                      n_total = l, 
                      alpha = alpha, 
                      xmin = xmin, 
                      p = p, 
                      n_tail = ntail)

write.csv(results, "out/subsamples-results-departments-2000-2009.csv", row.names = F)
save.image(file = "out/subsamples-results-departments-2000-2009.RData")



# Data preparation C: 2010-2018 -------------------------------------------

rm(list= ls())

data <- readRDS("data/colombia.rds")
data <- data %>% filter(where_prec <= 4)
data <- data %>% select(id, year, deaths_total, adm_1)

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

data.c <- data %>% filter(year > 2009)


antioquia.2.c <- data.c %>% filter(dep_id == 2)
n.2.c <- length(antioquia.2.c$deaths_total)

arauca.3.c <- data.c %>% filter(dep_id == 3)
n.3.c <- length(arauca.3.c$deaths_total)

caqueta.10.c <- data.c %>% filter(dep_id == 10)
n.10.c <- length(caqueta.10.c$deaths_total)

cauca.12.c <- data.c %>% filter(dep_id == 12)
n.12.c <- length(cauca.12.c$deaths_total)

choco.14.c <- data.c %>% filter(dep_id == 14)
n.14.c <- length(choco.14.c$deaths_total)

guaviare.17.c <- data.c %>% filter(dep_id == 17)
n.17.c <- length(guaviare.17.c$deaths_total)

huila.18.c <- data.c %>% filter(dep_id == 18)
n.18.c <- length(huila.18.c$deaths_total)

meta.21.c <- data.c %>% filter(dep_id == 21)
n.21.c <- length(meta.21.c$deaths_total)

narino.22.c <- data.c %>% filter(dep_id == 22)
n.22.c <- length(narino.22.c$deaths_total)

nortesantander.23.c <- data.c %>% filter(dep_id == 23)
n.23.c <- length(nortesantander.23.c$deaths_total)

sizes.c <- c(n.2.c, n.3.c, n.10.c, n.12.c, n.14.c, n.17.c, n.18.c, 
             n.21.c, n.22.c, n.23.c)

# Sub-sampling C: 2010-2018 -----------------------------------------------

set.seed(18)

create_subsample <- function(i, data.c) {
  sample_data <- data[sample(nrow(data.c), i, replace = TRUE), ]
  sample_data$identifier <- paste("Sample", i)
  return(sample_data)
}

num_replications <- 50

subsample_results <- list()

for (replication in 1:num_replications) {
  results <- lapply(sizes.c, function(i) create_subsample(i, data))
  subsample_results[[replication]] <- results
}

print(subsample_results)

combined_df <- bind_rows(subsample_results, .id = "ID_sample")

print(combined_df)

trans_table <- combined_df %>% 
  select(ID_sample, identifier) %>% 
  distinct(ID_sample, identifier)

print(trans_table)
write.csv(trans_table, "out/translation-table-departments-2010-2018.csv", row.names = F)



# Fitting to power-law C: 2010-2018 ---------------------------------------

matrix.data <- split(combined_df$deaths_total, combined_df$ID_sample) 
rm(list=setdiff(ls(), "matrix.data"))

subsample <- names(matrix.data)
l <- as.vector(do.call(rbind, lapply(matrix.data, length)))
N <- length(matrix.data)
M <- list()
B <- list()

xmin <- vector(mode = "numeric", length=0)
alpha <- vector(mode = "numeric", length=0)
ntail <- vector(mode = "numeric", length=0)
p <- vector(mode = "numeric", length=0)

for(i in 1:N){
  print(i)
  
  powerl <- displ$new(matrix.data[[i]]) 
  
  bootsr <- bootstrap_p(powerl, no_of_sims = 1000, xmins = seq(1, 30, 1), threads = 2, seed = 18)
  
  M[[i]]<-powerl
  B[[i]]<-bootsr
  
  alpha[i]<-mean(bootsr$bootstraps$pars,na.rm=T)
  xmin[i]<-mean(bootsr$bootstraps$xmin,na.rm=T)
  ntail[i]<-mean(bootsr$bootstraps$ntail,na.rm=T)
  p[i]<-bootsr$p
  
}

results <- data.frame(subsample = subsample, 
                      n_total = l, 
                      alpha = alpha, 
                      xmin = xmin, 
                      p = p, 
                      n_tail = ntail)

write.csv(results, "out/subsamples-results-departments-2010-2018.csv", row.names = F)
save.image(file = "out/subsamples-results-departments-2010-2018.RData")



