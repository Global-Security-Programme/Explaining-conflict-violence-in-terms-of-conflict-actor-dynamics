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

region.1.a <- data.a %>% filter(dep_id == 19 |
                              dep_id == 13 |
                              dep_id == 20 |
                              dep_id == 4 |
                              dep_id == 6 |
                              dep_id == 28)

n.1.a <- length(region.1.a$deaths_total)

region.2.a <- data.a %>% filter(dep_id == 8|
                              dep_id == 2 |
                              dep_id == 14)

n.2.a <- length(region.2.a$deaths_total)

region.3.a <- data.a %>% filter(dep_id == 23|
                              dep_id == 27)   

n.3.a <- length(region.3.a$deaths_total)

region.4.a <- data.a %>% filter(dep_id == 22|
                              dep_id == 12 |
                              dep_id == 31 |
                              dep_id == 26 |
                              dep_id == 9 |
                              dep_id == 25)

n.4.a <- length(region.4.a$deaths_total)

region.5.a <- data.a %>% filter(dep_id == 15|
                              dep_id == 7 |
                              dep_id == 30 |
                              dep_id == 18 |
                              dep_id == 5)

n.5.a <- length(region.5.a$deaths_total)

region.6.a <- data.a %>% filter(dep_id == 10|
                              dep_id == 24 |
                              dep_id == 1)

n.6.a <- length(region.6.a$deaths_total)

region.7.a <- data.a %>% filter(dep_id == 3|
                              dep_id == 11 |
                              dep_id == 21 |
                              dep_id == 33 |
                              dep_id == 16 |
                              dep_id == 17 |
                              dep_id == 32)

n.7.a <- length(region.7.a$deaths_total)

sizes.a <- c(n.1.a, n.2.a, n.3.a, n.4.a, n.5.a, n.6.a, n.7.a)


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
write.csv(trans_table, "out/translation-table-regions-1989-1999.csv", row.names = F)



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

write.csv(results, "out/subsamples-results-regions-1989-1999.csv", row.names = F)
save.image(file = "out/subsamples-results-regions-1989-1999.RData")



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

region.1.b <- data.b %>% filter(dep_id == 19 |
                                  dep_id == 13 |
                                  dep_id == 20 |
                                  dep_id == 4 |
                                  dep_id == 6 |
                                  dep_id == 28)

n.1.b <- length(region.1.b$deaths_total)

region.2.b <- data.b %>% filter(dep_id == 8|
                                  dep_id == 2 |
                                  dep_id == 14)

n.2.b <- length(region.2.b$deaths_total)

region.3.b <- data.b %>% filter(dep_id == 23|
                                  dep_id == 27)   

n.3.b <- length(region.3.b$deaths_total)

region.4.b <- data.b %>% filter(dep_id == 22|
                                  dep_id == 12 |
                                  dep_id == 31 |
                                  dep_id == 26 |
                                  dep_id == 9 |
                                  dep_id == 25)

n.4.b <- length(region.4.b$deaths_total)

region.5.b <- data.b %>% filter(dep_id == 15|
                                  dep_id == 7 |
                                  dep_id == 30 |
                                  dep_id == 18 |
                                  dep_id == 5)

n.5.b <- length(region.5.b$deaths_total)

region.6.b <- data.b %>% filter(dep_id == 10|
                                  dep_id == 24 |
                                  dep_id == 1)

n.6.b <- length(region.6.b$deaths_total)

region.7.b <- data.b %>% filter(dep_id == 3|
                                  dep_id == 11 |
                                  dep_id == 21 |
                                  dep_id == 33 |
                                  dep_id == 16 |
                                  dep_id == 17 |
                                  dep_id == 32)

n.7.b <- length(region.7.b$deaths_total)

sizes.b <- c(n.1.b, n.2.b, n.3.b, n.4.b, n.5.b, n.6.b, n.7.b)


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
write.csv(trans_table, "out/translation-table-regions-2000-2009.csv", row.names = F)



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

write.csv(results, "out/subsamples-results-regions-2000-2009.csv", row.names = F)
save.image(file = "out/subsamples-results-regions-2000-2009.RData")



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

data.c <- data %>% filter(year > 2010)

region.1.c <- data.c %>% filter(dep_id == 19 |
                                  dep_id == 13 |
                                  dep_id == 20 |
                                  dep_id == 4 |
                                  dep_id == 6 |
                                  dep_id == 28)

n.1.c <- length(region.1.c$deaths_total)

region.2.c <- data.c %>% filter(dep_id == 8|
                                  dep_id == 2 |
                                  dep_id == 14)

n.2.c <- length(region.2.c$deaths_total)

region.3.c <- data.c %>% filter(dep_id == 23|
                                  dep_id == 27)   

n.3.c <- length(region.3.c$deaths_total)

region.4.c <- data.c %>% filter(dep_id == 22|
                                  dep_id == 12 |
                                  dep_id == 31 |
                                  dep_id == 26 |
                                  dep_id == 9 |
                                  dep_id == 25)

n.4.c <- length(region.4.c$deaths_total)

region.5.c <- data.c %>% filter(dep_id == 15|
                                  dep_id == 7 |
                                  dep_id == 30 |
                                  dep_id == 18 |
                                  dep_id == 5)

n.5.c <- length(region.5.c$deaths_total)

region.6.c <- data.c %>% filter(dep_id == 10|
                                  dep_id == 24 |
                                  dep_id == 1)

n.6.c <- length(region.6.c$deaths_total)

region.7.c <- data.c %>% filter(dep_id == 3|
                                  dep_id == 11 |
                                  dep_id == 21 |
                                  dep_id == 33 |
                                  dep_id == 16 |
                                  dep_id == 17 |
                                  dep_id == 32)

n.7.c <- length(region.7.c$deaths_total)

sizes.c <- c(n.1.c, n.2.c, n.3.c, n.4.c, n.5.c, n.6.c, n.7.c)


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
write.csv(trans_table, "out/translation-table-regions-2010-2018.csv", row.names = F)



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

write.csv(results, "out/subsamples-results-regions-2010-2018.csv", row.names = F)
save.image(file = "out/subsamples-results-regions-2010-2018.RData")



