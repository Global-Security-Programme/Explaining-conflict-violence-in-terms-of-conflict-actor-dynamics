### Preamble ###############################################################
# Comparing random sampling with the results - Departments
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 18 October

rm(list= ls())


library(furniture)
library(flextable)
library(poweRlaw)
library(ggplot2)
library(tidyverse)



# Data 1989-1999 --------------------------------------------------------------------

results.a <- read.csv("out/subsamples-results-departments-1989-1999.csv")
trans.a <- read.csv("out/translation-table-departments-1989-1999.csv")
departments.a <- read.csv("out/departments-results-1989-1999.csv")

results.a <- results.a %>% rename(ID_sample = subsample)

name_sample.a <- c("Antioquia department", "Arauca department", "Bogotá department",
                   "Bolívar department", "Boyacá department", "Córdoba department",
                   "Caquetá department", "Casanare department", "Cauca department",
                   "Cesar department", "Chocó department", "Cundinamarca department",
                   "Guaviare department", "Huila department", "Magdalena department",
                   "Meta department", "Nariño department", "Norte de Santander department",
                   "Putumayo department", "Santander department", "Sucre department",
                   "Tolima  department", "Valle del Cauca  department")


trans.a$name_sample <- rep(name_sample.a, 50)

data.a <- merge(results.a, trans.a)

departments.a$department <- name_sample.a


ged <- readRDS("data/colombia.rds")
ged <- ged %>% filter(where_prec <= 4)

ged.a <- ged %>% filter(year <= 1999)


# Comparison 1989-1999 --------------------------------------------------------------

# all

ggplot(data.a, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.a, aes(x = department, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "1989-1999", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-departments-1989-1999.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.a.pval <- data.a %>% filter(p >= 0.1)

ggplot(data.a.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.a, aes(x = department, y = alpha), 
             shape = 8, color = ifelse(departments.a$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "1989-1999", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-departments-1989-1999.pdf", width = 6, height = 4, dpi = 300)


data.a.pval.comp <- data.a.pval %>% 
  group_by(name_sample) %>% 
  mutate(N = n()) %>%
  mutate(alpha_mean = mean(alpha)) %>% 
  mutate(alpha_sd = round(sd(alpha), 2)) %>% 
  mutate(alpha_q25 = round(quantile(alpha, probs = 0.25), 2)) %>% 
  mutate(alpha_q75 = round(quantile(alpha, probs = 0.75), 2)) %>% 
  ungroup() %>% 
  distinct(name_sample, .keep_all = T) %>% 
  select(name_sample, N, alpha_mean, alpha_sd, alpha_q25, alpha_q75) %>% 
  rename(department = name_sample) %>% 
  left_join(departments.a) %>% 
  select(-c(n, xmin, ntail, X, dep_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

data.a.pval.comp

write.csv(data.a.pval.comp, "out/comparison-pval-departments-1989-1999-tab.csv")



# Data 2000-2009 --------------------------------------------------------------------

results.b <- read.csv("out/subsamples-results-departments-2000-2009.csv")
trans.b <- read.csv("out/translation-table-departments-2000-2009.csv")
departments.b <- read.csv("out/departments-results-2000-2009.csv")

results.b <- results.b %>% rename(ID_sample = subsample)

name_sample.b <- c("Antioquia department", "Arauca department", "Bogotá department",
                   "Bolívar department", "Boyacá department", "Córdoba department", 
                   "Caldas department", "Caquetá department", "Casanare department", 
                   "Cauca department", "Cesar department", "Chocó department", 
                   "Cundinamarca department", "Guaviare department", "Huila department", 
                   "La Guajira department", "Magdalena department", "Meta department", 
                   "Nariño department", "Norte de Santander department", "Putumayo department", 
                   "Quindío department", "Risaralda department", "Santander department", 
                   "Sucre department", "Tolima  department", "Valle del Cauca  department")


trans.b$name_sample <- rep(name_sample.b, 50)

data.b <- merge(results.b, trans.b)

departments.b$department <- name_sample.b


ged.b <- ged %>% filter(year >= 2000 & year <= 2009)


# Comparison 2000-2009 --------------------------------------------------------------

# all

ggplot(data.b, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.b, aes(x = department, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2000-2009", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-departments-2000-2009.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.b.pval <- data.b %>% filter(p >= 0.1)

ggplot(data.b.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.b, aes(x = department, y = alpha), 
             shape = 8, color = ifelse(departments.b$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2000-2009", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-departments-2000-2009.pdf", width = 6, height = 4, dpi = 300)


data.b.pval.comp <- data.b.pval %>% 
  group_by(name_sample) %>% 
  mutate(N = n()) %>%
  mutate(alpha_mean = mean(alpha)) %>% 
  mutate(alpha_sd = round(sd(alpha), 2)) %>% 
  mutate(alpha_q25 = round(quantile(alpha, probs = 0.25), 2)) %>% 
  mutate(alpha_q75 = round(quantile(alpha, probs = 0.75), 2)) %>% 
  ungroup() %>% 
  distinct(name_sample, .keep_all = T) %>% 
  select(name_sample, N, alpha_mean, alpha_sd, alpha_q25, alpha_q75) %>% 
  rename(department = name_sample) %>% 
  left_join(departments.b) %>% 
  select(-c(N, xmin, ntail, X, dep_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

data.b.pval.comp

write.csv(data.b.pval.comp, "out/comparison-pval-departments-2000-2009-tab.csv")



# Data 2010-2018 --------------------------------------------------------------------

results.c <- read.csv("out/subsamples-results-departments-2010-2018.csv")
trans.c <- read.csv("out/translation-table-departments-2010-2018.csv")
departments.c <- read.csv("out/departments-results-2010-2018.csv")

results.c <- results.c %>% rename(ID_sample = subsample)

name_sample.c <- c("Antioquia department", "Arauca department", "Caquetá department",
                   "Cauca department", "Chocó department", "Guaviare department", 
                   "Huila department", "Meta department", "Nariño department", 
                   "Norte de Santander department")

trans.c$name_sample <- rep(name_sample.c, 50)

data.c <- merge(results.c, trans.c)

departments.c$department <- name_sample.c


ged.c <- ged %>% filter(year > 2009)


# Comparison 2010-2018 --------------------------------------------------------------

# all

ggplot(data.c, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.c, aes(x = department, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2010-2018", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-departments-2010-2018.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.c.pval <- data.c %>% filter(p >= 0.1)

ggplot(data.c.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = departments.c, aes(x = department, y = alpha), 
             shape = 8, color = ifelse(departments.c$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2010-2018", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-departments-2010-2018.pdf", width = 6, height = 4, dpi = 300)


data.c.pval.comp <- data.c.pval %>% 
  group_by(name_sample) %>% 
  mutate(N = n()) %>%
  mutate(alpha_mean = mean(alpha)) %>% 
  mutate(alpha_sd = round(sd(alpha), 2)) %>% 
  mutate(alpha_q25 = round(quantile(alpha, probs = 0.25), 2)) %>% 
  mutate(alpha_q75 = round(quantile(alpha, probs = 0.75), 2)) %>% 
  ungroup() %>% 
  distinct(name_sample, .keep_all = T) %>% 
  select(name_sample, N, alpha_mean, alpha_sd, alpha_q25, alpha_q75) %>% 
  rename(department = name_sample) %>% 
  left_join(departments.b) %>% 
  select(-c(N, xmin, ntail, X, dep_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

data.c.pval.comp

write.csv(data.c.pval.comp, "out/comparison-pval-departments-2010-2018-tab.csv")

