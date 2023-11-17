### Preamble ###############################################################
# Comparing random sampling with the results - regions
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

results.a <- read.csv("out/subsamples-results-regions-1989-1999.csv")
trans.a <- read.csv("out/translation-table-regions-1989-1999.csv")
regions.a <- read.csv("out/regions-results-1989-1999.csv")

results.a <- results.a %>% rename(ID_sample = subsample)

name_sample <- c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", 
                 "Region 6", "Region 7")

trans.a$name_sample <- rep(name_sample, 50)

data.a <- merge(results.a, trans.a)


ged <- readRDS("data/colombia.rds")
ged <- ged %>% filter(where_prec <= 4)

ged.a <- ged %>% filter(year <= 1999)



# Comparison 1989-1999 --------------------------------------------------------------

# all

ggplot(data.a, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.a, aes(x = region, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "1989-1999", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-regions-1989-1999.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.a.pval <- data.a %>% filter(p >= 0.1)

ggplot(data.a.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.a, aes(x = region, y = alpha), 
             shape = 8, color = ifelse(regions.a$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "1989-1999", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-regions-1989-1999.pdf", width = 6, height = 4, dpi = 300)


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
  rename(region = name_sample) %>% 
  left_join(regions.a) %>% 
  select(-c(n, xmin, ntail, X, region_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

write.csv(data.a.pval.comp, "out/comparison-pval-regions-1989-1999-tab.csv")


# Data 2000-2009 --------------------------------------------------------------------

results.b <- read.csv("out/subsamples-results-regions-2000-2009.csv")
trans.b <- read.csv("out/translation-table-regions-2000-2009.csv")
regions.b <- read.csv("out/regions-results-2000-2009.csv")

results.b <- results.b %>% rename(ID_sample = subsample)

name_sample <- c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", 
                 "Region 6", "Region 7")

trans.b$name_sample <- rep(name_sample, 50)

data.b <- merge(results.b, trans.b)

ged.b <- ged %>% filter(year >= 2000 & year <= 2009)


# Comparison 2000-2009 --------------------------------------------------------------

# all

ggplot(data.b, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.b, aes(x = region, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2000-2009", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-regions-2000-2009.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.b.pval <- data.b %>% filter(p >= 0.1)

ggplot(data.b.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.b, aes(x = region, y = alpha), 
             shape = 8, color = ifelse(regions.b$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2000-2009", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-regions-2000-2009.pdf", width = 6, height = 4, dpi = 300)


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
  rename(region = name_sample) %>% 
  left_join(regions.b) %>% 
  select(-c(n, xmin, ntail, X, region_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

write.csv(data.b.pval.comp, "out/comparison-pval-regions-2000-2009-tab.csv")



# Data 2010-2018 --------------------------------------------------------------------

results.c <- read.csv("out/subsamples-results-regions-2010-2018.csv")
trans.c <- read.csv("out/translation-table-regions-2010-2018.csv")
regions.c <- read.csv("out/regions-results-2010-2018.csv")

results.c <- results.c %>% rename(ID_sample = subsample)

name_sample <- c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", 
                 "Region 6", "Region 7")

trans.c$name_sample <- rep(name_sample, 50)

data.c <- merge(results.c, trans.c)


ged.c <- ged %>% filter(year > 2009)



# Comparison 2010-2018 --------------------------------------------------------------

# all

ggplot(data.c, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.c, aes(x = region, y = alpha), 
             shape = 8, color = "red") +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2010-2018", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-regions-2010-2018.pdf", width = 6, height = 4, dpi = 300)


# only with P-value => 0.1

data.c.pval <- data.c %>% filter(p >= 0.1)

ggplot(data.c.pval, aes(x = name_sample, y = alpha)) +
  geom_boxplot() +
  
  geom_point(data = regions.c, aes(x = region, y = alpha), 
             shape = 8, color = ifelse(regions.c$pKS >= 0.1, "red", "grey")) +
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "2010-2018", x = "", y = "Alpha")
ggsave(filename = "figs/comparison-pval-regions-2010-2018.pdf", width = 6, height = 4, dpi = 300)


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
  rename(region = name_sample) %>% 
  left_join(regions.c) %>% 
  select(-c(n, xmin, ntail, X, region_id)) %>% 
  mutate(pKS = round(pKS, 2)) %>% 
  mutate(alpha = round(alpha, 2)) %>% 
  mutate(alpha_mean = round(alpha_mean, 2))

write.csv(data.c.pval.comp, "out/comparison-pval-regions-2010-2018-tab.csv")


