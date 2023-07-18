### Preamble ###############################################################
# Fitting data to the power-law distribution: Armed conflict in Colombia
# Supplementary material for the article Explaining conflict violence in terms 
# of conflict actor dynamics  by Tkacova, Idler, Johnson and Lopez (2023)
# Date created: 12 July 2023

rm(list= ls())

library(ggplot2)
library(poweRlaw)
library(tidyverse)

set.seed(18)

data <- readRDS("data/colombia.rds")

png(file="figs/colombia-frequency-distribution.png")
hist(data$deaths_total, breaks = max(data$deaths_total),
     xlab="Severity (battle deaths)", ylab="Frequency", main="")
fatalities_summary <- summary(data$deaths_total)
fatalities_mean <- fatalities_summary[4]
abline(v=fatalities_mean, col="blue")
text(100, 1000, paste("Mean =", round(fatalities_mean,2)), srt=0.2, col = "blue")
dev.off()


m_sp = displ$new(data$deaths_total)
est_sp = estimate_xmin(m_sp)
m_sp$setXmin(est_sp)

bs_p = bootstrap_p(m_sp, no_of_sims=5000, threads=2, xmins = seq(1, 30, 1))

png(file="figs/colombia-frequency-size-distribution.png")
plot(m_sp, pch=1, bg=1, panel.first=grid(col="grey80"),
     xlab="Severity (battle deaths)", ylab="CDF")
title(main="")
lines(m_sp, col=2, lwd=1)
text(50, 0.06, paste("alpha =", round(est_sp$pars,2)), col = "red")
text(200, 0.5, paste("p(KS) =", round(bs_p$p,2)), srt=0.2, col = "grey40")
dev.off()

png(file="figs/colombia-bootstrapping-results.png")
plot(bs_p)
title(main="Colombia", adj=1,line=-15)
dev.off()




