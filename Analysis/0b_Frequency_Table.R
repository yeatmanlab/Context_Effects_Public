# Make table of stimulus presentations
library(kableExtra)
library(data.table)

rm(list = ls())
stimlist <- read.csv("../Stim_List/Stimlist_Gaussian_Ba_Da.txt", header = FALSE)

freq_tab <- as.data.frame(table(stimlist$V1))
freq_tab <- transpose(freq_tab)
freq_tab <- freq_tab[-1,]
row.names(freq_tab) <- NULL
colnames(freq_tab) <- c("1","2","3","4","5","6","7")

options(knitr.table.format = "latex")
kable(freq_tab, booktabs = TRUE, linesep = "", escape = FALSE)
Bet
