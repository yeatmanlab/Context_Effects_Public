# Make a plot of the slope, lapse, and PC components for all the data we have so far
library(ggplot2)
library(tidyr)
library(ggExtra)
library(ggpubr)
library(RColorBrewer)

rm(list = ls())

# Load in the data
psychometrics <- read.csv("../../cleaned_psychometrics.csv")
psychometrics <- psychometrics %>%
  subset(threshold >1 & threshold < 7)
# Do PCA
params <- psychometrics[,4:7]
PCA<- prcomp(params, scale=TRUE)
psychometrics <- cbind(psychometrics, PCA$x)


# Estimate the lapse rate
psychometrics$lapse_rate <- with(psychometrics, (lo_asymp + hi_asymp) / 2)

# Get the composite reading score
psychometrics$read <- (psychometrics$wj_brs + psychometrics$twre_index)/2 
# Melt the dataframe
psych_sub <- psychometrics %>%
  dplyr::select(c("type","subject_id", "read","slope","lapse_rate","threshold","PC1"))
psych_sub <- gather(psych_sub, condition, measurement, slope:PC1, factor_key = TRUE)

# Formatting for plotting
psych_sub$type <- as.factor(psych_sub$type)
levels(psych_sub$type) = c("Bimodal", "Uniform") 
levels(psych_sub$condition) = c("Slope", "Asymptote","Threshold", "Principal\nComponent")
#psych_sub$condition <- ordered(psych_sub$condition, levels = c("Principal\nComponent", "Slope", "Asymptote"))


# Dummy data
dummy = data.frame(reading_score=80, condition=rep(c("Principal\nComponent", "Slope","Asymptote","Threshold"), each=1), 
                   value=c(2*max(psych_sub$measurement[psych_sub$condition=="Principal\nComponent"]),
                           1.2*max(psych_sub$measurement[psych_sub$condition=="Slope"]),
                           1.2*max(psych_sub$measurement[psych_sub$condition=="Asymptote"]),
                           0.8*max(psych_sub$measurement[psych_sub$condition=="Threshold"])))


px <- ggplot(psych_sub, aes(read, measurement))+
  geom_point()+
  geom_blank(data=dummy, aes(reading_score, value))+
  facet_grid(condition ~ type, scales = "free_y" )+
  geom_smooth(method = "lm", aes(colour = type),size = 1.5, alpha = 0.6)+
  scale_color_brewer(palette = "Set2")+
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
  theme_bw()+
  xlab("Reading Score")+
  ylab("Parameter Estimate")+
  stat_cor(label.y.npc = "top",label.x.npc = "left")+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18),
        legend.position="none",
        panel.spacing = unit(0.5, "lines"))

px



ggsave("Figure1.pdf", px,
       device=cairo_pdf, width=8, height=11)
ggsave("Figure1.png", px,
       width=8, height=11)



