library(ggplot2)
library(dplyr)
library(lme4)

rm(list = ls())
# Load in cleaned data
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis")
df<- read.csv("../cleaned_data.csv")

endpts <- df %>%
  subset(step %in% c(1,7)) %>%
  mutate(acc = ifelse(step == 1 & response == 0, 1,
                      ifelse(step ==7 & response == 1, 1, 0)))



############### MAKE A FIGURE! ##################################
labs <- paste0(as.character(seq(1:10)*10),"%")
endpts$bins <- cut(endpts$trial, 10, labels = labs)

bin_sum <- endpts %>%
  group_by(group, bins)%>%
  summarise(acc = mean(acc),
            se = sqrt( (acc*(1-acc))/n() ))

mypalette <- c("firebrick3", "royalblue3")

px <- ggplot(bin_sum, aes(bins, acc))+
  geom_smooth(aes(group = group, colour = group), method = "lm", se = FALSE)+
  geom_point(aes(colour = group), size = 2, alpha = 0.5)+
  geom_errorbar(aes(ymin=acc-se*1.96, ymax = acc+se*1.96, colour = group), alpha = 0.5)+
  geom_line(aes(colour = group, group = group), alpha = 0.5)+
  scale_colour_manual(values = mypalette, name = "Group")+
  theme_light()+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlab("Task progress")+
  ylab("Accuracy")

px


ggsave("fatigue_fx_group.pdf", px,
       device=cairo_pdf, width=8, height=6)
ggsave("fatigue_fx_group.png", px,
       width=8, height=8)

