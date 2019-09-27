#!/usr/bin/env Rscript

## figure_group_response_curves.R
## aggregates the data by group and cue type and plots response curves

library(dplyr)
library(ggplot2)
rm(list = ls())
## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis/Figures")
setwd("../..")


clean_data <- read.csv("cleaned_data.csv", stringsAsFactors=FALSE)
group_levels <- c("Dyslexic", "Below Average", "Above Average", "Group Means")
clean_data$group <- factor(clean_data$group, levels=group_levels)

clean_data$order <- ifelse(clean_data$subject_id %%2 == 0, "Gaussian First","Uniform First")

## ## ## ## ## ## ##
## COLORMAP STUFF ##
## ## ## ## ## ## ##
n_steps <- 256
## RED-PURPLE-BLUE
cmap <- colorRampPalette(c("royalblue3", "firebrick3"))(n_steps)

## FUNCTION FOR COMPUTING COLORS FROM COLORMAPS (FOR GROUP-LEVEL COLORS)
get_cmap_color <- function(x, cmap) {
  ramp <- grDevices::colorRamp(cmap, space="Lab", interpolate="spline")
  rgbs <- ramp(x) / 255
  grDevices::rgb(rgbs[,1], rgbs[,2], rgbs[,3])
}

## ## ## ## ## ## ## ## ## ## ##
## AGGREGATE AT SUBJECT LEVEL ##
## ## ## ## ## ## ## ## ## ## ##
subj_agg <- clean_data %>%
  group_by(subject_id, type, step,order) %>%
  summarise(group=as.character(unique(group)),
            wj_brs=unique(wj_brs),
            response=mean(response, na.rm=TRUE))
subj_agg$plot_group <- as.character(with(subj_agg, interaction(subject_id, type)))
subj_agg <- na.omit(subj_agg)
subj_agg$orig_group <- as.character(subj_agg$group)
subj_agg$step <- as.integer(subj_agg$step)
subj_agg$group <- as.character(subj_agg$group)

# mean + SE by group, across subjects
group_ribbon <- subj_agg %>%
  group_by(type, step,order) %>%
  summarise(wj_brs=mean(wj_brs, na.rm=TRUE),
            response_mu=mean(response),
            response_se=sd(response) / sqrt(n()),
            lower=response_mu - response_se,
            upper=response_mu + response_se) %>%
  rename(response=response_mu, orig_group=order)
group_ribbon$group <- "Group Means"
group_ribbon$group <- factor(group_ribbon$group, levels=group_levels)

group_ribbon$plot_group <- with(group_ribbon, interaction(orig_group, type))
i <- sapply(group_ribbon, is.factor)
group_ribbon[i] <- lapply(group_ribbon[i], as.character)


## avoid the annoying "unequal factor levels" warning
plot_group_levels <- c(levels(factor(subj_agg$plot_group)), levels(factor(group_ribbon$plot_group)))
subj_agg$plot_group <- factor(subj_agg$plot_group, levels=plot_group_levels)
group_ribbon$plot_group <- factor(group_ribbon$plot_group, levels=plot_group_levels)

## unite the 2 tibbles
this_data <- bind_rows(subj_agg, group_ribbon)

## GET THE COLORMAP VALUES FOR THE GROUP MEANS
endpoints <- range(na.omit(clean_data$wj_brs))
c_range <- diff(endpoints)
group_vals <- subj_agg %>% group_by(group) %>% summarise(wj_brs=mean(wj_brs))
group_vals <- (group_vals$wj_brs - endpoints[1]) / c_range
group_cols <- get_cmap_color(group_vals, cmap)
cscale <- scale_colour_gradientn(colours=cmap)
fscale <- scale_fill_gradientn(colours=cmap)
group_cols <- c(group_cols, "#808080")  # for the group-means facet
names(group_cols) <- levels(as.factor(this_data$group))

# Ensure that the order of figures is plotted correctly
this_data$group <- factor(this_data$group, levels=c('Dyslexic','Below Average','Above Average','Group Means'))

## custom labeller for the strip
duration_labeller <- as_labeller(c(`U`="Uniform",
                                   `G`="Gaussian",
                                   `Dyslexic`="Dyslexic",
                                   `Below Average`="Below Average",
                                   `Above Average`="Above Average",
                                   `Group Means`="Group Means"))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  FACET GRID OF INDIVID. + MEAN RESPONSE CURVES W/ WJBRS COLORMAP ON CURVES ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
sum_only <- subset(this_data, group == "Group Means")

fig <- ggplot(sum_only, mapping=aes(x=step, y=response, group=orig_group)) +
  geom_line(aes(linetype = type, group = type), size=0.75, alpha=1) +
  #geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, size=0) +
  scale_x_continuous(labels=as.character, breaks=seq(1, 7)) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0, 1, by=0.25)) +
  cscale + fscale + theme_bw() +
  theme(text=element_text(size=12, colour="black", family="Open Sans"),
        strip.text.x=element_text(face="bold", colour="white"),
        strip.text.y=element_text(face="plain", size=10),
        strip.placement="outside", panel.grid.minor=element_blank()) +
  labs(title="", colour="WJ-BRS", fill="WJ-BRS") +
  xlab("Continuum step") + ylab("") +
  facet_grid(~orig_group)

fig
## PLOT IT INTERACTIVELY
grid::grid.draw(gtable)

## SAVE TO FILE
setwd('./Analysis/Figures')
ggsave("figure_individ_and_mean_resp_curves_by_group.pdf", gtable,
       device=cairo_pdf, width=7.5, height=4.5)
ggsave("figure_individ_and_mean_resp_curves_by_group.png", gtable,
       width = 7.5, height = 4.5, dpi = 300)
