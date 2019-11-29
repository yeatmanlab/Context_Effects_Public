library(ggplot2)
library(dplyr)
library(lme4)

rm(list = ls())
# Load in cleaned data
setwd("/home/eobrien/bde/Projects/Context_Effects_Public/Analysis")
df<- read.csv("../cleaned_data.csv")

# Distance from the mean
df$d_mean = df$step - 4

# Since we break every 35 stimuli, only consider stimulus order WITHIN A BLOCK
df = mutate(df, block = ifelse(trial < 36, 1,
                               ifelse(trial < 71, 2,
                                      ifelse(trial < 106, 3,
                                             ifelse(trial < 141, 4,
                                                    ifelse(trial < 176, 5,
                                                           6
                                                    ))))))
hist(df$block)                                       
df$trial_mod = df$trial %% 35 + 1
df$trial_code <- ifelse(df$type == "Gaussian", paste0("Gaussian_", df$block),
                        paste0("Uniform_",df$block))


# Define a function for getting the previous step
get_prev_step <- function(df){
  df <- df[order(df$trial_mod),]
  
  df$prev_0 <- df$step
  df$prev_1 <- c("NA",df$step[1:(length(df$step)-1)] )
  df$prev_2 <- c("NA","NA",df$step[1:(length(df$step)-2)] )
  df$prev_3 <- c("NA","NA","NA",df$step[1:(length(df$step)-3)] )
  df$prev_4 <- c("NA","NA","NA","NA",df$step[1:(length(df$step)-4)] )
  
  return(df)
  
}

### For every participant, get their 
out <- data.frame()
subj_list <- unique(df$subject_id)
for (s in subj_list){
  
  sub <- subset(df, subject_id == s)
  blocks <- unique(sub$trial_code)
  
  for (b in blocks){
    sub_block <- subset(sub, trial_code == b)
    tmp <- get_prev_step(sub_block)
    out = rbind(out, tmp)
  }
  
}


glm_df <- out

glm_df$d_0 <- glm_df$d_mean
glm_df$prev_0 <- as.numeric(glm_df$prev_0)
glm_df$prev_1 <- as.numeric(glm_df$prev_1)
glm_df$prev_2 <- as.numeric(glm_df$prev_2)
glm_df$prev_3 <- as.numeric(glm_df$prev_3)
glm_df$prev_4 <- as.numeric(glm_df$prev_4)

glm_df$d_1 <- glm_df$prev_0 - glm_df$prev_1
glm_df$d_2 <- glm_df$prev_0 - glm_df$prev_2
glm_df$d_3 <- glm_df$prev_0 - glm_df$prev_3
glm_df$d_4 <- glm_df$prev_0 - glm_df$prev_4

# 
glm_df$read <- scale(glm_df$read)

# If we were to split this up by group....
dys_glm <- glmer(response ~ d_0 + d_1 + d_2 + d_3 + d_4 + (1|subject_id), 
                 data = subset(glm_df, group == "Dyslexic"),
                 family=binomial(link="probit"),
                 control = glmerControl(optimizer ="Nelder_Mead"))
ctrl_glm <- glmer(response ~ d_0 + d_1 + d_2 + d_3 + d_4 + (1|subject_id), 
                  data = subset(glm_df, group != "Dyslexic"),
                  family=binomial(link="probit"),
                  control = glmerControl(optimizer ="Nelder_Mead"))

dys_sum <- as.data.frame(summary(dys_glm)$coefficients)
dys_sum$group = "Dyslexic"
dys_sum$trial = row.names(dys_sum)


ctrl_sum <- as.data.frame(summary(ctrl_glm)$coefficients)
ctrl_sum$group = "Control"
ctrl_sum$trial = row.names(ctrl_sum)

glm_sum <- rbind(ctrl_sum, dys_sum)

mypalette <- c("firebrick3", "royalblue3")
px <- ggplot(glm_sum, aes(trial, Estimate, colour = group)) +
  geom_point()+
  geom_line(aes(group = group))+
  geom_errorbar(aes(x=trial, ymin=Estimate + `Std. Error`*1.96, ymax=Estimate - `Std. Error`*1.96))+
  xlab("")+
  ylab("Estimate")+
  geom_hline(yintercept = 0)+
  scale_colour_manual(values = mypalette, name = "Group")+
  scale_x_discrete(labels=c("d_1" = expression(d[1]), 
                            "d_2" = expression(d[2]),
                            "d_3" = expression(d[3]),
                            "d_4" = expression(d[4]),
                            "d_0" = expression(d[0])))+
                            
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

px

ggsave("Figures/Figure2.pdf", px,
       device=cairo_pdf, width=8, height=8)
ggsave("Figures/Figure2.png", px,
       width=8, height=8)


# With reading skill as a continuous variable?
big_glm <- glmer(response ~ d_0*read + d_1 + d_2 + (1|subject_id), 
                 data = glm_df,
                 family=binomial(link="probit"),
                 control = glmerControl(optimizer ="Nelder_Mead"))
summary(big_glm)


pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('$<$ %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}



coefs <- data.frame(coef(summary(big_glm)))
colnames(coefs) <- c("$\\beta$", "SE","$z$","$p$")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)

options(knitr.table.format = "latex")
kable(coefs[-3], booktabs = TRUE, linesep = "", escape = FALSE)


# WHAT IF THERE WAS AN INTERACTION WITH D_1?
big_glm2 <- glmer(response ~ d_inf*read + d_1*read + d_2 + (1|subject_id), 
                 data = glm_df,
                 family=binomial(link="probit"),
                 control = glmerControl(optimizer ="Nelder_Mead"))
summary(big_glm2)

# WHAT IF THERE WAS AN INTERACTION WITH D_2?
big_glm3 <- glmer(response ~ d_inf*read + d_1 + d_2*read + (1|subject_id), 
                  data = glm_df,
                  family=binomial(link="probit"),
                  control = glmerControl(optimizer ="Nelder_Mead"))
summary(big_glm3)


# WHAT IF THERE WAS AN INTERACTION WITH STIMULUS DISTRIBUTION? #
big_glm4 <- glmer(response ~ d_0*read + d_1 + d_2 + type*read + (1|subject_id), 
                  data = glm_df,
                  family=binomial(link="probit"),
                  control = glmerControl(optimizer ="Nelder_Mead"))



## WHAT IS THE INFLUENCE OF D_1? SEEMS LIKE ITS INFLUENCE SHOULD BE GREATER WHEN D0 
# IS NEAR THE CENTER OF THE CONTINUUM #
glm_df$is_ambig_0 <- ifelse(glm_df$d_0 %in% c(-1,0,1),"Ambiguous","Endpoint")

d_1mech <- glmer(response ~ d_0 + is_ambig_0:d_1 + (1|subject_id), 
                  data = glm_df,
                  family=binomial(link="probit"),
                  control = glmerControl(optimizer ="Nelder_Mead"))
summary(d_1mech)


############# MAKE A FIGURE ##########################
edsum <- glm_df %>%
  group_by(is_ambig_0, d_1) %>%
  summarise(resp = mean(response))%>%
  subset(d_1 >= -4) %>%
  subset(d_1 <= 4)

ggplot(edsum, aes(d_1, resp))+
  geom_bar(stat = "identity", aes(fill = is_ambig_0), position = "dodge")+
  xlab(expression(d[1]))+
  ylab("Probability of responding 'da'")+  
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

edsum <- glm_df %>%
  subset(type == "Uniform") %>%
  group_by(is_ambig_0, d_1) %>%
  summarise(resp = mean(response),
            se = sd(response)/sqrt(n())) %>%
  subset(d_1 != "NA")


brks = as.character(seq(1,7))
ggplot(edsum, aes(x=d_1, 
                  y=resp,
                  ymin = resp - 1.96*se,
                  ymax = resp + 1.96*se,
                  fill = is_ambig_0)) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.5)+
  ylab('Probability of responding "da"')+  
  xlab("Previous stimulus\n(Step along continuum)")+
  scale_fill_manual(values = c("#999999","#444444"), name = "Current stimulus")+
  scale_x_continuous(breaks = round(seq(1,7, by = 1),1))+
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



edsum <- glm_df %>%
  subset(prev_1 %in% c(1,7)) %>%
  subset(prev_0 %in% c(1,4,7)) %>%
  group_by(prev_0, prev_1,group) %>%
  summarise(mu = mean(response),
            se = sqrt( (mu*(mu))/n() ))
 
edsum$prev_0 = as.factor(edsum$prev_0)
edsum$prev_1 = as.factor(edsum$prev_1)

levels(edsum$prev_1) <- c('Clear "ba"\n(Step 1)', 'Clear "da"\n(Step 7)')
levels(edsum$prev_0) <- c('Clear "ba"\n(Step 1)', 'Ambiguous\n(Step 4)','Clear "da"\n(Step 7)')

cols <- colorRampPalette(brewer.pal(3,"Blues"))(3)
ggplot(edsum, aes(x = prev_1, 
                  y = mu,
                  ymin = mu - 1.96*se,
                  ymax = mu + 1.96*se,
                  fill = prev_0))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(position = position_dodge(0.9), width = 0.5)+
  xlab("Previous stimulus")+
  ylab('Probability of responding "da"')+
  scale_fill_manual(values = cols, name = "Current\nstimulus")+
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(margin = margin(t = 10)))+
  facet_wrap(~group, nrow = 2)


glm_df$prev_group <- ifelse(glm_df$prev_1 %in% c(1,2), "Clear ba",
                            ifelse(glm_df$prev_1 %in% c(6,7), "Clear da",
                                   "Ambiguous"))
edsum <- glm_df %>%
  subset(prev_group != "Ambiguous") %>%
  subset(prev_0 %in% c(3,4,5)) %>%
  group_by(prev_group,group) %>%
  summarise(mu = mean(response),
            se = sqrt( (mu*(mu))/n() ))
mypalette <- c("firebrick3", "royalblue3")
ggplot(edsum, aes(x = prev_group, 
                  y = mu,
                  ymin = mu - 1.96*se,
                  ymax = mu + 1.96*se,
                  fill = group))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(position = position_dodge(0.9), width = 0.5)+
  xlab("Previous stimulus")+
  ylab('Probability of responding "da"')+
  scale_fill_manual(values = mypalette, name = "Group")+
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  facet_wrap(~group)


# OK now what if we had repeated this with RTs?
# If we were to split this up by group....

rt_df <- glm_df %>%
  subset(RT > 0.2) %>%
  subset(RT < 2)
rt_df$log_RT <- log(rt_df$RT)

# What if we had done it by group
big_glm <- lmer(log_RT ~ poly+ d_1*read + d_2 +  adhd_dx + age_at_testing +  (1|subject_id), rt_df)
summary(big_glm)

dys_glm <- lmer(log_RT ~ poly(d_inf,2) + d_1 + d_2 + (1|subject_id), 
                data = subset(rt_df, group == "Dyslexic"), 
                control = lmerControl(optimizer ="Nelder_Mead"))
ctrl_glm <- lmer(log_RT ~ poly(d_inf,2) + d_1 + d_2 + (1|subject_id), 
                 data = subset(rt_df, group != "Dyslexic"),
                 control = lmerControl(optimizer ="Nelder_Mead"))

dys_sum <- as.data.frame(summary(dys_glm)$coefficients)
dys_sum$group = "Dyslexic"
dys_sum$trial = row.names(dys_sum)

ctrl_sum <- as.data.frame(summary(ctrl_glm)$coefficients)
ctrl_sum$group = "Control"
ctrl_sum$trial = row.names(ctrl_sum)

glm_sum <- rbind(ctrl_sum, dys_sum)

# Clean it up!
glm_sum$trial <- factor(glm_sum$trial)
#glm_sum$trial <- factor(glm_sum$trial, levels = c("d_inf","d_1","(Intercept)","d_2","d_3","d_4"))
#levels(glm_sum$trial) <- c("t-1","t-2","t-3","t-4","t_inf","Intercept")
#glm_sum$trial <- relevel(glm_sum$trial, "t_inf")
#glm_sum <- subset(glm_sum, trial %in% c("d_1","d_2"))



ggplot(glm_sum, aes(trial, Estimate, colour = group)) +
  geom_point()+
  geom_line(aes(group = group))+
  geom_errorbar(aes(x=trial, ymin=Estimate + 1.96*`Std. Error`, ymax=Estimate - 1.96*`Std. Error`))+
  xlab("Trial")+
  ylab("Coefficient")+
  ggtitle("Recency effects on reaction time")+
  geom_hline(yintercept = 0)+
  theme_light()+
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

################################################################################
############### Which condition is associated with more d_1 and more d_2 changes? 
ggplot(glm_df, aes(d_1))+
  geom_freqpoly(aes(colour = type), alpha = 0.5)

ggplot(glm_df, aes(d_2))+
  geom_freqpoly(aes(colour = type), alpha = 0.5)

ggplot(rt_df, aes(d_1,log_RT))+
  geom_violin(aes(group = d_1, fill = d_1))+
  coord_flip()+
  facet_wrap(~group)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, colour = "white")+
  theme_light()+
  theme(panel.grid = element_blank())

type_sum <- glm_df %>%
  group_by(type, subject_id) %>%
  summarise(mu = mean(d_1, na.rm = TRUE)) %>%
  group_by(type) %>%
  summarise(mu_d1 = mean(mu))
  

# For each PERSON and each condition, let's fit their history effects
out <- data.frame()

for (s in subj_list){
  subj_df <- subset(rt_df, subject_id == s)

  glm_u <- lm(log_RT ~ poly(d_inf,2) + d_1, data = subj_df)
  
  #glm_u <- summary(lmer(log_RT ~ poly(d_inf,2) + d_1 + (1|step), data = subset(subj_df, type == "Uniform")))
  #glm_g <- summary(lmer(log_RT ~ poly(d_inf,2) + d_1 + (1|step), data = subset(subj_df, type == "Gaussian")))
  out <- rbind(out,c(unname(glm_u$coefficients), s), stringsAsFactors = FALSE)
  #out <- rbind(out,c(unname(glm_g$coefficients), s,"Gaussian"), stringsAsFactors = FALSE)
}

names(out) <- c("intercept","poly1","poly2","d_1","subject_id")
read_df <- glm_df %>%
  select(c(subject_id, read, age_at_testing, wasi_mr_ts, adhd_dx)) %>%
  unique()
subj_glm <- merge(read_df, out)


subj_glm[,3:5] <- lapply(subj_glm[,3:5], as.numeric) 

ggplot(subj_glm, aes(read, d_1))+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(d_1 ~ read, subj_glm))
