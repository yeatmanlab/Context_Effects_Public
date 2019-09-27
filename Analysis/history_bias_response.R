# Looking at bias and recency effects
library(ggplot2)
library(dplyr)

rm(list = ls())
# Load in cleaned data
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis")
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


# First, let's estimate a psychometric function for every subject as a function of the last sound.
get_prev_step <- function(df){
  df <- df[order(df$trial_mod),]
  
  df$prev_0 <- df$step
  df$prev_1 <- c("NA",df$step[1:(length(df$step)-1)] )
  df$prev_2 <- c("NA","NA",df$step[1:(length(df$step)-2)] )
  df$prev_3 <- c("NA","NA","NA",df$step[1:(length(df$step)-3)] )
  df$prev_4 <- c("NA","NA","NA","NA",df$step[1:(length(df$step)-4)] )
  
  return(df)
  
}

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

binary <- subset(out, prev_1 != "NA")
binary$last_category <- ifelse(binary$prev_1 < 4, "Ba",
                               ifelse(binary$prev_1 >4, "Da",
                                      "NA"))
binary <- binary %>% subset(last_category != "NA")


# Make psychometrics for each group
binary_psy <- binary %>%
  group_by(subject_id,step, last_category) %>%
  summarise(prct = mean(response),
            group = unique(group)) %>%
  group_by(group, step, last_category) %>%
  summarise(prct_mu = mean(prct),
            se = sd(prct)/sqrt(n()) )

ggplot(binary_psy, aes(step, prct_mu)) +
  geom_line(aes(group = last_category, colour = last_category)) +
  facet_wrap(~group)+
  geom_ribbon(aes(ymin = prct_mu -se, ymax = prct_mu + se, group = last_category), alpha = 0.5)

# Another way of viewing this
oneback <- subset(out, prev_1 != "NA" & prev_0 != "NA")
oneback$delta_1 <- oneback$prev_0 - as.numeric(oneback$prev_1 )
oneback <- mutate(oneback, sign = ifelse(delta_1 <0, "-1",
                                         ifelse(delta_1 > 0, "1",
                                                "NA")))
oneback <- oneback %>% subset(sign != "NA")


oneback_sum <- oneback %>% 
  group_by(group, sign, step)%>%
  summarise(prct = mean(response))
oneback_sum$sign <- factor(oneback_sum$sign) #, 
oneback_sum <- subset(oneback_sum, sign != "NA")
droplevels(oneback_sum$sign)
levels(oneback_sum$sign) = c("Last sound\nmore `Ba`-like","Last sound\nmore `Da`-like","NA")

# Oneback  
# <- oneback %>%
#  group_by(group, delta_1, step) %>%
#  summarise(prct = mean(response)) %>%
#  mutate(half = ifelse(step >=4, "1","0"))

#ggplot(oneback_sum, aes(delta_1, prct))+
#  geom_line(aes(group = half ))+
# facet_wrap(~group)


ggplot(oneback_sum, aes(step, prct)) +
  geom_line(aes(group = sign, colour = sign), size = 2) +
  facet_wrap(~group)+
  xlab("Step") +
  ylab("Percent labelled 'Da'")+
  theme(text = element_text(size = 14))

# What if we did this by individual? 
oneback_sum <- oneback %>% 
  mutate(sign = ifelse(delta_1 <0, "-1",
                       ifelse(delta_1 > 0, "1",
                              "NA")))%>%
  group_by(subject_id, sign, step)%>%
  subset(sign != "NA") %>%
  summarise(prct = mean(response))
oneback_sum$sign <- factor(oneback_sum$sign) #, 
levels(oneback_sum$sign) = c("Last sound\nmore `Ba`-like","Last sound\nmore `Da`-like")

ggplot(oneback_sum, aes(step, prct)) +
  geom_line(aes(group = sign, colour = sign), size = 2) +
  facet_wrap(~subject_id)+
  xlab("Step") +
  ylab("Percent labelled 'Da'")+
  theme(text = element_text(size = 14))


###### WHAT IF we fit a probit to everyone #####
fit_probit_on_half <- function(df, subject_id_in, sign_in){
  sub <- subset(df, subject_id == subject_id_in & sign == sign_in)
  myprobita <- glm(response ~ step, family = binomial(link="probit"),
                   data = sub)
  return(myprobita$coefficients[1])
}


fit_probit_on_half(oneback, 846,-1)

#### Their wacko method
oneback$step_sign <- ifelse(oneback$step > 4,"1",
                            ifelse(oneback$step < 4, "-1",
                                   "NA"))
sub <- subset(oneback, subject_id == 205)
sub <- subset(sub, sign != "NA")

mu_resp <- sub %>%
  group_by(step_sign, sign)%>%
  summarise(mean = mean(response))


###### LETS MAKE LINEAR MODELS ########
glm_df <- subset(out, prev_1 != "NA") # &
#  prev_2 != "NA" & prev_3 != "NA" 
#& prev_4 != "NA")

glm_df$d_inf <- glm_df$d_mean
glm_df$prev_0 <- as.numeric(glm_df$prev_0)
glm_df$prev_1 <- as.numeric(glm_df$prev_1)
glm_df$prev_2 <- as.numeric(glm_df$prev_2)
glm_df$prev_3 <- as.numeric(glm_df$prev_3)
glm_df$prev_4 <- as.numeric(glm_df$prev_4)

glm_df$d_1 <- glm_df$prev_0 - glm_df$prev_1
glm_df$d_2 <- glm_df$prev_0 - glm_df$prev_2
glm_df$d_3 <- glm_df$prev_0 - glm_df$prev_3
glm_df$d_4 <- glm_df$prev_0 - glm_df$prev_4

dys_glm <- glm(response ~ d_inf + d_1 + (1|subject_id), data = subset(glm_df, group == "Dyslexic"),family=binomial(link="probit"))
ctrl_glm <- glm(response ~ d_inf + d_1 + (1|subject_id), data = subset(glm_df, group != "Dyslexic"),family=binomial(link="probit"))

dys_sum <- as.data.frame(summary(dys_glm)$coefficients)
dys_sum$group = "Dyslexic"
dys_sum$trial = row.names(dys_sum)


ctrl_sum <- as.data.frame(summary(ctrl_glm)$coefficients)
ctrl_sum$group = "Control"
ctrl_sum$trial = row.names(ctrl_sum)


glm_sum <- rbind(ctrl_sum, dys_sum)
glm_sum

glm_sum$trial <- factor(glm_sum$trial, levels = c("d_inf","d_1","(Intercept)"))
levels(glm_sum$trial) <- c("t_inf","t-1","Intercept")
glm_sum <- subset(glm_sum, trial != "Intercept")


ggplot(glm_sum, aes(trial, Estimate, colour = group)) +
  geom_point()+
  geom_line(aes(group = group))+
  geom_errorbar(aes(x=trial, ymin=Estimate + `Std. Error`, ymax=Estimate - `Std. Error`))+
  xlab("Trial")+
  ylab("Coefficient")+
  theme(text = element_text(size = 16))
