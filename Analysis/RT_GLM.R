# Looking at bias and recency effects
library(ggplot2)
library(dplyr)
library(broom)

rm(list = ls())
# Load in cleaned data
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis")
df<- read.csv("../cleaned_data.csv")

df <- subset(df, RT < 4 & RT > 0.2)
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
df$trial_mod = df$trial %% 35 + 1
df$trial_code <- ifelse(df$type == "Gaussian", paste0("Gaussian_", df$block),
                        paste0("Uniform_",df$block))

ggplot(df, aes(trial, RT))+
  geom_line(aes(group = type, colour = type))+
  facet_wrap(~subject_id)+
  geom_smooth(method = "lm", aes(group = type, colour = type))


# Is there any relationship between stationarity of RT and reading skill?
df$read <- scale(df$read)
fit <- lmer(RT ~ trial*type*read + (1|subject_id), df)
summary(fit)


##### Said another way.... if we fit each individuals RTs in each condition with a line, what would happen?
subject_list = unique(df$subject_id)
type_list = unique(df$type)  


dfLearn = df %>% group_by(type,subject_id) %>%
  do(fitLearn = lm(RT ~ trial, data = .))
dfLearnCoef = tidy(dfLearn, fitLearn)

# How many subjects showed any learning effects?
dfLearn <- dfLearnCoef %>%
  subset(term == "trial")
bio <- unique(df[c("subject_id","read")])
dfLearn <- merge(dfLearn, bio)

ggplot(dfLearn, aes(read, estimate))+
  geom_point()+
  facet_wrap(~type)+
  geom_smooth(method = "lm")

fit <- lm(estimate ~ read*type, dfLearn)
summary(fit)
###### It's not very compelling; many participants don't actually seem to learn much. ############################################
###### Small effect that is really only observable when aggregating statistical power ############################################

# Look at stimulus history effects
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


###### LETS MAKE LINEAR MODELS ########
glm_df <- subset(out, prev_1 != "NA")
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


# A continuous model of the effects of the previous timepoint on reaction time
ggplot(glm_df, aes(as.factor(d_1), RT))+
  geom_violin()+
  facet_wrap(group~type)+
  stat_summary(fun.y = median, geom = "point",size =2 )

ggplot(glm_df, aes(d_inf,RT))+
  geom_point()+
  facet_wrap(group~type)

glm_sum <- glm_df %>%
  group_by(subject_id, d_inf,type) %>%
  summarise(med = median(RT),
            read = unique(read))

ggplot(glm_sum, aes(d_inf, med)) + 
  geom_point()+
  geom_line() +
  facet_wrap(type~.) + 
  geom_smooth()+
  coord_cartesian(ylim = c(0.25,0.75))

##### A mixed glm of RT ###############
glm_df$read <- scale(glm_df$read)
glm_df$age_at_testing <- scale(glm_df$age_at_testing)
glm_df$wasi_mr_ts <- scale(glm_df$wasi_mr_ts)

glm_df$d_inf_poly2 <- poly(glm_df$d_inf,2)[,2]
glm_df$d_1_poly2 <- poly(glm_df$d_1,2)[,2]

ggplot(glm_df, aes(read, RT)) +
  geom_boxplot(aes(group = subject_id))+
  facet_wrap(~type)
summary(lmer(med ~ read*type + (1|subject_id), df_sum))

# If we look only at reaction time medians....
df_sum <- df %>%
  group_by(subject_id,type) %>%
  summarise(med = median(RT),
            read = unique(read))


ggplot(df_sum, aes(read, med))+
  geom_point()+
  facet_wrap(~type)+
  geom_smooth(method = 'lm')

# Is there an overall relationship between reaction time and reading skill?
fit <- glmer(RT ~ read*type + (1|subject_id), data = df, family = Gamma(link = "identity"))
summary(fit)

# It seems like the relationship between reading skill and gamma-mu of RT distribution is more negative in the Gaussian condition. 


# In general, is there a measurable effect of stimulus history?
glm_df$abs_d1 <- abs(glm_df$d_1)
hist <- glmer(RT ~ poly(d_inf,2) + abs_d1*read+ (1|subject_id), data = glm_df, family = Gamma(link = "identity"))
summary(hist)

# Is there any reason to think that reading skill interacts with stimulus history?
glm_df$abs_d1 <- abs(glm_df$d_1)
hist <- glmer(RT ~ poly(d_inf,2) + abs_d1*read+ (1|subject_id), data = glm_df, family = Gamma(link = "identity"))
summary(hist)

# There is a significant interaction between polynomial shape and reading skill 
hist <- glmer(RT ~ poly(d_inf,2)*read + abs_d1 + (1|subject_id), data = glm_df, family = Gamma(link = "identity"))
summary(hist)

# Is there a significant interaction between polynomial shape, reading skill, and type?
type_fx <- glmer(RT ~ read + type + poly(step,2) +  poly(step,2):read:type + (1|subject_id), data = df, family = Gamma(link = "identity"))
summary(type_fx)



# What if we fit each subject's dependence on stimulus history?
subj_list = unique(glm_df$subject_id)
g_fit <- data.frame()
u_fit <- data.frame()
for (s in subj_list){
  sub <- subset(glm_df, subject_id == s)
  
  tryCatch({
  fit_U <- glm(RT ~ poly(step,2), data = subset(sub,type == "Uniform"), family = Gamma(link = "identity"))
  fit_G <- glm(RT ~ poly(step,2), data = subset(sub, type == "Gaussian"), family = Gamma(link = "identity"))
  # Append to dataframes
  g_fit <- rbind(g_fit, c(fit_G$coefficients, s, unique(sub$read)))
  u_fit <- rbind(u_fit, c(fit_U$coefficients, s, unique(sub$read))) 
  }, error = function(e){})
}

names(g_fit) <- c("Intercept","poly1","poly2","subject_id","read")
names(u_fit) <- c("Intercept","poly1","poly2","subject_id","read")

g_fit$type = "Gaussian"
u_fit$type = "Uniform"

subj_glm <- rbind(g_fit, u_fit)
ggplot(subj_glm, aes(read, poly2)) +
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~type)

summary(lmer(poly2 ~ read + type + (1|subject_id), subj_glm))

