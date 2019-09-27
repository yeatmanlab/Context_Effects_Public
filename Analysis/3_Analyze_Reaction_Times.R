##############################################################################################3
############# 3. Analyze how reaction times depend on stimulus distribution #####################
#################################################################################################

library(dplyr)
library(ggplot2)
library(lme4)
rm(list = ls())
## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis/Figures")
setwd("../..")


clean_data <- read.csv("cleaned_data.csv", stringsAsFactors=FALSE)
group_levels <- c("Dyslexic", "Control")
clean_data$group <- factor(clean_data$group, levels=group_levels)

clean_data <- clean_data %>%
  subset(RT < 2 ) %>%
  subset(RT > 0.2)

# Does reaction time depend on condition?
clean_data$read <- scale(clean_data$read)

clean_data$log_RT <- log(clean_data$RT)
ggplot(clean_data, aes(log_RT))+
  geom_density(aes(fill = type), alpha = 0.5)+
  facet_wrap(~group)

# Does mean reaction time depend on reading skill?
lmfit <- lmer(log_RT ~ type*read + age_at_testing + (1 + type|subject_id) + (1|step), clean_data)
summary(lmfit) # Yes- there is an interaction of continuum type and reading skill

# Uniform is overall associated with slower reaction times
# Age at testing is related strongly to reaction time- older -> faster
# There is a significant interaction of continuum type and reading skill- in estimating mean reaction time

df_sum <- clean_data %>%
  group_by(subject_id, type) %>%
  summarise(mu = mean(log_RT),
            se = sd(RT)/sqrt(n()),
            read = unique(read),
            group = unique(group))

# Here we can see that there is a strong relationship between reading and mean RT in the Gaussian condition
ggplot(df_sum, aes(read, mu))+
  #facet_wrap(~type) +
  geom_smooth(method = "lm", aes(group = type, colour = type))+
  theme_light()


df_sum_step <- clean_data %>%
  group_by(subject_id, type,step) %>%
  summarise(mu = mean(log_RT),
            se = sd(log_RT)/sqrt(n()),
            read = unique(read),
            group = unique(group))


# We see some evidence that the shape of the RT functions (of step) are what differ
ggplot(clean_data, aes(step, log_RT, colour = group, fill = group))+
  geom_smooth(method = "gam", formula = y ~ poly(x,2))+
  facet_wrap(~type)

ggplot(clean_data, aes(step, log_RT, colour = type, fill = type))+
  geom_smooth(method = "gam", formula = y ~ poly(x,2))+
  facet_wrap(~group)

# Individual plots?



# A linear model says....
lmfit <- lmer(log_RT ~ I(step^2)*read + (1|subject_id), clean_data)
summary(lmfit)

lmfit <- lmer(log_RT ~ poly(step,2)*read + (1|subject_id), clean_data)
summary(lmfit)



# High reading skill --> low RT
# Uniform continuum --> more positive 2nd order 


# For each subject, fit a polynomial
subj_list = unique(clean_data$subject_id)
subj_list <- subj_list[-c(32,41,50)]

u_coefs <- vector()
g_coefs <- vector()
for (s in subj_list){
  sub <- subset(clean_data, subject_id == s)
  
  modelG <- lm(log_RT ~ poly(step,2), subset(sub, type == "Gaussian"))
  modelU <- lm(log_RT ~ poly(step,2), subset(sub, type == "Uniform"))
  
  u_coefs<- rbind(u_coefs, unlist(modelU$coefficients[3]))
  g_coefs <- rbind(g_coefs, unlist(modelG$coefficients[3]))
} 

fits <- data.frame(subject_id = subj_list,
                   u_coef = u_coefs[,1],
                   g_coef = g_coefs[,1])
fits <- gather(fits, key = type, value = coef, u_coef:g_coef)

read_df <- clean_data %>%
  group_by(subject_id) %>%
  summarise(read = unique(read),
            group = unique(group))
fits <- merge(fits, read_df)

# Individual 2nd order polynomial fits show evidence for correlation with reading skill
summary(lmer(coef ~ read +(1|subject_id), fits))

ggplot(fits, aes(read, coef))+
  geom_point()+
  facet_wrap(~type)+
  geom_smooth(method = "lm")


