# Let's make some junk data!
rm(list = ls())
library(dplyr)
library(lme4)
n_subj <- 50
subject_id <- unlist(lapply(seq(1,n_subj), as.character))
ntrials <- 210

uniform_d1 <- glm_df %>%
  subset(type == "Uniform") %>%
  select(d_1)
gaussian_d1 <- glm_df %>%
  subset(type == "Gaussian") %>%
  select(d_1)


gauss_df <- data.frame(d_1 = sample(gaussian_d1$d_1, n_subj*ntrials),
                       type = "Gaussian",
                       subject_id = rep(subject_id, ntrials),
                       step = rep(seq(1,7), n_subj*ntrials))
unif_df <- data.frame(d_1 = sample(uniform_d1$d_1, n_subj*ntrials),
                       type = "Uniform",
                      subject_id = rep(subject_id, ntrials),
                      step = rep(seq(1,7), n_subj*ntrials))


sim_df <- rbind(gauss_df, unif_df)

# Simulate each subjects slopes
subj_slopes <- data.frame(subject_id = unique(sim_df$subject_id),
                         slope = -0.041509 )
subj_intercepts <- data.frame(subject_id = unique(sim_df$subject_id),
                              intercept = rnorm(n = n_subj, mean = 0, sd = 0.4976))
sim_df <- merge(sim_df, subj_slopes)
sim_df <- merge(sim_df, subj_intercepts)
sim_df$measure_error <- rnorm(n = nrow(sim_df), mean = 0, sd =5 )
sim_df$step_slope <- -0.006079
sim_df$step <- scale(sim_df$step, scale = FALSE)

# RT depends on :
# 2nd order polynomial of step (centered, of course, the same in all subjects)
# d_1 (history effect, the same in all subjects)
# an intercept (different in all subjects)
# measurement error
sim_df$RT <- (sim_df$step^2*sim_df$step_slope) + (sim_df$slope * sim_df$d_1) + sim_df$intercept + sim_df$measure_error

#ggplot(sim_df, aes(step, RT))+
#  facet_wrap(~subject_id)+
#  geom_smooth()

#ggplot(sim_df, aes(d_1))+
#  geom_density(aes(fill = type))

sim_lm <- lmer(RT ~ I(step^2) + type + d_1 + (1|subject_id), sim_df)
summary(sim_lm)

