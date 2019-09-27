#############################################################################################3
###################### analyze if parameters of the psychometric functions changed ###########
library(dplyr)
library(lme4)
library(pbkrtest)
library(ggplot2)
library(xtable)
library(kableExtra)
rm(list = ls())
psychometrics <- read.csv("../cleaned_psychometrics.csv")
psychometrics <- psychometrics %>%
  subset(threshold > 1 & threshold < 7)


## set deviation contrasts
type_dimnames <- list(levels(psychometrics$type),
                      levels(psychometrics$type)[2])
contrasts(psychometrics$type) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=type_dimnames)

### center reading score, etc
psychometrics$adhd_dx <- as.logical(psychometrics$adhd_dx)
psychometrics$read <- scale(psychometrics$read) #log(max(psychometrics$wj_brs)+1 - psychometrics$wj_brs)
psychometrics$wasi_mr_ts <- scale(psychometrics$wasi_mr_ts)

## ## ## ## ## ## ##
##  SLOPE MODELS  ##
## ## ## ## ## ## ##
full_model <- lmer(slope ~ read*type + wasi_mr_ts + age_at_testing + adhd_dx + (1|subject_id),
                   data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

# Remove nuisance vars?
no_adhd_dx <- lmer(slope ~ read*type + wasi_mr_ts + age_at_testing + (1|subject_id),
                   data=psychometrics)
anova(full_model, no_adhd_dx) # OK to remove adhd

no_wasi <- lmer(slope ~ read*type + age_at_testing +(1|subject_id), 
                data = psychometrics)
anova(no_adhd_dx, no_wasi) # OK to remove wasi

no_age <- lmer(slope ~ read*type + (1|subject_id),
               data = psychometrics)
anova(no_wasi, no_age) # OK to remove age

# Now on to the main effects... try testing no type
no_type <- lmer(slope ~ read + (1|subject_id),
                data = psychometrics)
coefs <- data.frame(coef(summary(no_age)))
df.KR <- get_ddf_Lb(no_age, fixef(no_age))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)


anova(no_wasi, no_type) # OK to remove type
final_slope <- no_type

# Get the p-values
coefs <- data.frame(coef(summary(final_slope)))
df.KR <- get_ddf_Lb(final_slope, fixef(final_slope))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill")

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


coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_slope_sum <- coefs




### ## ## ## ## ## ## ##
##  LAPSE RATE MODEL ##
## ## ## ## ## ## ## ##
psychometrics$lapse <- with(psychometrics, (lo_asymp + hi_asymp) / 2)

full_model <- lmer(lapse ~ read*type + wasi_mr_ts + age_at_testing + adhd_dx + (1|subject_id),
                  data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

# Remove nuisance vars?
no_adhd_dx <- lmer(lapse ~ read*type + wasi_mr_ts + age_at_testing + (1|subject_id),
                   data=psychometrics)
anova(full_model, no_adhd_dx) # OK to remove adhd

no_wasi <- lmer(lapse ~ read*type + age_at_testing +(1|subject_id), 
                data = psychometrics)
anova(no_adhd_dx, no_wasi) # OK to remove wasi

no_age <- lmer(lapse ~ read*type + (1|subject_id),
               data = psychometrics)
anova(no_wasi, no_age) # NOT OK to remove age


coefs <- data.frame(coef(summary(no_wasi)))
df.KR <- get_ddf_Lb(no_wasi, fixef(no_wasi))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)



no_type <- lmer(lapse ~ read + age_at_testing + (1|subject_id),
                data = psychometrics)
anova(no_wasi, no_type) #OK to remove type
final_lapse <- no_type

coefs <- data.frame(coef(summary(final_lapse)))
df.KR <- get_ddf_Lb(final_lapse, fixef(final_lapse))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Age")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_lapse_sum <- coefs





#####################
#### PCA MODEL ######
#####################

params <- psychometrics[,4:7]
PCA<- prcomp(params, scale=TRUE)
psychometrics <- cbind(psychometrics, PCA$x)
summary(PCA)

full_model <- lmer(PC1 ~ read*type + wasi_mr_ts + age_at_testing + adhd_dx + (1|subject_id),
                   data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

# Remove nuisance vars?
no_adhd_dx <- lmer(PC1 ~ read*type + wasi_mr_ts + age_at_testing + (1|subject_id),
                   data=psychometrics)
anova(full_model, no_adhd_dx) # OK to remove adhd

no_wasi <- lmer(PC1 ~ read*type + age_at_testing +(1|subject_id), 
                data = psychometrics)
anova(no_adhd_dx, no_wasi) # OK to remove wasi

no_age <- lmer(PC1 ~ read*type + (1|subject_id),
               data = psychometrics)
anova(no_wasi, no_age) # NOT OK to remove age

no_type <- lmer(PC1 ~ read + age_at_testing + (1|subject_id),
                data = psychometrics)
anova(no_wasi, no_type) #OK to remove type

final_PC1 <- no_type

coefs <- data.frame(coef(summary(final_PC1)))
df.KR <- get_ddf_Lb(final_lapse, fixef(final_PC1))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Age")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_PC1_sum <- coefs


###### Print all the tables
options(knitr.table.format = "latex")


kable(final_slope_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)
kable(final_lapse_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)
kable(final_PC1_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)



#################### CATEGORICAL ####################
group_slope <- lmer(slope ~ group*type + (1|subject_id), psychometrics)
df.KR <- get_ddf_Lb(group_slope, fixef(group_slope))
Fval <- anova(group_slope)$F
df1 <- anova(group_slope)$Df
omnibus_sig = 1-pf(Fval, df1, df.KR)
omnibus_sig


group_lapse<- lmer(lapse ~ group*type + (1|subject_id), psychometrics)
df.KR <- get_ddf_Lb(group_lapse, fixef(group_lapse))
Fval <- anova(group_lapse)$F
df1 <- anova(group_lapse)$Df
omnibus_sig = 1-pf(Fval, df1, df.KR)
omnibus_sig

group_PC1 <- lmer(PC1 ~ group*type + (1|subject_id), psychometrics)
df.KR <- get_ddf_Lb(group_PC1, fixef(group_PC1))
Fval <- anova(group_PC1)$F
df1 <- anova(group_PC1)$Df
omnibus_sig = 1-pf(Fval, df1, df.KR)
omnibus_sig

################## COHENS D ################
psy_sum <- psychometrics %>%
  group_by(subject_id)%>%
  summarise(PC1 = mean(PC1),
            slope = mean(slope),
            lapse = mean(lapse),
            group = unique(group))
psy_sum$group <- factor(psy_sum$group)

cohen.d(psy_sum$PC1, psy_sum$group)

cohen.d(psy_sum$slope, psy_sum$group)

cohen.d(psy_sum$lapse, psy_sum$group)

