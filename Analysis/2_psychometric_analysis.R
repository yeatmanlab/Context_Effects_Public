#############################################################################################3
###################### analyze if parameters of the psychometric functions changed ###########
library(dplyr)
library(lme4)
library(pbkrtest)
library(ggplot2)
library(xtable)
library(kableExtra)
rm(list = ls())
psychometrics <- read.csv("cleaned_psychometrics.csv")

## set deviation contrasts
type_dimnames <- list(levels(psychometrics$type),
                      levels(psychometrics$type)[2])
contrasts(psychometrics$type) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=type_dimnames)

### center reading score, etc
psychometrics$adhd_dx <- as.logical(psychometrics$adhd_dx)
psychometrics$read <- scale(psychometrics$read) 
psychometrics$wasi_mr_ts <- scale(psychometrics$wasi_mr_ts)

## ## ## ## ## ## ##
##  SLOPE MODELS  ##
## ## ## ## ## ## ##

# Base model
full_model <- lmer(slope ~ read*type +  (1|subject_id),
                   data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

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

colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Condition", "Reading skill * Condition")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_slope_sum <- coefs

# Estimate the Bayes Factor from linear model
h0 <- lmer(slope ~ read + type + (1|subject_id),
           data = psychometrics)
h1 <- lmer(slope ~ read*type + (1|subject_id),
           data = psychometrics)

BF = exp( (BIC(h1) - BIC(h0) )/2)
print(paste("BF =", round(BF,1)))



### ## ## ## ## ## ## ##
##  LAPSE RATE MODEL ##
## ## ## ## ## ## ## ##
psychometrics$lapse <- with(psychometrics, (lo_asymp + hi_asymp) / 2)

full_model <- lmer(lapse ~ read*type + (1|subject_id),
                  data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Condition", "Reading skill * Condition")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_lapse_sum <- coefs

# Estimate the Bayes Factor from linear model
h0 <- lmer(lapse ~ read + type + (1|subject_id),
           data = psychometrics)
h1 <- lmer(lapse ~ read*type + (1|subject_id),
           data = psychometrics)

BF = exp( (BIC(h1) - BIC(h0) )/2)
print(paste("BF =", round(BF,1)))

### ## ## ## ## ## ## ##
##  THRESHOLD MODEL ##
## ## ## ## ## ## ## ##
full_model <- lmer(threshold ~ read*type + (1|subject_id),
                   data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))

colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Condition", "Reading skill * Condition")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_thresh_sum <- coefs


# Calculate Bayes Factor
h0 <- lmer(threshold ~ read + type + (1|subject_id),
           data = psychometrics)
h1 <- lmer(threshold ~ read*type + (1|subject_id),
           data = psychometrics)

BF = exp( (BIC(h1) - BIC(h0) )/2)
print(paste("BF =", round(BF,1)))


#####################
#### PCA MODEL ######
#####################

params <- psychometrics[,4:7]
PCA<- prcomp(params, scale=TRUE)
psychometrics <- cbind(psychometrics, PCA$x)
summary(PCA)

full_model <- lmer(PC1 ~ read*type + (1|subject_id),
                   data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))

colnames(coefs) <- c("$\\beta$", "SE","$t$","$p$")
row.names(coefs) <- c("(Intercept)","Reading skill", "Condition", "Reading skill * Condition")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)
final_PC1_sum <- coefs


# Estimate the Bayes Factor from linear model
h0 <- lmer(PC1 ~ read + type + (1|subject_id),
           data = psychometrics)
h1 <- lmer(PC1 ~ read*type + (1|subject_id),
           data = psychometrics)

BF = exp( (BIC(h1) - BIC(h0) )/2)
print(paste("BF =", round(BF,1)))


###### Print all the tables
options(knitr.table.format = "latex")


kable(final_slope_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)
kable(final_lapse_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)
kable(final_thresh_sum[-3],booktabs = TRUE, linesep="", escape= FALSE)
kable(final_PC1_sum[-3], booktabs = TRUE, linesep = "", escape = FALSE)



#################### CATEGORICAL ####################
grp_psy <- subset(psychometrics, group != "Other")

group_slope <- lmer(slope ~ group*type + (1|subject_id), grp_psy)
df.KR <- get_ddf_Lb(group_slope, fixef(group_slope))
Fval <- anova(group_slope)$F
omnibus_sig = 1-pf(Fval, 1, df.KR)
omnibus_sig


group_lapse<- lmer(lapse ~ group*type + (1|subject_id), grp_psy)
df.KR <- get_ddf_Lb(group_lapse, fixef(group_lapse))
Fval <- anova(group_lapse)$F
omnibus_sig = 1-pf(Fval, 1, df.KR)
omnibus_sig

group_PC1 <- lmer(PC1 ~ group*type + (1|subject_id), grp_psy)
df.KR <- get_ddf_Lb(group_PC1, fixef(group_PC1))
Fval <- anova(group_PC1)$F
omnibus_sig = 1-pf(Fval, df1, df.KR)
omnibus_sig

################## COHENS D ################
psy_sum <- grp_psy %>%
  group_by(subject_id)%>%
  summarise(PC1 = mean(PC1),
            slope = mean(slope),
            lapse = mean(lapse),
            group = unique(group))
psy_sum$group <- factor(psy_sum$group)

cohen.d(psy_sum$PC1, psy_sum$group)

cohen.d(psy_sum$slope, psy_sum$group)

cohen.d(psy_sum$lapse, psy_sum$group)

