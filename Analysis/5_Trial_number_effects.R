library(ggplot2)
library(dplyr)
library(lme4)
library(kableExtra)
rm(list = ls())
# Load in cleaned data
setwd("/home/eobrien/bde/Projects/Speech_dist/Analysis")
df<- read.csv("../cleaned_data.csv")

endpts <- df %>%
  subset(step %in% c(1,7)) %>%
  mutate(acc = ifelse(step == 1 & response == 0, 1,
                      ifelse(step ==7 & response == 1, 1, 0)))

#### Is there an effect of trial # on response?
# If you remvoe subject 1254, the interaction goes away completely!
endpts$trial <- scale(endpts$trial)
endpts$read <- scale(endpts$read)
lmfit <- glmer(acc ~ trial*read + (1|subject_id), endpts, family=binomial(link="probit"))
summary(lmfit)

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



coefs <- data.frame(coef(summary(lmfit)))
colnames(coefs) <- c("$\\beta$", "SE","$z$","$p$")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)

options(knitr.table.format = "latex")
kable(coefs[-3], booktabs = TRUE, linesep = "", escape = FALSE)

########## What about reaction time? ###########################
# Filter to remove any spurious reaction times (too long or too short)
endpts_rt <- endpts %>%
  subset(RT > 0.2) %>%
  subset(RT < 3)

# Log to make them normally distributed (ish, it's not perfect)
endpts_rt$logRT <- log(endpts_rt$RT)
lmfitRT <- lmer(logRT ~ trial*read + (1|subject_id), endpts_rt)
summary(lmfitRT)

# Need to do a KR approx to estimate p from t
coefs <- data.frame(coef(summary(lmfitRT)))
df.KR <- get_ddf_Lb(lmfitRT, fixef(lmfitRT))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))

colnames(coefs) <- c("$\\beta$", "SE","$z$","$p$")
coefs$`$p$` <- pvalr(coefs$`$p$`)
coefs[,1:3] <- round(coefs[,1:3],3)

# Print it out
options(knitr.table.format = "latex")
kable(coefs[-3], booktabs = TRUE, linesep = "", escape = FALSE)
