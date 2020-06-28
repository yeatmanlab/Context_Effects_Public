#########################################################################################
#### Is there any effect of which distribution came first on psychometric fn shape? #####
#########################################################################################
rm(list = ls())
library(dplyr)
library(lme4)
library(pbkrtest)
library(ggplot2)

psychometrics <- read.csv("cleaned_psychometrics.csv")

table(psychometrics$Gauss_first)/2

## set deviation contrasts
type_dimnames <- list(levels(psychometrics$type),
                      levels(psychometrics$type)[2])
contrasts(psychometrics$type) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=type_dimnames)



### center reading score, etc
psychometrics$adhd_dx <- as.logical(psychometrics$adhd_dx)
psychometrics$read <- scale(psychometrics$read) #log(max(psychometrics$wj_brs)+1 - psychometrics$wj_brs)
psychometrics$wasi_mr_ts <- scale(psychometrics$wasi_mr_ts)
psychometrics$Gauss_first <- as.logical(psychometrics$Gauss_first)

# Do PCA
params <- psychometrics[,4:7]
PCA<- prcomp(params, scale=TRUE)
psychometrics <- cbind(psychometrics, PCA$x)
summary(PCA)

ggplot(psychometrics, aes(read,PC1))+
  geom_point(aes(colour = type))+
  facet_wrap(~Gauss_first)+
  geom_smooth(method = "lm", aes(group = type, colour = type))

m1 <- lmer(slope ~ type*Gauss_first +(1|subject_id), psychometrics)
summary(m1)

