library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(kableExtra)
library(abind)

rm(list = ls())
bio <- read.csv("../cleaned_data.csv")

bio_df <- bio %>%
  group_by(subject_id)%>%
  summarise(wasi_fs2 = unique(wasi_fs2),
            wasi_mr_ts = unique(wasi_mr_ts),
            wj_brs = unique(wj_brs),
            wj_wa_ss = unique(wj_wa_ss),
            wj_lwid_ss = unique(wj_lwid_ss),
            twre_index = unique(twre_index),
            twre_pde_ss = unique(twre_pde_ss),
            twre_swe_ss = unique(twre_swe_ss),
            ctopp_pa = mean(ctopp_pa),
            ctopp_pm = mean(ctopp_pm),
            ctopp_rapid = mean(ctopp_rapid),
            group = unique(group))

demo_df <- bio %>%
  group_by(subject_id) %>%
  summarise(group = unique(group),
            adhd_dx = unique(adhd_dx),
            gender = unique(gender),
            age_at_testing = unique(age_at_testing),
            read = unique(read))



# Some numerical formatting
sigdig = 3
# What is the correlation of WJ and TOWRE? 
wj_towre <- summary(lm(wj_brs ~ twre_index, bio_df))
eqn1 <- paste0("$r=", round(sqrt(wj_towre$r.squared),sigdig), ", p", format.pval(wj_towre$coefficients[2,4], digits = sigdig),"$")

# How many participants were in each group?
group_count <- table(bio_df$group)
sex_count <- table(demo_df$group, demo_df$gender)


# Was there a relationship between age and group?
age_x_grp <- kruskal.test(age_at_testing ~ group, demo_df)
age_x_rd <- summary(lm(age_at_testing ~ read, demo_df))


# Look at ADHD diagnoses
num_subj <- nrow(demo_df)
num_adhd <- sum(demo_df$adhd_dx)
adhd_grp <- table(demo_df$group, demo_df$adhd_dx)
adhd_x_grp <- kruskal.test(adhd_dx ~ group, demo_df)


############## PRINT THE WHOLE DEMOGRAPHICS SECTION #################################

p1 <- "Here, we present analyses of task performance where reading skill is treated as either a continuous or discrete group variable. We have previously argued that reading skill is best modeled as a continuous variable, in agreement with {REFS} that there is no clear demarcation between readers who are below-average and readers who are dyslexic. Our results on phoneme categorization published thus far {REFS} confirm an entirely continuous relationship between task performance and reading skill. However, for completeness and ease of comparison with existing literature on dyslexia, we also provide group-level analyses. "
p2 <- paste0("Reading skill was summarised in a composite variable: as both the WJ-BRS and the TOWRE index are scored on the same standardized scale, a composite reading skill measure was created by averaging the two metrics for each participant. Using a composite of both measures as our criterion improved the confidence of our group assignments since they are highly correlated measures in our sample (", eqn1, ").")

p3 <- paste0("There were ", group_count[2], " subjects in the Dyslexic group (",sex_count[2,1], " male) and ",
             group_count[1], " subjects in the Control group (", sex_count[1,1], " male).")

p4 <- paste0("There was not a significant difference in age between groups (Kruskal-Wallis rank sum test, $H(", age_x_grp$parameter, ")=",
             round(age_x_grp$statistic, sigdig), ", p=", round(age_x_grp$p.value,sigdig),
             "$), nor was there a significant correlation between age and reading score ($r = ",
             round(sqrt(age_x_rd$r.squared), sigdig), ", p = ", format.pval(age_x_rd$coefficients[2,4], digits = sigdig), "$).")
p5 <- "We did not exclude participants with ADHD diagnoses from the study because ADHD is highly comorbid with dyslexia [Germano et al 2010]. Instead, we accounted for ADHD diagnosis as a covariate in our statistical analyses."
p6 <-  paste0("Of ", num_subj, " participants, ", num_adhd," had a formal diagnosis of ADHD: ",
              adhd_grp[2,2]," in the Dyslexic group and ", adhd_grp[1,2]," in the Control group.")

p7 <- paste0("The difference in prevalence of ADHD across groups was not significant ($H(",
             adhd_x_grp$parameter,") = ", round(adhd_x_grp$statistic, sigdig),", p=",
             format.pval(adhd_x_grp$p.value, digits = sigdig),"$).")



demo_section <- paste(p1,p2,p3,p4,p5,p6,p7)
demo_section



#########################################################################################################################################################
############ PRINT TABLES ###################################################
# Within each group, get the mean of each value
mu <- aggregate(.~group, bio_df, mean, na.action = na.omit)


t_mu <- transpose(mu[,-c(1,2)])
colnames(t_mu) <- mu[,1]
rownames(t_mu) <- colnames(mu[,-c(1,2)])


sigma <- aggregate(.~group, bio_df, sd, na.action = na.omit)
t_sd <- transpose(sigma[,-c(1,2)])
colnames(t_sd) <- sigma[,1]


##### Now we have to add together these two so that a parenthetical appears after 
datArray <- round(abind(t_mu,t_sd,along=3),1)
formatted_array <- apply(datArray,1:2,function(x)paste0(x[1]," (",x[2], ")"))



# WASI 
p_fs2 <- wilcox.test(wasi_fs2 ~ group, bio_df)$p.value
p_mr <-wilcox.test(wasi_mr_ts ~ group, bio_df)$p.value


# WJ-BRS
p_wjbrs<-wilcox.test(wj_brs ~ group, bio_df)$p.value
p_lwid <-wilcox.test(wj_lwid_ss ~ group, bio_df)$p.value
p_wass <-wilcox.test(wj_wa_ss ~ group, bio_df)$p.value

# TOWRE
p_twre <- wilcox.test(twre_index ~ group, bio_df)$p.value
p_swe <- wilcox.test(twre_swe_ss ~ group, bio_df)$p.value
p_pde <- wilcox.test(twre_pde_ss ~ group, bio_df)$p.value


# CTOPP
p_pa <-wilcox.test(ctopp_pa~ group, bio_df)$p.value
p_pm <-wilcox.test(ctopp_pm~ group, bio_df)$p.value
p_ran <-wilcox.test(ctopp_rapid~ group, bio_df)$p.value


p_table <- c(p_fs2, p_mr, p_wjbrs, p_lwid, p_wass,p_twre,
                   p_swe, p_pde, p_pa, p_pm, p_ran)

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


p_table <- pvalr(p_table)
big_array <- cbind(formatted_array, p_table)

# Demographic array
line1 <- c(paste0("$n=$", group_count[1]), paste0("$n=$",group_count[2]), "")
line2 <- c(paste0(sex_count[1,1], " male, ", sex_count[1,2], " female"),
           paste0(sex_count[2,1], " male, ", sex_count[2,2], " female"),
           "")

header <- rbind(line1,line2)
big_array <- rbind(header, big_array)

# Insert some break rows for 
x <- rep("", ncol(big_array))
big_array <- rbind(big_array[1:2,],x, big_array[3:nrow(big_array),])
big_array <- rbind(big_array[1:5,], x, big_array[6:nrow(big_array),])
big_array <- rbind(big_array[1:9,], x, big_array[10:nrow(big_array),])
big_array <- rbind(big_array[1:13,], x, big_array[14:nrow(big_array),])

# Format the rownames
row.names(big_array) <- c("", "",
                          "\\textbf{WASI-III}", "FS-2", "Nonverbal IQ", 
                          "\\textbf{Woodcock-Johnson IV}", "Basic Reading Score", "Nonword", "Real Word",
                          "\\textbf{TOWRE 2}","TOWRE Index", "Nonword","Real Word",
                          "\\textbf{CTOPP 2}", "Phonological Awareness","Phonological Memory",
                          "Rapid Naming")
columns <- c("Control",
             "Dyslexic",
             "Significance"
)


# Some table options
options(knitr.table.format = "latex")
kable(big_array, booktabs = TRUE, linesep = "", escape = FALSE,
      col.names = linebreak(columns, align = "c"))

