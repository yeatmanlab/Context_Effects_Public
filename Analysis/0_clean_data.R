# Process data
#!/usr/bin/env Rscript

## prep_data.R
## loads, cleans, aggregates, and saves raw response data and psychometric fits.

library(dplyr)
setwd("/home/eobrien/bde/Projects/Speech_dist")

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
#setwd("..")

## load the raw response data
response_df <- data.frame()
data_dir <- file.path("Results", "Raw")
raw_files <- list.files(path=data_dir)
## keep only categorization data (not discrimination); remove practice blocks
## and pilot data (subject "nnn")
raw_files <- raw_files[!grepl("practice", raw_files)]
raw_files <- raw_files[!grepl("nnn", raw_files)]
raw_files <- raw_files[!grepl("Pilot", raw_files)]
raw_files <- raw_files[!grepl("Plots", raw_files)]
## read in remaining raw data files
opts <- options(warn=2)  # convert warnings to errors, while reading in files
for (fname in raw_files) {
  ## skip=1 because first row of each file is a timestamp
  df_tmp <- tryCatch(read.csv(file.path(data_dir, fname), skip=1),
                     error=function(e) {print(paste("skipping", fname, 
                                                    conditionMessage(e))); e})
  if(inherits(df_tmp, "error")) next
  ## only keep complete blocks
  if(dim(df_tmp)[1] == 210) {
    df_tmp$subject_id <- strsplit(fname, "_")[[1]][1]
    df_tmp$stimulus <- as.character(df_tmp$stimulus)
    df_tmp$step <- sapply(df_tmp$stimulus,
                          function(i) strsplit(strsplit(i, "_")[[1]][4],
                                               ".", fixed=TRUE)[[1]][1])
    df_tmp$response <- ifelse(df_tmp$selection %in% c("Da"), 1, 0)
    
    ## get the timestamp
    conn <- file(file.path(data_dir, fname), "r")
    df_tmp$psych_date <- strsplit(readLines(conn, 1), ",")[[1]][1]
    close(conn)
    ## concatenate with other files
    response_df <- rbind(response_df, df_tmp)
  } else {
    print(paste("skipping", fname, "(incomplete block)"))
  }
}
options(opts)  # restore default options


# Raw files for how many subjects found?
print(paste("There are ", length(unique(response_df$subject_id)), "subjects."))

## load the repository / registry data
repository_df <- read.csv("RDRPRepository_DATA_2019-09-19_1553.csv")
registry_df <- read.csv("RDRPRegistry_DATA_2019-09-19_1554.csv")
demog_df <- merge(repository_df, registry_df, by = "record_id")
demog_df$sid.x <- as.numeric(gsub("\\D", "", demog_df$sid.x))

## get reading scores
names <- colnames(demog_df)
wj_cols <- c("wj_brs","wj_wa_ss","wj_lwid_ss")
ctopp_cols <- c("ctopp_pa","ctopp_rapid","ctopp_pm")
twre_cols <- c("twre_index","twre_pde_ss","twre_swe_ss")
wasi_cols <- c("wasi_fs2", "wasi_mr_ts")
reading_columns <- c("record_id", "visit_dt","dys_dx", "adhd_dx", "brain_injury", "aud_dis",
                     "psych_dx", wj_cols, ctopp_cols, twre_cols, wasi_cols)
reading_df <- demog_df %>% dplyr::select(reading_columns)

df_bool <- reading_df %>%
  dplyr::select(c("record_id","dys_dx","adhd_dx","aud_dis","psych_dx")) %>%
  na.omit(.)

reading_df <- reading_df[!duplicated(reading_df),]
reading_df$ctopp_rapid <- as.numeric(as.character(reading_df$ctopp_rapid))
reading_df$ctopp_rapid <- sapply(reading_df$ctopp_rapid, function(f){is.na(f)<-which(f == '');f}) 


#### Some date handling ##############
########################################################################
# Get the day that each subject came in on
date_df <- response_df %>%
  dplyr::select(c("subject_id","psych_date")) %>%
  mutate(subject_id = as.numeric(gsub("\\D", "", subject_id))) %>%
  unique(.) %>%
  mutate(record_id = subject_id)

# Get the birthdate of each subject
bday <- demog_df %>%
  dplyr::select(c("record_id","dob")) %>%
  unique(.) %>%
  subset(dob != "")
date_df <- merge(date_df, bday)
date_df$psych_age <-  with(date_df, difftime(psych_date, dob, units="weeks"))

# Make a 
visit_df <- reading_df %>% 
  dplyr::select(c("record_id","visit_dt")) %>%
  subset(visit_dt != "")

date_df <- merge(date_df, visit_df, by = "record_id")
date_df$visit_age <-  with(date_df, difftime(visit_dt, dob, units="weeks"))

reading_df <- merge(date_df, reading_df)
reading_df$elapsed <- reading_df$visit_age - reading_df$psych_age

#############################################
# Get the most recent reading score for each subject #
subj_list = unique(reading_df$record_id)
big_out = data.frame()
for (s in subj_list){
  sub <- reading_df %>% subset(record_id == s)
  vars <- names(sub)[14:length(names(sub))-1]
  
  out = data.frame(record_id = s)
  for( v in vars ){
    opts = sub %>% dplyr::select(c(v,"elapsed")) %>%
      na.omit(.) %>% 
      mutate(elapsed = as.numeric(elapsed))
    opts = opts[order(abs(opts$elapsed)),]
    fetched = opts[1,] %>% dplyr::select(v)
    out = cbind(out, fetched)
  }
  out$psych_date = sub$psych_date[1]
  big_out = rbind(big_out, out)
}

big_out2 <- merge(big_out, df_bool, by = "record_id")

## biographic details
bio_df <- demog_df[c("sid.x", "dob", "record_id","gender")]
bio_df[bio_df==""] <- NA
bio_df <- na.omit(bio_df)
bio_df$dob <- as.POSIXct(bio_df$dob, format="%Y-%m-%d")
colnames(bio_df)[colnames(bio_df) == "sid.x"] <- "subject_id"

## merge biographic info, reading scores, and psychometric data
use_df <- merge(bio_df, big_out2)
# Merge with response data
use_df <- merge(use_df, response_df, by = "subject_id", all.x = FALSE)
use_df <- use_df[!duplicated(use_df),]

## compute age at testing
use_df$age_at_testing <- with(use_df, difftime(psych_date.x, dob, units="weeks"))
use_df$age_at_testing <- as.numeric(use_df$age_at_testing) / 52.25

# Filter out any subjects who do not meet our criteria for IQ/auditory disorder
use_df <- use_df %>% 
  filter(age_at_testing >= 8 & age_at_testing < 14)%>%
  filter(aud_dis == 0 | is.nan(aud_dis))  %>%                # no auditory disorder
  filter(wasi_fs2 >= 80 | is.nan(wasi_fs2)) %>%           # WASI criterion
  filter(wasi_mr_ts > 30) %>%           # WASI nonverbal not less than 2 sd below mean
  


print(paste("There are currently", length(unique(use_df$subject_id)), "subjects."))


# REMOVE ANY SUBJECTS WHO DIDN'T GET THE GAUSSIAN CONDITION FIRST
fpath <- file.path("Results", "Psychometrics", "Raw")
flist <- list.files(fpath)
raw_psych <- data.frame()
for (f in flist){
  subj_idx = as.numeric(strsplit(f, "_|\\.")[[1]][3])
  df_tmp <- read.csv(file.path(fpath, f))
  df_tmp$subject_id <- subj_idx
  raw_psych <- rbind(raw_psych, df_tmp)
}

raw_psych_G <- subset(raw_psych, first == "Gaussian")
use_df <- mutate(use_df, Gauss_first = if_else(subject_id %in% raw_psych_G$subject_id,1,0))


# 
print(paste("There are currently", length(unique(use_df$subject_id)), "subjects."))
## assign to groups
use_df$read <- (use_df$wj_brs + use_df$twre_index)*0.5
use_df$group <- with(use_df, ifelse(read<= 85, "Dyslexic",
                                    ifelse(read, "Control")))

## drop identifying information
use_df <- use_df[ , !(names(use_df) == "dob")]
write.table(use_df, file="cleaned_data.csv", sep=",", quote=FALSE, row.names=FALSE)

## ## ## ## ## ## ## ## ## ## ##
## LOAD PSYCHOMETRIC FIT DATA ##
## ## ## ## ## ## ## ## ## ## ##
#setwd("..")
fpath <- file.path("Results", "Psychometrics", "Fit")
flist <- list.files(fpath)
psychometric_df <- do.call(rbind, lapply(file.path(fpath, flist), read.csv))
## make subject_id & asymptote column names consistent
psychometric_df <- rename(psychometric_df, subject_id=SubjectID,
                          lo_asymp=guess, hi_asymp=lapse)


## na.locf is "last observation carry forward". This works because we know the
## rows of the psychometrics dataframe are loaded in groups of 3, where all 3
## rows of the CSV file are the same contrast, and "single" is the last row.

## add group and reading ability to psychometrics dataframe
columns <- c("subject_id", "group", "wj_brs","twre_index","read","adhd_dx", "wasi_mr_ts","age_at_testing","Gauss_first")
group_table <- unique(use_df[columns])

psychometric_df <- subset(psychometric_df,
                          subject_id %in% use_df$subject_id)
psychometric_df <- merge(psychometric_df, group_table, all.x=TRUE, all.y=FALSE)
psychometric_df <- subset(psychometric_df, threshold >1 && threshold < 7)
## omit subjects missing from clean_data (must have been excluded earlier)
#psychometric_df <- na.omit(psychometric_df)
## add "paradigm" column

write.table(psychometric_df, file="cleaned_psychometrics.csv", sep=",",
            quote=FALSE, row.names=FALSE)
setwd("./Analysis")

