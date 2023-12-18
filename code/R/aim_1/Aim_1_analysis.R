library(tidyverse)
library(readxl)
source("./code/R/co2_function.R")

# Generate predictor and outcome dataset
mda = read_excel("./data/MDA_DATA/MDA_DATA_V3_11.21.23.xls")
mda_ad = read_excel("./data/MDA_DATA/MDA_DATA_V3.xls")
mda_ad = mda_ad %>% dplyr::select("PDF_Filename", "date_visit", "Days since onset at visit")
summary_df = mda %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "onset_region___1", "onset_region___2", "onset_region___3", "alsfrs_10", "alsfrs_total", "niv_date", "trach_date", "death_date", "visit_age") %>% filter(grepl("HYP|MDA", PDF_Filename))
summary_df  = summary_df %>% left_join(mda_ad, by = c("PDF_Filename", "date_visit"))
colnames(summary_df) = c("filename", "date_visit", "fvc", "onset_region___1", "onset_region___2", "onset_region___3", "alsfrs_10", "alsfrs_total", "niv_date", "trach_date", "death_date", "visit_age", "diagnosis_delay")

summary_df$subject = sapply(summary_df$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
summary_df$visit = sapply(summary_df$date_visit, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)

onset_region = summary_df %>% group_by(subject) %>% summarize(onset_region_1 = sum(na.omit(onset_region___1)),
                                               onset_region_2 = sum(na.omit(onset_region___2)),
                                               onset_region_3 = sum(na.omit(onset_region___3))) %>% 
  mutate(onset_region = case_when(onset_region_1 == 1 ~ "limb",
                                  onset_region_2 == 1 ~ "bulbar",
                                  onset_region_3 == 1 ~ "limb")) %>% mutate(onset_region_limb = case_when(onset_region == "limb" ~ 1,
                                                                                                          .default = 0)) %>% dplyr::select(subject, onset_region_limb)

summary_df = summary_df %>% left_join(onset_region, by = "subject") %>% dplyr::select("subject", "visit", "onset_region_limb", "alsfrs_10", "alsfrs_total", "fvc", "niv_date", "trach_date", "death_date", "diagnosis_delay", "visit_age")
summary_df = summary_df %>% mutate(fvc_outcome = case_when(fvc < 50 ~ 1,
                                                           .default = 0))
subject = unique(summary_df$subject)
age = sapply(subject, function(x) summary_df %>% filter(subject == x) %>% head(1) %>% pull(visit_age), USE.NAMES = FALSE)
age_df = data.frame(cbind(subject, age))
age_df$age = as.numeric(age_df$age)
summary_df = summary_df %>% left_join(age_df, by = "subject") %>% dplyr::select("subject", "visit", "onset_region_limb", "alsfrs_10", "alsfrs_total", "fvc", "niv_date", "trach_date", "death_date", "diagnosis_delay", "age", "fvc_outcome")

# Generate PCO2 related variables
#summary_info = read_excel("./data/MDA_DATA/MDA_DATA_V3_11.21.23.xls")
#summary_info = summary_info %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset") %>% filter(grepl("HYP|MDA", PDF_Filename))
#colnames(summary_info) = c("filename", "period", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset")
#
#summary_info$subject = sapply(summary_info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
#  item = str_split(x,"_")[[1]]
#  upenn = item[which(grepl("UPENN", item))]
#  upenn = str_sub(upenn, start = 1, end = 7)
#  return(upenn)
#}}, USE.NAMES = FALSE)
#summary_info$day  = as.character(summary_info$period)
#
#other_info = summary_info %>% dplyr::select(subject, day, fvc, mip, spot_co2, alsfrs_10, alsfrs_11, alsfrs_total) %>% na.omit()
#other_info$visit = sapply(other_info$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
#
#total_df = list.files("./data/data_management/analysis_data", full.names = TRUE) %>% read_csv() %>% bind_rows()
#pco2_summary = subject_summary_gen(total_df, other_info)

# or simply readin
#pco2_feature = pco2_summary[c(1:5, 11:16, 18:23, 26, 29, 31:35)]
pco2_summary = read_csv("./code/co2_app/data/wavelet/subject_summary.csv")
pco2_feature = pco2_summary[c(1:2, 4:5, 11:16, 18:23, 26, 29, 31:35)]
pco2_feature = pco2_feature %>% group_by(subject, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup()
pco2_feature_df = pco2_feature %>% left_join(summary_df %>% dplyr::select(subject, visit, onset_region_limb, diagnosis_delay, age, fvc_outcome) %>% na.omit(), by = c("subject", "visit"))
pco2_feature_df[which(!complete.cases(pco2_feature_df)),]
write_csv(pco2_feature_df, "./data/aim1/predictors_1206.csv")


# PCA
pco2_feature_name = colnames(pco2_feature_df)[3:17]
pca_comp = prcomp(pco2_feature_df[pco2_feature_name], scale. = TRUE)
#summary(pca_comp)
## chose 7 PCs
pc_df = data.frame(pca_comp$x)[c(1:6)]
pca_df = cbind(pco2_feature_df[c(1:2, 18:20, 25:28)], pc_df)

model = lmerTest::lmer(fvc_outcome ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + (1 | subject), data = pca_df)
pco2_table = model %>% tidy() %>% filter(p.value < 0.05) %>% dplyr::select(term, estimate, std.error, statistic, df, p.value)
variable_matrix = as.matrix(cbind(pco2_feature_df[c(18, 20, 23, 25:27)], pc_df))
M = cor(variable_matrix)
corrplot(M, method = 'color', order = 'alphabet')
