library(wavethresh)
library(parallel)
library(dplyr)
library(stringr)
library(tidyverse)
library(glue)
library(readxl)
library(ggplot2)
library(rlang)
library(DT)
library(broom)
library(ggfortify)
library(kableExtra)
library(corrplot)
library(car)
library(openxlsx)
library(lme4)
library(broom.mixed)
setwd("/home/zhengren/Desktop/Project/overnight_co2")
source("/home/zhengren/Desktop/Project/overnight_co2/code/co2_function.R")

# Data Management
total_df = read_csv("./data/data_management/raw_data/overnight_data_all_subjects_20231129.csv")

# Subject Summary Info
summary_info = read_excel("./data/Subject datasets/MDA_DATA/MDA_DATA_V3_11.21.23.xls")
summary_info = summary_info %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset") %>% filter(grepl("HYP|MDA", PDF_Filename))
colnames(summary_info) = c("filename", "period", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "alsfrs_total", "date_onset")

summary_info$subject = sapply(summary_info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
  }}, USE.NAMES = FALSE)
summary_info$day  = as.character(summary_info$period)

other_info = summary_info %>% dplyr::select(subject, day, fvc, mip, spot_co2, alsfrs_10, alsfrs_11, alsfrs_total) %>% na.omit()
other_info$visit = sapply(other_info$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)

# Raw Feature Extraction
## readin data
miss_summary = read_csv("./data/qc_data/missing_summary/missing_summary_20231129.csv")
non_miss = miss_summary %>% filter(PCO2_status == "No") %>% dplyr::select(subject, day)
non_miss_df = lapply(1:nrow(non_miss), function(i){
  sub_df = total_df %>% filter(subject == non_miss[[i, "subject"]], day == non_miss[[i, "day"]])
  return(sub_df)
}) %>% bind_rows()
write_csv(non_miss_df, "./data/qc_data/non_missing_data/non_missing_df_20231206.csv")

non_miss_summary = subject_summary_gen(df = non_miss_df, summary_df = other_info)
#write_csv(non_miss_summary, "./data/data_management/non_missing_summary_sen.csv")

missing_inpute = read_csv("./data/qc_data/input_data/inpute_missing_data_20231206.csv")
total_subject = rbind(non_miss_df, missing_inpute)
write_csv(total_subject, "./data/qc_data/total_df/total_df_20231206.csv")
subject_summary = subject_summary_gen(total_subject, other_info)
write_csv(subject_summary, "./data/data_management/summary_data/subject_summary_20231206.csv")


# Wavelet Feature Extraction

# Read-in data
source("/home/zhengren/Desktop/Project/overnight_co2/code/wavelet.R")
total_subject_df = total_subject
total_subject_df$day = as.character(total_subject_df$day)
total_subject_input = total_subject_df %>% group_by(subject, day) %>% summarize(count = n()) %>% ungroup()
total_subject_input$power = sapply(total_subject_input$count, function(x) floor(log2(x)), USE.NAMES = FALSE)
total_subject_input = total_subject_input %>% filter(power >= 13)

# Wavelet Family Selection
#waveletFamily = list(c("DaubExPhase",1),
#                      c("DaubExPhase",2),
#                      c("DaubExPhase",3),
#                      c("DaubExPhase",4),
#                      c("DaubExPhase",5),
#                      c("DaubExPhase",6),
#                      c("DaubExPhase",7),
#                      c("DaubExPhase",8),
#                      c("DaubExPhase",9),
#                      c("DaubExPhase",10),
#                      c("DaubLeAsymm",4),
#                      c("DaubLeAsymm",5),
#                      c("DaubLeAsymm",6),
#                      c("DaubLeAsymm",7),
#                      c("DaubLeAsymm",8),
#                      c("DaubLeAsymm",9),
#                      c("DaubLeAsymm",10),
#                      c("Coiflets", 1),
#                      c("Coiflets", 2),
#                      c("Coiflets", 3),
#                      c("Coiflets", 4),
#                      c("Coiflets", 5),
#                      c("LinaMayrand", 3.1),
#                      c("LinaMayrand", 4.1),
#                      c("LinaMayrand", 5.1),
#                      c("LinaMayrand", 5.2),
#                      c("LinaMayrand", 5.3),
#                      c("LinaMayrand", 5.4))

#total_subject_input$family_1 = sapply(1:nrow(total_subject_input), function(i){
#  wavelet_pre(total_subject_df, total_subject_input, i, 1, "PCO2_DC")}, USE.NAMES = FALSE)

#total_subject_input$family_2 = sapply(1:nrow(total_subject_input), function(i){
#  wavelet_pre(total_subject_df, total_subject_input, i, 2, "PCO2_DC")}, USE.NAMES = FALSE)
#
#total_subject_input$family_3 = sapply(1:nrow(total_subject_input), function(i){
#  wavelet_pre(total_subject_df, total_subject_input, i, 3, "PCO2_DC")}, USE.NAMES = FALSE)
#
#family = c(total_subject_input$family_1, total_subject_input$family_2, total_subject_input$family_3)
#data.frame(table(family)) %>% arrange(desc(Freq))

feature_df = lapply(seq_len(nrow(total_subject_input)), function(i){
    feature_extraction(total_subject_df, total_subject_input, i)
}) %>% bind_rows()

write_csv(feature_df, "./data/qc_data/wavelet_features/extracted_wavelet_features_20231206.csv")

#########STOP######










subject_summary = total_df %>% na.omit() %>% group_by(subject, visit, day) %>% summarize(mean_PCO2 = mean(PCO2_DC), sd_PCO2 = sd(PCO2_DC), mean_SpO2 = mean(SpO2), sd_SpO2 = sd(SpO2), mean_PR = mean(PR), sd_PR = sd(PR), time_length = n()) %>% ungroup()

subject_summary$PCO2.45 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "time")}, USE.NAMES = FALSE) 

subject_summary$PCO2.50 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "time")}, USE.NAMES = FALSE)

subject_summary$SpO2.88 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 88, total_df, type = "SpO2", return_type = "time")}, USE.NAMES = FALSE)


subject_summary = subject_summary %>% mutate(
  PCO2.45 = PCO2.45/time_length,
  PCO2.50 = PCO2.50/time_length,
  SpO2.88 = SpO2.88/time_length)

subject_summary$event_45 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "event")}, USE.NAMES = FALSE)

subject_summary$event_50 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "event")}, USE.NAMES = FALSE)


subject_summary$event_spo2 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 88, total_df, type = "SpO2", return_type = "event")}, USE.NAMES = FALSE)

#subject_summary$delta = sapply(1:nrow(subject_summary), function(i){
  #delta = delta_cal(subject_summary[[i,"subject"]], subject_summary[[i,"visit"]], subject_summary[[i,"day"]], spot_check_df, total_df)
  #return(delta)
#}, USE.NAMES = FALSE)

#measures = c("PCO2_DC", "SpO2", "PR")
max_pco2_sd = lapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "PCO2_DC", total_df, period = 60)) %>% bind_rows()
colnames(max_pco2_sd) = c("subject", "day", "max_sd_pco2", "sd_start_pco2", "sd_end_pco2")

max_spo2_sd = lapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "SpO2", total_df, period = 60)) %>% bind_rows()
colnames(max_spo2_sd) = c("subject", "day", "max_sd_spo2", "sd_start_spo2", "sd_end_spo2")

max_pr_sd = lapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "PR", total_df, period = 60)) %>% bind_rows()
colnames(max_pr_sd) = c("subject", "day", "max_sd_pr", "sd_start_pr", "sd_end_pr")

subject_summary = subject_summary %>% left_join(max_pco2_sd, by = c("subject", "day")) %>% left_join(max_spo2_sd, by = c("subject", "day")) %>% left_join(max_pr_sd, by = c("subject", "day"))
family = str_split(datasets[w],"_")[[1]][length(str_split(datasets[w],"_")[[1]])]

#subject_summary$max_pco2_sd = sapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "PCO2_DC", total_df), USE.NAMES = FALSE)

#subject_summary$max_spo2_sd = sapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "SpO2", total_df), USE.NAMES = FALSE)

#subject_summary$max_pr_sd = sapply(1:nrow(subject_summary), function(i) rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "PR", total_df), USE.NAMES = FALSE)
write_csv(subject_summary, paste0("./data/subject_summary_data_60min_", family))

for (i in 1:nrow(subject_summary)){
  print(paste0(subject_summary$subject[i], ",", subject_summary$day[i]))
  rolling_sd(subject_summary[[i,"subject"]], subject_summary[[i,"day"]], "PCO2_DC", total_df, period = 60)
}



# Frequency Conversion
features_df = read_csv("./data/qc_data/wavelet_features/extracted_wavelet_features_20231206.csv")
feature_input = features_df %>% dplyr::select(subject, day)
n_level = seq(1,13,1)

for (n in n_level){
  cols = sapply(1:nrow(feature_input), function(i){ 
    a = frequency_conversion(total_df, feature_input[[i, "subject"]], feature_input[[i, "day"]], n)
    return(a)}, USE.NAMES = FALSE)
  feature_input = cbind(feature_input, cols)
}
colnames(feature_input) = c("subject", "day", paste0("level_", n_level))
write_csv(feature_input, "./data/qc_data/frequency_table/frequency_table_20231206.csv")
