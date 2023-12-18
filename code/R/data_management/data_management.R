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

source("./code/R/pipeline/co2_function.R")

## Compile total_df


path = list.files("./data/All subjects/20230510", recursive = TRUE, full.names = TRUE)

total_df_gen = function(path, date){
  hyp_path = path[which(grepl("HYP", path))]
  file_size = sapply(hyp_path, file.size, USE.NAMES = FALSE)
  hyp_path = hyp_path[which(file_size > 80000)]
  subject = sapply(hyp_path, info_get, info = "subject", USE.NAMES = FALSE)
  visit = sapply(hyp_path, info_get, info = "visit", USE.NAMES = FALSE)
  date = sapply(hyp_path, info_get, info = "date", USE.NAMES = FALSE)
  
  df = data.frame(cbind(subject, visit, date, hyp_path))
  
  df$DC = sapply(df$hyp_path, drift_correction, USE.NAMES = FALSE)
  df$PR = sapply(df$hyp_path, PR, USE.NAMES = FALSE)
  df = df %>% filter(DC == "DC")
  input_hy = df %>% group_by(subject, visit) %>% count() %>% dplyr::select(1:2)
  total_df = lapply(1:nrow(input_hy), subject_com_df, input = input_hy, df = df) %>% bind_rows()
  
  ##MDA_path = list.files("./Overnight data streams/All subjects", pattern = "MDA", recursive = TRUE, full.names = TRUE)
  MDA_path = path[which(grepl("MDA", path))]
  subject = sapply(MDA_path, info_get, info = "subject", USE.NAMES = FALSE)
  visit = sapply(MDA_path, info_get, info = "visit", USE.NAMES = FALSE)
  date = sapply(MDA_path, info_get, info = "date", USE.NAMES = FALSE)
  
  df_mda = data.frame(cbind(subject, visit, date, MDA_path))
  df_mda$DC = sapply(df_mda$MDA_path, drift_correction, USE.NAMES = FALSE)
  df_mda$PR = sapply(df_mda$MDA_path, PR, USE.NAMES = FALSE)
  df_mda = df_mda %>% filter(DC == "DC")
  input_mda = df_mda %>% group_by(subject, visit) %>% count() %>% dplyr::select(1:2)
  
  total_df_mda = lapply(1:nrow(input_mda), subject_com_df, input = input_mda, df = df_mda) %>% bind_rows()
  
  total_df = rbind(total_df, total_df_mda)
  return(total_df)
  #write_csv(total_df, paste0("./data/data_management/total_df/overnight_data_all_subjects_", data, ".csv"))
}

total_df = total_df_gen(new_files, "20231129")

## Check missing data
missing_summary_gen = function(total_df, date){
  input = total_df %>% group_by(file, subject, visit, day) %>% summarize(count = n()) %>% dplyr::select(file, subject, visit, day) %>% ungroup()
  
  input[["PCO2_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PCO2_DC", type = "status"), USE.NAMES = FALSE)
  input[["SpO2_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "SpO2", type = "status"), USE.NAMES = FALSE)
  input[["PR_status"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PR", type = "status"), USE.NAMES = FALSE)
  input[["PCO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PCO2_DC", type = "times"), USE.NAMES = FALSE)
  input[["SpO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "SpO2", type = "times"), USE.NAMES = FALSE)
  input[["PR_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(df = total_df, s = input[[i, "subject"]], d = input[[i, "day"]], m = "PR", type = "times"), USE.NAMES = FALSE)
  write_csv(input, paste0("./data/data_management/missing/missing_summary_", date, ".csv"))
  return(input)
}
missing_summary_df = missing_summary_gen(total_df = total_df, date = "20231129")

  
### Compile subject summary dataset
#summary_info = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
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
## readin data
#total_df = read_csv("./data/data_management/total_df/overnight_data_all_subjects_20230510.csv")
#miss_summary = read_csv("./data/data_management/missing/missing_summary_20230510.csv")
#non_miss = miss_summary %>% filter(PCO2_status == "No") %>% dplyr::select(subject, day)
#non_miss_df = lapply(1:nrow(non_miss), function(i){
#  sub_df = total_df %>% filter(subject == non_miss[[i, "subject"]], day == non_miss[[i, "day"]])
#  return(sub_df)
#}) %>% bind_rows()
#write_csv(non_miss_df, "./data/qc_data/non_missing_df_20230510.csv")
#
#non_miss_summary = subject_summary_gen(non_miss_df, other_info)
##write_csv(non_miss_summary, "./data/data_management/non_missing_summary_sen.csv")
#
#missing_inpute = read_csv("./data/qc_data/inpute_missing_data.csv")
#total_subject = rbind(non_miss_df, missing_inpute)
#write_csv(total_subject, "./data/qc_data/total_subject_20230510.csv")
#
#subject_summary = subject_summary_gen(total_subject, other_info)
#write_csv(subject_summary, "./data/qc_data/subject_summary_20230510.csv")



