library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(readxl)
source("./code/R/pipeline/co2_function.R")

path = list.files("./data/All subjects/20231127", recursive = TRUE, full.names = TRUE)

# HYP
hyp_path = path[which(grepl("HYP", path))]
file_size = sapply(hyp_path, file.size, USE.NAMES = FALSE)
hyp_path = hyp_path[which(file_size > 80000)]
subject = sapply(hyp_path, info_get, info = "subject", USE.NAMES = FALSE)
visit = sapply(hyp_path, info_get, info = "visit", USE.NAMES = FALSE)
date = sapply(hyp_path, info_get, info = "date", USE.NAMES = FALSE)
df = data.frame(cbind(subject, visit, date, hyp_path))
df$DC = sapply(df$hyp_path, drift_correction, USE.NAMES = FALSE)
df$PR = sapply(df$hyp_path, PR, USE.NAMES = FALSE)
df$period = sapply(df$hyp_path, time_length, USE.NAMES = FALSE)
colnames(df) = c("subject", "visit", "date", "path", "DC", "PR", "period")

# MDA
MDA_path = path[which(grepl("MDA", path))]
subject = sapply(MDA_path, info_get, info = "subject", USE.NAMES = FALSE)
visit = sapply(MDA_path, info_get, info = "visit", USE.NAMES = FALSE)
date = sapply(MDA_path, info_get, info = "date", USE.NAMES = FALSE)
mda_df = data.frame(cbind(subject, visit, date, MDA_path))
mda_df$DC = sapply(mda_df$MDA_path, drift_correction, USE.NAMES = FALSE)
mda_df$PR = sapply(mda_df$MDA_path, PR, USE.NAMES = FALSE)
mda_df$period = sapply(mda_df$MDA_path, time_length, USE.NAMES = FALSE)
colnames(mda_df) = c("subject", "visit", "date", "path", "DC", "PR", "period")
df = rbind(df, mda_df)
df$day = sapply(df$path, function(x){
  sub_df = read_excel(x)
  t =convertToDate(sub_df$Time[nrow(sub_df)])
  return(as.character(t))}, USE.NAMES = FALSE)

df = df %>% mutate(path = basename(path)) %>% dplyr::select("subject", "visit", "day", "path", "DC", "period") %>% rename(file = path)
df[which(df$subject == "HYP09" & df$visit == "07"), "visit"] = "06"
df[which(df$subject == "HYP11" & df$visit == "04"), "visit"] = "03"
df[which(df$subject == "UPENN02" & df$visit == "10"), "visit"] = "09"

# Sample Size Summary
N = nrow(df)
N_visits = df %>% group_by(subject, visit) %>% summarize(count = n()) %>% ungroup() %>% nrow()
n_dc_4 = nrow(df %>% filter(DC == "DC", period == ">= 4 hours"))
n_dc_4_visit = df %>% filter(DC == "DC", period == ">= 4 hours") %>% group_by(subject, visit) %>% summarize(count = n()) %>% ungroup() %>% nrow() 

subject_summary = list.files("./data/data_management/summary_data", full.names = TRUE, recursive = TRUE) %>% read_csv()
subject_summary$day = as.character(subject_summary$day)
df_info = df %>% dplyr::select(subject, visit, day, file, DC, period) %>% left_join(subject_summary, by = c("subject", "visit", "day"))
#df_info %>% na.omit() %>% filter(DC == "DC", period == ">= 4 hours")
write_csv(df_info, "./data/data_management/data_info/info_1206.csv")

#df_sample = df_info %>% na.omit() %>% filter(DC == "DC", period == ">= 4 hours")
sample_size_summary = sample_info(df_info)
sample_size_summary_ex_4hour = sample_info(df_sample)
write_csv(sample_size_summary, "./data/data_management/data_info/data_summary_20231206.csv")
write_csv(sample_size_summary_ex_4hour, "./data/data_management/data_info/records_info_exclude_4hour.csv")

####STOP HERE######

# Prelim Data Table
subject_summary = read_csv("./data/cluster/hierarchical_clustering_results.csv")
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
info = info %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "spot_co2",	"spot_o2", "alsfrs_10", "alsfrs_11", "alsfrs_total", "sex", "visit_age") %>% 
  filter(grepl("HYP|MDA", PDF_Filename)) %>% na.omit()
colnames(info) = c("filename", "period", "fvc", "mip",  "spot_co2",	"spot_o2", "alsfrs_10", "alsfrs_11", "alsfrs_total", "sex", "age")

info$subject = sapply(info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
info$day  = as.character(info$period)

subject_summary = subject_summary %>% left_join(info %>% dplyr::select(subject, sex, age), by = "subject")

# Visit-level Table 2
visit_summary = subject_summary %>% dplyr::select(subject, day, visit, event_45, event_45.10, delta_pco2_50_event, fvc, event_spo2_less_5) %>% 
  mutate(fvc_event = case_when(fvc > 50 ~ 1,
                               .default = 0)) %>% 
  group_by(subject, visit) %>% 
  summarize(event_45 = sum(event_45),
            event_45.10 = sum(event_45.10),
            delta_pco2_50_event = sum(delta_pco2_50_event), 
            fvc_event = sum(fvc_event),
            event_spo2_less_5 = sum(event_spo2_less_5))

# Follow up time
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_08.04.23.xls")
info = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
info = info %>% dplyr::select("PDF_Filename", "date_visit", "Analysis Period") %>% filter(grepl("HYP|MDA", PDF_Filename)) 
colnames(info) = c("filename", "period", "ap")

info$subject = sapply(info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)

info_start = na.omit(info[c("subject", "period")])
info_start$period = as.Date(info_start$period)
info_end = na.omit(info[c("subject", "ap")])
info_end$ap = sapply(info_end$ap, function(x) str_split(str_split(x, " - ")[[1]][2], " ")[[1]][1], USE.NAMES = FALSE)
info_end$ap = sapply(info_end$ap, function(x) as.character(as.Date(x, format = "%d-%b-%Y")), USE.NAMES = FALSE)
info_end$ap = as.Date(info_end$ap) 

end = info_end %>% filter(subject %in% unique(subject_summary$subject)) %>% group_by(subject) %>% summarize(end_date = tail(ap, n = 1)) %>% ungroup()
start = info_start %>% filter(subject %in% unique(subject_summary$subject)) %>% group_by(subject) %>% summarize(start_date = head(period, n = 1)) %>% ungroup()
date_df = start %>% left_join(end, by = "subject") %>% mutate(follow_up_time = end_date - start_date)

median(date_df$follow_up_time)
quantile(date_df$follow_up_time)

date_df_end = subject_summary %>% group_by(subject) %>% summarize(end_date = head(day, n = 1)) %>% ungroup()
date_df_tail = subject_summary %>% group_by(subject) %>% summarize(start_date = tail(day, n = 1)) %>% ungroup()
date_df %>% left_join(date_df_tail, by = c("subject")) %>% mutate(follow_up_time = end_date - start_date)
follow_up_time = lapply(unique(date_df$subject), function(x){
  sub_df = date_df %>% filter(subject == x) %>% mutate(time = date - lag(date)) %>% na.omit()
}) %>% bind_rows()

median(follow_up_time$time)
IQR(follow_up_time$time)
