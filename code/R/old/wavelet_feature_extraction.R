library(wavethresh)
library(parallel)
library(dplyr)
library(stringr)
library(tidyverse)


# Read-in data
source("./code/co2_function.R")
total_subject_df = read_csv("./data/qc_data/total_subject_20230510.csv")
total_subject_df$day = as.character(total_subject_df$day)
total_subject_input = total_subject_df %>% group_by(subject, day) %>% summarize(count = n()) %>% ungroup()
total_subject_input$power = sapply(total_subject_input$count, function(x) floor(log2(x)), USE.NAMES = FALSE)
total_subject_input = total_subject_input %>% filter(power > 10)

# feature extraction
feature_df = mclapply(1:nrow(total_subject_input), function(i){
  feature_extraction(total_subject_df, total_subject_input, i)
}, mc.cores = detectCores()) %>% bind_rows()
#write_csv(feature_df, "./data/qc_data/extracted_wavelet_features.csv")
write_csv(feature_df, "./data/qc_data/extracted_wavelet_features_20230510.csv")

