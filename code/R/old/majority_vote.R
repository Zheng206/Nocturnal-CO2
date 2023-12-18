library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(readxl)
source("./code/co2_function.R")

args <- commandArgs(trailingOnly = TRUE)
random_type = args[1]
result_type = args[2]

subject_summary = read_csv("./data/qc_data/subject_summary_20230510.csv")
#subject_summary$day = as.character(subject_summary$day)
features_df = read_csv("./data/qc_data/extracted_wavelet_features_20230510.csv")
colnames(features_df) = sapply(colnames(features_df), function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
#features_df$day = as.character(features_df$day)
k_means_df = subject_summary %>% na.omit() %>% left_join(features_df, by = c("subject", "day"))
k_means_df= k_means_df[,apply(k_means_df, 2, function(col) { length(unique(col)) > 1 })]
features = colnames(k_means_df)[which(grepl("level", colnames(k_means_df)))]

# Exclude records that has less than 4 hour time points
info = read_csv("./data/data_management/data_info/info.csv") %>% na.omit() %>% filter(period == ">= 4 hours") %>% dplyr::select(subject, visit, day)
info$day = as.Date(as.POSIXct(info$day, format = "%m/%d/%y"))
k_means_df = info %>% dplyr::select(subject, day) %>% left_join(k_means_df, by = c("subject", "day"))

# Majority Vote Clustering
true_performance = majority_vote(k_means_df)
mv_df = majority_vote(k_means_df, rt = TRUE)
for (col in c("cluster", "cluster_night", "cluster_visit")){
  mv_df[[col]] = sapply(mv_df[[col]], function(x){
    case_when(x  == "Group A" ~ "abnormal",
              x  == "Group C" ~ "normal",
              x  == "Group A/C" ~ "abnormal")
  }, USE.NAMES = FALSE)
}
write_csv(mv_df, "./data/cluster/hierarchical_clustering_results.csv")
# Simulation
if(random_type == "rn"){
  if(result_type == "mixed_model"){
## Random Night
    sig_result_table = lapply(1:10000, function(i){
      result = majority_vote(simulation_test(k_means_df, bs = FALSE))
      sig_table = result$mixed_model_result %>% filter(!is.na(sig)) %>% mutate(sim = i)
      return(sig_table)
    }) %>% bind_rows()
    write_csv(sig_result_table, "./data/simulation/mixed_model_result_rn.csv")
  }else if(result_type == "agree"){
    mip_agree_result_rn = replicate(
      10000,
      expr = majority_vote(simulation_test(k_means_df, bs = FALSE))$mip_agree_rate
    )
    low.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.05)
    mean.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.5)
    high.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.95)
    mip_agree_result = data.frame(cbind(low.ci.mip, mean.ci.mip, high.ci.mip))
    colnames(mip_agree_result) = c("Low.CI", "Median", "Heigh.CI")
    standard_agree_result_rn = replicate(
      10000,
      expr = majority_vote(simulation_test(k_means_df, bs = FALSE))$standard_agree_rate
    )
    low.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.05)
    mean.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.5)
    high.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.95)
    standard_agree_result = data.frame(cbind(low.ci.standard, mean.ci.standard, high.ci.standard))
    colnames(standard_agree_result) = c("Low.CI", "Median", "Heigh.CI")
    agree_result = rbind(mip_agree_result, standard_agree_result)
    write_csv(agree_result, "./data/simulation/agree_result_rn.csv")
  }
}else if(random_type == "bs"){
## Bootstrapping
  if(result_type == "mixed_model"){
    sig_result_table = lapply(1:10000, function(i){
      result = majority_vote(simulation_test(k_means_df))
      sig_table = result$mixed_model_result %>% filter(!is.na(sig)) %>% mutate(sim = i)
      return(sig_table)
    }) %>% bind_rows()
    write_csv(sig_result_table, "./data/simulation/mixed_model_result_bs.csv")
  }else if(result_type == "agree"){
    mip_agree_result_rn = replicate(
      10000,
      expr = majority_vote(simulation_test(k_means_df))$mip_agree_rate
    )
    low.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.05)
    mean.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.5)
    high.ci.mip = quantile(sort(mip_agree_result_rn), probs = 0.95)
    mip_agree_result = data.frame(cbind(low.ci.mip, mean.ci.mip, high.ci.mip))
    colnames(mip_agree_result) = c("Low.CI", "Median", "Heigh.CI")
    standard_agree_result_rn = replicate(
      10000,
      expr = majority_vote(simulation_test(k_means_df))$standard_agree_rate
    )
    low.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.05)
    mean.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.5)
    high.ci.standard = quantile(sort(standard_agree_result_rn), probs = 0.95)
    standard_agree_result = data.frame(cbind(low.ci.standard, mean.ci.standard, high.ci.standard))
    colnames(standard_agree_result) = c("Low.CI", "Median", "Heigh.CI")
    agree_result = rbind(mip_agree_result, standard_agree_result)
    write_csv(agree_result, "./data/simulation/agree_result_bs.csv")
  }
}





