library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(readxl)
library(pROC)
source("./code/co2_function.R")

subject_summary = read_csv("./data/qc_data/subject_summary_20230510.csv")
#subject_summary$day = as.character(subject_summary$day)
features_df = read_csv("./data/qc_data/extracted_wavelet_features_20230510.csv")
colnames(features_df) = sapply(colnames(features_df), function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
#features_df$day = as.character(features_df$day)
k_means_df = subject_summary %>% na.omit() %>% left_join(features_df, by = c("subject", "day"))
k_means_df= k_means_df[,apply(k_means_df, 2, function(col) { length(unique(col)) > 1 })]
features = colnames(k_means_df)[which(grepl("level", colnames(k_means_df)))]
features = features[which(!grepl("*_13$|*_12$|*_11$|*_10$", features))]
# True Label Generation
## Use both MIP and ALSFRS scores
### Median MIP ALSFRS 8
k_means_df$standard_label = sapply(1:nrow(k_means_df), function(i){
  label = case_when(k_means_df$mip[i] < median(k_means_df$mip) & (k_means_df$alsfrs_10[i] + k_means_df$alsfrs_11[i]) == 8 ~ "normal",
                    .default = "abnormal")
  return(label)
}, USE.NAMES = FALSE)
### Median MIP
k_means_df$mip_label = sapply(1:nrow(k_means_df), function(i){
  label = case_when(k_means_df$mip[i] < median(k_means_df$mip) ~ "normal",
                    .default = "abnormal")
  return(label)
}, USE.NAMES = FALSE)

#a = co2_cluster(k_means_df, features, 0.1, 0.3)
true_df = best_thre(k_means_df, features)
set.seed(123456)
sample_1 = best_thre(simulation_test(k_means_df), features)
sample_2 = best_thre(simulation_test(k_means_df), features)


# Night-level Label
mat = as.matrix(k_means_df[features]) 
hclust.out = hclust(dist(mat))
k_means_df$cluster = cutree(hclust.out, k = 3)
k_means_df$cluster = as.factor(k_means_df$cluster)
k_means_df$cluster = sapply(k_means_df$cluster, function(x){case_when(x == "2" ~ 1, 
                                                                      x == "3" ~ 0,
                                                                      x == "1" ~ 1)}, USE.NAMES = FALSE)
k_means_df$cluster_night = k_means_df$cluster

# Visit-level Label
k_means_df[c("subject", "visit", "day", "cluster_night")]
visit_cluster = k_means_df %>% dplyr::select(subject, visit, day, cluster_night) %>% group_by(subject, visit) %>% summarize(cluster_visit = mean(cluster_night)) %>% ungroup()
k_means_df = k_means_df %>% left_join(visit_cluster, by = c("subject", "visit"))

b = visit_cluster %>% left_join(k_means_df %>% dplyr::select(subject, visit, standard_label, mip_label), by = c("subject", "visit"))
b$cluster_enco1 = sapply(b$cluster_visit, function(x) case_when(x>0.5~1,
                                                                .default = x), USE.NAMES = FALSE)
roc(b$mip_label, b$cluster_visit)
roc(b$standard_label, b$cluster_visit)
roc(b$standard_label, b$cluster_enco1)

# Patient-level Label
patient_level = k_means_df %>% dplyr::select(subject, visit, standard_label, mip_label) %>% 
  mutate(standard_label = case_when(standard_label == "abnormal" ~ 1,
                                    .default = 0),
         mip_label = case_when(mip_label == "abnormal" ~ 1,
                               .default = 0)) %>% group_by(subject) %>% summarize(standard_label = case_when(mean(standard_label) >= 0.5 ~ "abnormal",
                                                                                                             .default = "normal"), 
                                                                                  mip_label = case_when(mean(mip_label) >= 0.5 ~ "abnormal",
                                                                                                            .default = "normal")) %>% left_join(visit_cluster %>% group_by(subject) %>% summarize(cluster = mean(cluster_visit),
                                                                                                                                                                                                  cluster_enco1 = case_when(cluster > 0.5 ~ 1,
                                                                                                                                                                                                                            .default = cluster)), by = c("subject"))
roc(patient_level$mip_label, patient_level$cluster)
roc(patient_level$mip_label, patient_level$cluster_enco1)
roc(patient_level$standard_label, patient_level$cluster)
roc(b$standard_label, b$cluster_enco1)

k_means_df$cluster = sapply(k_means_df$subject, function(x) assign_cluster(x, k = 3, k_means_df), USE.NAMES = FALSE)



k_means_df$cluster_night = sapply(k_means_df$cluster_night, function(x){
  case_when(x == "1" ~ "Group A",
            x == "2" ~ "Group B",
            x == "3" ~ "Group C",
            x == "1/3" ~ "Group A/C",
            x == "1/2" ~ "Group A/B",
            x == "2/3" ~ "Group B/C")
}, USE.NAMES = FALSE)

k_means_df$cluster_visit = sapply(k_means_df$cluster_visit, function(x){
  case_when(x == "1" ~ "Group A",
            x == "2" ~ "Group B",
            x == "3" ~ "Group C",
            x == "1/3" ~ "Group A/C",
            x == "1/2" ~ "Group A/B",
            x == "2/3" ~ "Group B/C")
}, USE.NAMES = FALSE)







k_means_df$standard_label = sapply(1:nrow(k_means_df), function(i){
  label = case_when(k_means_df$mip[i] < median(k_means_df$mip) & (k_means_df$alsfrs_10[i] + k_means_df$alsfrs_11[i]) == 8 ~ "normal",
                    .default = "abnormal")
  return(label)
}, USE.NAMES = FALSE)
k_means_df[c("cluster","standard_label")] %>% group_by(standard_label, cluster) %>% summarize(count = n())
k_means_df[c("cluster","mip_label")] %>% group_by(mip_label, cluster) %>% summarize(count = n())
