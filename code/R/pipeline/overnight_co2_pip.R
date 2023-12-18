library(dplyr)
library(stringr)
library(tidyverse)
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
library(glmmLasso)
library(Rwave)
library(kableExtra)
library(wavethresh)
library(survival)
library(ggsurvfit)
source("./code/R/pipeline/co2_function.R")

# Data Management

total_df = list.files("./data/data_management/analysis_data", full.names = TRUE) %>% read_csv() %>% bind_rows()
subject_summary = list.files("./data/data_management/summary_data", full.names = TRUE) %>% read_csv() %>% bind_rows()
features_df = list.files("./data/data_management/wavelet_features", full.names = TRUE) %>% read_csv() %>% bind_rows()
input_index_df = list.files("./data/data_management/input_index", full.names = TRUE) %>% read_csv() %>% bind_rows()
df_info = read_csv("./data/data_management/data_info/info_1206.csv")
frequency_table = list.files("./data/data_management/frequency_table", full.names = TRUE) %>% read_csv() %>% bind_rows()
used_record = df_info %>% filter(DC == "DC", period == ">= 4 hours") %>% dplyr::select(subject, day)
total_df = total_df %>% semi_join(used_record, by = c("subject", "day"))
subject_summary = subject_summary %>% semi_join(used_record, by = c("subject", "day"))
features_df = features_df %>% semi_join(used_record, by = c("subject", "day"))

write_csv(total_df, "./code/co2_app/data/data_stream/total_df.csv")
write_csv(features_df, "./code/co2_app/data/wavelet/features_df.csv")
write_csv(subject_summary, "./code/co2_app/data/wavelet/subject_summary.csv")
write_csv(frequency_table, "./code/co2_app/data/wavelet/frequency_table.csv")
write_csv(input_index_df, "./code/co2_app/data/input_index/input_index.csv")
#frequency_table = read_csv("./data/frequency_table.csv")
#frequency_table_mix = subject_summary[c("subject", "day")] %>% left_join(frequency_table, by = c("subject", "day"))
#index_df = read_csv("./data/inpute_missing_data_index.csv")

summary_info = read_excel("./data/MDA_DATA/MDA_DATA_V3_11.21.23.xls") %>% dplyr::select("PDF_Filename", "date_visit", "date_onset") %>% 
  filter(grepl("HYP|MDA", PDF_Filename)) %>% na.omit()
colnames(summary_info) = c("filename", "period",  "date_onset") 
summary_info$subject = sapply(summary_info$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
summary_info$day  = as.character(summary_info$period)
summary_info$visit = sapply(summary_info$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)

# Wavelet Module Data
wave_features = colnames(features_df)[-c(1:2)]
wave_features = sapply(wave_features, function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
colnames(features_df) = c(colnames(features_df)[1:2], wave_features)

# Mixed Model Data
mix_df = subject_summary %>% left_join(features_df, by = c("subject", "day")) %>% na.omit() %>% group_by(subject, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() 
mix_df= mix_df[,apply(mix_df, 2, function(col) { length(unique(col)) > 1 })]
wave_features = colnames(mix_df)[grepl("_level_", colnames(mix_df))]
wave_features = wave_features[which(!grepl("*_13$|*_12$|*_11$|*_10$", wave_features))]
mix_df[wave_features] = scale(mix_df[wave_features], center = TRUE, scale = TRUE)
mix_df$subject = as.factor(mix_df$subject)
#write_csv(mix_df, "./code/co2_app/data/wavelet/mix_df.csv")

PCO2_thre = c(45, 50)
outcomes = c("fvc", "mip")
input_df = expand_grid(outcomes, PCO2_thre)
# Raw Feature Significance
raw_sig_df = lapply(1:nrow(input_df), function(i){mix_model(input_df[[i, "outcomes"]], input_df[[i, "PCO2_thre"]], mix_df)[[1]]}) %>% bind_rows()
# Wavelet Feature Significance
lambda = seq(500, 250, by = -5)
lambda = seq(250, 100, by = -10)
best_lambda_fvc = lambda_tunning(lambda = lambda, outcome = "fvc", df = mix_df, features = wave_features, type = "cv")
best_lambda_mip = lambda_tunning(lambda = lambda, outcome = "mip", df = mix_df, features = wave_features, type = "cv")

result_table = lapply(outcomes, function(w){
  if (w == "fvc"){thre = 165}else{thre = 290}
  #if (w == "fvc"){thre = 135}else{thre = 290}
  mix_model(w, thre = 50, mix_df, type = "wavelet", lambda = thre, features = wave_features)[1]
}) %>% bind_rows()

### PCA

#pca_comp = prcomp(mix_df[wave_features], scale. = TRUE)
#pc_df = data.frame(pca_comp$x)[c(1:25)]
#pca_df = cbind(mix_df[c(1:34)], pc_df)
#glm_mip = lmerTest::lmer(as.formula(paste0("mip ~ ", paste0("PC", 1:25, collapse = " + "), " + (1 | subject)")), data = pca_df)
#glm_mip %>% tidy() %>% filter(effect == "fixed")


# Hierarchical Clustering

# Night-level Label
k_means_df = subject_summary %>% left_join(features_df, by = c("subject", "day")) %>% na.omit()
#raw_features = colnames(k_means_df)[c(4:5,11:16, 18:23, 32:35)]
mat = as.matrix(k_means_df[wave_features]) 
#mat = as.matrix(k_means_df[c(wave_features,raw_features)]) 
hclust.out = hclust(dist(mat))
k_means_df$cluster = cutree(hclust.out, k = 4)
ggplot(k_means_df, aes(x = as.factor(cluster), y = mip, fill = as.factor(cluster))) +
  geom_boxplot() +
  geom_jitter() +
  #scale_x_discrete(labels = c("Normal", "Abnormal")) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Cluster", y = "MIP", fill = "Cluster") +
  theme(legend.position = "none")

k_means_df$cluster_night = sapply(k_means_df$cluster, function(x){case_when(x == 1 ~ 1, 
                                                                            x == 2 ~ 1,
                                                                            x == 3 ~ 1,
                                                                            x == 4 ~ 0)}, USE.NAMES = FALSE)

t.test(k_means_df$mip[which(k_means_df$cluster_night == 1)], k_means_df$mip[which(k_means_df$cluster_night == 0)], alternative = "two.sided")


ggplot(k_means_df, aes(x = as.factor(cluster_night), y = mip, fill = as.factor(cluster_night))) +
  geom_boxplot() +
  geom_jitter() +
  scale_x_discrete(labels = c("Normal", "Abnormal")) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Cluster", y = "MIP", fill = "Cluster") +
  theme(legend.position = "none")

event_summary = k_means_df %>% group_by(as.character(cluster_night)) %>% summarize(event_45 = mean(event_45),
                                                                   event_50 = mean(event_50),
                                                                   event_45.10 = mean(event_45.10),
                                                                   event_50.10 = mean(event_50.10),
                                                                   event_spo2 = mean(event_spo2),
                                                                   delta_pco2_event = mean(delta_pco2_event),
                                                                   delta_pco2_50_event = mean(delta_pco2_50_event)) %>% t() %>% data.frame() 
event_summary = event_summary[2:nrow(event_summary),]
colnames(event_summary) = c("normal", "abnormal")
event_summary = event_summary %>% mutate(normal = sprintf("%.3f", as.numeric(normal)),
                                       abnormal = sprintf("%.3f", as.numeric(abnormal)))
event_summary = as_tibble(event_summary, rownames = "event")
time_summary = k_means_df %>% group_by(as.character(cluster_night)) %>% summarize(PCO2.45 = mean(PCO2.45),
                                                                                  PCO2.50 = mean(PCO2.50),
                                                                                  SpO2.88 = mean(SpO2.88),
                                                                                  PCO2.45.10 = mean(PCO2.45.10),
                                                                                  PCO2.50.10 = mean(PCO2.50.10),
                                                                                  delta_pco2_time = mean(delta_pco2_time),
                                                                                  delta_pco2_50_time = mean(delta_pco2_50_time)) %>% t() %>% data.frame()
time_summary = time_summary[2:nrow(time_summary),]
colnames(time_summary) = c("normal", "abnormal")
time_summary = time_summary %>% mutate(normal = sprintf("%.3f", as.numeric(normal)),
                                       abnormal = sprintf("%.3f", as.numeric(abnormal)))
time_summary = as_tibble(time_summary, rownames = "period")

write_csv(event_summary, "./code/co2_app/data/abnormal_summary/event_summary.csv")
write_csv(time_summary, "./code/co2_app/data/abnormal_summary/time_summary.csv")

# Visit-level Label
visit_cluster = k_means_df %>% dplyr::select(subject, visit, day, cluster_night) %>% group_by(subject, visit) %>% summarize(cluster_visit = mean(cluster_night)) %>% ungroup()
visit_cluster$cluster_visit = sapply(visit_cluster$cluster_visit, function(x) case_when(x>=0.5~1,
                                                                       .default = 0), USE.NAMES = FALSE)
visit_cluster = visit_cluster %>% left_join(k_means_df %>% dplyr::select(subject, visit, mip, fvc), by = c("subject", "visit")) %>% distinct()
t.test(visit_cluster$mip[which(visit_cluster$cluster_visit == 1)], visit_cluster$mip[which(visit_cluster$cluster_visit == 0)], alternative = "two.sided")


ggplot(visit_cluster, aes(x = as.factor(cluster_visit), y = mip, fill = as.factor(cluster_visit))) +
  geom_boxplot() +
  geom_jitter() +
  scale_x_discrete(labels = c("Normal", "Abnormal")) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Cluster", y = "MIP", fill = "Cluster") +
  theme(legend.position = "none")

k_means_df  = k_means_df %>% left_join(visit_cluster %>% dplyr::select(subject, visit, cluster_visit), by = c("subject", "visit"))
# Patient-level Label

patient_cluster = k_means_df %>% dplyr::select(subject, visit, day, cluster_night) %>% group_by(subject) %>% summarize(cluster_patient = mean(cluster_night)) %>% ungroup()
patient_cluster$cluster_patient = sapply(patient_cluster$cluster_patient, function(x) case_when(x>=0.5~1,
                                                                                        .default = 0), USE.NAMES = FALSE)
patient_cluster =patient_cluster %>% left_join(k_means_df %>% dplyr::select(subject, visit, mip, fvc) %>% group_by(subject) %>% summarize(mip = mean(mip), fvc = mean(fvc)), by = c("subject")) %>% distinct()
ggplot(patient_cluster, aes(x = as.factor(cluster_patient), y = mip, fill = as.factor(cluster_patient))) +
  geom_boxplot() +
  geom_jitter() +
  scale_x_discrete(labels = c("Normal", "Abnormal")) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Cluster", y = "MIP", fill = "Cluster") +
  theme(legend.position = "none")

k_means_df  = k_means_df %>% left_join(patient_cluster %>% dplyr::select(subject, cluster_patient), by = c("subject"))

k_means_df = k_means_df %>% mutate(cluster_night = case_when(cluster_night == 1~"Abnormal",
                                                             .default = "Normal"),
                                   cluster_visit = case_when(cluster_visit == 1~"Abnormal",
                                                             .default = "Normal"),
                                   cluster_patient = case_when(cluster_patient == 1~"Abnormal",
                                                               .default = "Normal"))

write_csv(k_means_df, "./code/co2_app/data/clustering/k_means_df.csv")

# Kaplan Meier Curve
## preparing km dataset
km_df = k_means_df %>% left_join(summary_info %>% dplyr::select(subject, date_onset), by = c("subject")) 
km_df$day = as_datetime(km_df$day)
visit_time_df = lapply(1:nrow(visit_cluster), function(i){
  sub_df = k_means_df %>% filter(subject == visit_cluster[[i, "subject"]], visit == visit_cluster[[i, "visit"]]) %>% arrange(day) %>% head(1) %>% dplyr::select(subject, visit, day)
  colnames(sub_df) = c("subject", "visit", "period")
  return(sub_df)
}) %>% bind_rows()
km_df = km_df %>% left_join(visit_time_df, by = c("subject", "visit"))
km_df$period = as_datetime(km_df$period)
km_df = km_df %>% dplyr::select(subject, day, date_onset, period, event_45, event_50.10, event_spo2, SpO2.88, delta_pco2_event, delta_pco2_50_event, fvc, mip, cluster_visit) %>% 
  mutate(fvc_50 = case_when(fvc < 50 ~ 1,
                            .default = 0),
         mip_60 = case_when(mip > -60 ~ 1,
                            .default = 0),
         pco2_45 = case_when(event_45 > 0 ~ 1,
                             .default = 0),
         spo2_88 = case_when(event_spo2 > 0 ~ 1,
                             .default = 0),
         pco2_50.10 = case_when(event_50.10 > 0 ~ 1,
                                .default = 0),
         delta_pco2_event = case_when(delta_pco2_event > 0 ~ 1,
                                      .default = 0),
         delta_pco2_50_event = case_when(delta_pco2_50_event > 0 ~ 1,
                                         .default = 0),
         nocturnal_test = case_when(cluster_visit == "Abnormal" ~ 1,
                                    .default = 0),
         time_sd = period - date_onset,
         time_co2 = day - date_onset)

sd_df = km_df %>% dplyr::select(subject, fvc, mip, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% distinct()
sd_df = sd_df %>% dplyr::select(subject, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% pivot_longer(c(2:4), names_to = "test", values_to = "event") %>% rename(time = time_sd)

pco2_df = km_df %>% dplyr::select(subject, pco2_45, spo2_88, pco2_50.10, delta_pco2_event, delta_pco2_50_event, time_co2, cluster_visit) %>% distinct()
pco2_df = pco2_df %>% pivot_longer(c(2:6), names_to = "test", values_to = "event") %>% rename(time = time_co2)
test_df =rbind(sd_df, pco2_df) 
write_csv(test_df, "./code/co2_app/data/kaplan_mierer/kaplan_mierer.csv")
#write_csv(test_df, "./data/kaplan-meier/kaplan_meier_20230510.csv")

test_df$test = factor(test_df$test, levels = c("fvc_50", "mip_60", "pco2_45", "spo2_88", "pco2_50.10", "delta_pco2_event", "delta_pco2_50_event"),
                      labels = c("FVC < 50%", "MIP < 60cm H20", "Nocturnal CO2 >45mmHg for >5mins", "SpO2 ≤88% for ≥5mins", 
                                 "Nocturnal CO2 >50mmHg for >10mins", "nocturnal CO2 increase >10 mmHg from baseline CO2 for >10 mins", 
                                 "nocturnal CO2 increase >10 mmHg from baseline to above >50 mmHg for >10 mins"))

survfit2(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("FVC < 50%", "MIP < 60cm H20", "Nocturnal CO2 >45mmHg for >5mins", "SpO2 ≤88% for ≥5mins"))) %>% 
  ggsurvfit(linetype_aes = TRUE) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset (days)",
    y = "Free of abnormal test"
  ) +
  theme(legend.position = "bottom") +
  add_confidence_interval() +
  add_risktable()





## Subset Data for other use
visit_reorder = function(subject_summary, s){
  sub_df = subject_summary %>% filter(subject == s) %>% arrange(day) %>% 
    group_by(subject, visit) %>% summarize(count = n()) 
  sub_df = sub_df %>% mutate(visit_ord = paste0("Visit ", 1:nrow(sub_df))) %>% dplyr::select(subject, visit, visit_ord)
  return(sub_df)
}

subject_id = unique(subject_summary$subject)
co2_features = colnames(subject_summary)[c(4:5, 11:16, 18:23, 28, 32:35)]
visit_table = lapply(unique(subject_summary$subject), function(x) visit_reorder(subject_summary, x)) %>% bind_rows() %>% ungroup()
co2_features_df = subject_summary %>% left_join(visit_table, by = c("subject", "visit")) %>% dplyr::select(c("subject", "visit_ord", "day", co2_features)) %>% 
  group_by(subject, visit_ord) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() 
write_csv(co2_features_df, "~/Desktop/co2_features.csv")
co2_features_df = co2_features_df %>% pivot_longer(c(3:21), names_to = "PCO2_feature", values_to = "Measurements") %>% pivot_wider(names_from = visit_ord, values_from = c(3,4))
