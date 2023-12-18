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
library(glmmLasso)
library(Rwave)
library(igraph)
library(visNetwork)
library(network)
library(shiny)
library(shinythemes)
library(kableExtra)
library(wavethresh)
library(mathjaxr)
library(bslib)
library(bsicons)
library(rsconnect)
library(survival)
library(ggsurvfit)
source("./co2_function.R")


#patients = c("HYP01", "HYP02", "HYP04", "HYP06", "HYP07", "HYP08", "HYP09", 
#             "HYP10", "HYP11", "HYP12", "HYP13", "UPENN01", "UPENN04")
#
#patient_list = list(
#  "HYP01" = c("2022-05-26", "2022-05-27", "2022-05-28", "2022-09-08", "2022-09-09", 
#              "2022-09-10", "2023-01-05", "2023-01-07", "2023-04-13", "2023-04-15"),
#  "HYP02" = c("2022-10-28", "2023-02-09", "2023-02-12"),
#  "HYP04" = c("2022-07-21", "2022-07-22", "2023-03-03", "2023-03-04", "2023-03-05"),
#  "HYP06" = c("2022-08-02","2022-08-03", "2022-08-04"),
#  "HYP07" = c("2022-08-11", "2022-08-12", "2022-08-13", "2022-12-07", "2022-12-08"),
#  "HYP08" = c("2022-08-11", "2022-08-12", "2022-08-13", "2022-12-08", "2022-12-09", "2022-12-10",
#              "2023-03-30", "2023-03-31", "2023-04-01"),
#  "HYP09" = c("2022-08-17", "2022-08-18", "2022-08-19", "2022-12-19", "2022-12-21"),
#  "HYP10" = c("2022-08-25", "2022-08-27", "2022-12-15"),
#  "HYP11" = c("2022-09-21", "2022-09-22"),
#  "HYP12" = c("2022-10-18", "2022-10-19", "2022-10-20"),
#  "HYP13" = c("2023-03-02", "2023-03-03", "2023-03-04"),
#  "UPENN01" = c("2022-09-19", "2022-09-20", "2022-12-16", "2022-12-18", "2023-03-18", 
#                "2023-03-19", "2023-03-20"),
#  "UPENN04" = c("2023-02-23", "2023-02-24", "2023-02-25")
#)
#
#
#patient_v = c("HYP01", "HYP02", "HYP04", "HYP06", "HYP07", "HYP08", "HYP09", "HYP10", "HYP11",  
#              "HYP12", "HYP13", "UPENN01", "UPENN04")

total_df = read_csv("./data/data_stream/total_df.csv")
k_means_df = read_csv("./data/clustering/k_means_df.csv")
patients = unique(k_means_df$subject)
patient_list = vector(mode = "list", length = length(patients))
names(patient_list) = patients
for (p in patients){
  patient_list[p] = list(k_means_df %>% filter(subject == p) %>% pull(day) %>% as.character()) 
}

subject_summary = read_csv("./data/wavelet/subject_summary.csv")
features_df = read_csv("./data/wavelet/features_df.csv")
frequency_table = read_csv("./data/wavelet/frequency_table.csv")
frequency_table_mix = k_means_df[c("subject", "day")] %>% left_join(frequency_table, by = c("subject", "day"))
index_df = read_csv("./data/input_index/input_index.csv")
mix_df = read_csv("./data/wavelet/mix_df.csv")

# Wavelet Module Data

wave_features = colnames(features_df)[-c(1:2)]
wave_features = sapply(wave_features, function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
colnames(features_df) = c(colnames(features_df)[1:2], wave_features)

# Mixed Model Data
mix_df = subject_summary %>% left_join(features_df, by = c("subject", "day")) %>% na.omit() %>% group_by(subject, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() 
mix_df= mix_df[,apply(mix_df, 2, function(col) { length(unique(col)) > 1 })]
wavelet_features = colnames(mix_df)[grepl("_level_", colnames(mix_df))]
wavelet_features = wavelet_features[which(!grepl("*_13$|*_12$|*_11$|*_10$", wavelet_features))]
mix_df[wavelet_features] = scale(mix_df[wavelet_features], center = TRUE, scale = TRUE)
mix_df$subject = as.factor(mix_df$subject)

PCO2_thre = c(45, 50)
outcomes = c("fvc", "mip")
input_df = expand_grid(outcomes, PCO2_thre)
input_df = input_df %>% mutate(lambda = case_when(outcomes == "fvc" ~ 165,
                                                  outcomes == "mip" ~ 290))
# Hierarchical Clustering

alsfrs_input = expand_grid(type = c("alsfrs_10", "alsfrs_11"), subject_status = c("Abnormal", "Normal"))

alsfrs_df = lapply(1:nrow(alsfrs_input), function(i){
  df = data.frame(table(k_means_df %>% filter(cluster_patient == alsfrs_input[[i, "subject_status"]]) %>% pull(alsfrs_input[[i, "type"]]))) 
  colnames(df) = c("alsfrs", "count")
  df$subject_status = alsfrs_input[[i, "subject_status"]]
  df$type = alsfrs_input[[i, "type"]]
  return(df)}) %>% bind_rows()
alsfrs_df$alsfrs = factor(alsfrs_df$alsfrs, levels = c("0", "1", "2", "3", "4"))

#k_means_df = subject_summary %>% dplyr::select(c(1:33, 331:335)) 
k_means_df = lapply(unique(k_means_df$subject), function(x){
  sub_df = k_means_df %>% filter(subject == x) %>% arrange(day) 
  sub_df = sub_df %>% mutate(visit_time = as.numeric(day - sub_df$day[1]), .after = visit)
  return(sub_df)
}) %>% bind_rows()
num_df = k_means_df %>% group_by(subject, visit) %>% summarize(across(where(is.numeric), mean)) %>% ungroup()
cha_df = k_means_df %>% dplyr::select(subject, visit, cluster_night, cluster_visit, cluster_patient) %>% distinct()
cluster_df = num_df %>% left_join(cha_df, by = c("subject", "visit"))
p_ex = cluster_df %>% group_by(subject) %>% summarize(count = n()) %>% filter(count == 1) %>% pull(subject)
subject_cluster_df = k_means_df[c("subject", "cluster_patient")] %>% distinct() %>% arrange(cluster_patient)
#
#evl_mip_df = cluster_df[c("cluster_visit", "mip_label")] %>% mutate(evaluation = case_when(mip_label == "abnormal" & cluster_visit == mip_label ~ "TP",
#                                                                                                mip_label == "normal" & cluster_visit == mip_label ~ "TN",
#                                                                                                mip_label == "abnormal" & cluster_visit != mip_label ~ "FN",
#                                                                                                mip_label == "normal" & cluster_visit != mip_label ~ "FP")) %>% group_by(evaluation) %>% summarize(count = n())
#
#sensitivity_mip = evl_mip_df[[which(evl_mip_df$evaluation == "TP"), "count"]]/(evl_mip_df[[which(evl_mip_df$evaluation == "TP"), "count"]] + evl_mip_df[[which(evl_mip_df$evaluation == "FN"), "count"]])                                                                                              
#specificity_mip = evl_mip_df[[which(evl_mip_df$evaluation == "TN"), "count"]]/(evl_mip_df[[which(evl_mip_df$evaluation == "TN"), "count"]] + evl_mip_df[[which(evl_mip_df$evaluation == "FP"), "count"]]) 
#evl_standard_df = cluster_df[c("cluster_visit", "standard_label")] %>% mutate(evaluation = case_when(standard_label == "abnormal" & cluster_visit == standard_label ~ "TP",
#                                                                                                     standard_label == "normal" & cluster_visit == standard_label ~ "TN",
#                                                                                                     standard_label == "abnormal" & cluster_visit != standard_label ~ "FN",
#                                                                                                     standard_label == "normal" & cluster_visit != standard_label ~ "FP")) %>% group_by(evaluation) %>% summarize(count = n())
#
#sensitivity_standard = evl_standard_df[[which(evl_standard_df$evaluation == "TP"), "count"]]/(evl_standard_df[[which(evl_standard_df$evaluation == "TP"), "count"]] + evl_standard_df[[which(evl_standard_df$evaluation == "FN"), "count"]])                                                                                              
#specificity_standard = evl_standard_df[[which(evl_standard_df$evaluation == "TN"), "count"]]/(evl_standard_df[[which(evl_standard_df$evaluation == "TN"), "count"]] + evl_standard_df[[which(evl_standard_df$evaluation == "FP"), "count"]])  

# Simulation Preparation
#sim_summary = read_csv("./data/subject_summary_20230510.csv")
#colnames(features_df) = sapply(colnames(features_df), function(x) gsub(" ", "_", x), USE.NAMES = FALSE)
#sim_df = sim_summary %>% na.omit() %>% left_join(features_df, by = c("subject", "day"))
#sim_df= sim_df[,apply(sim_df, 2, function(col) { length(unique(col)) > 1 })]
#features = colnames(sim_df)[which(grepl("level", colnames(sim_df)))]
#info = read_csv("./data/info.csv") %>% na.omit() %>% filter(period == ">= 4 hours") %>% dplyr::select(subject, visit, day)
#info$day = as.Date(as.POSIXct(info$day, format = "%m/%d/%y"))
#sim_df = info %>% dplyr::select(subject, day) %>% left_join(sim_df, by = c("subject", "day"))

feature_illustration = colnames(k_means_df)[c(5:10, 12:29, 33:36)]

# Subject level test
sig_result_subject = lapply(feature_illustration, function(y){
  cluster_df = k_means_df %>% group_by(subject) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup()
  cluster_df = cluster_df %>% left_join(k_means_df %>% dplyr::select(subject, cluster_patient) %>% distinct(), by = c("subject"))
  sig_df = cluster_test(y, cluster_df, label = "cluster_patient", type = "subject")
  return(sig_df)
}) %>% bind_rows() 
sig_result_subject$p.adj = p.adjust(as.numeric(sig_result_subject$p.value), n = length(sig_result_subject$p.value), method = "fdr")
sig_result_table_subject = sig_result_subject %>% mutate(p.value = sprintf("%.3f", p.value),
                                                         p.adj = sprintf("%.3f", p.adj),
                                                         sig.adj = case_when(p.adj < 0.1 & p.adj >= 0.05 ~ ".",
                                                                                p.adj < 0.05 & p.adj >= 0.01 ~ "*",
                                                                                p.adj < 0.01 & p.adj >= 0.001 ~ "**",
                                                                                p.adj < 0.001 ~ "***",
                                                                                .default = NA),
                                                         estimate = sprintf("%.2f", estimate))


# Visit level test
sig_result_visit = lapply(feature_illustration, function(y){
  cluster_df = k_means_df %>% group_by(subject, visit) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup()
  cluster_df = cluster_df %>% left_join(k_means_df %>% dplyr::select(subject, visit, cluster_visit) %>% distinct(), by = c("subject", "visit"))
  sig_df = cluster_test(y, cluster_df, label = "cluster_visit", type = "visit")
  return(sig_df)
}) %>% bind_rows() 
sig_result_visit$p.adj = p.adjust(as.numeric(sig_result_visit$p.value), n = length(sig_result_visit$p.value), method = "fdr")
sig_result_table_visit = sig_result_visit %>% mutate(p.value = sprintf("%.3f", p.value),
                                                     p.adj = sprintf("%.3f", p.adj),
                                                     sig.adj = case_when(p.adj < 0.1 & p.adj >= 0.05 ~ ".",
                                                                                p.adj < 0.05 & p.adj >= 0.01 ~ "*",
                                                                                p.adj < 0.01 & p.adj >= 0.001 ~ "**",
                                                                                p.adj < 0.001 ~ "***",
                                                                                .default = NA),
                                                     estimate = sprintf("%.2f", estimate))
# Night level test
sig_result_night = lapply(feature_illustration, function(y){
  sig_df = cluster_test(y, k_means_df, label = "cluster_night", type = "night")
  return(sig_df)
}) %>% bind_rows() 
sig_result_night$p.adj = p.adjust(as.numeric(sig_result_night$p.value), n = length(sig_result_night$p.value), method = "fdr")
sig_result_table_night = sig_result_night %>% mutate(p.value = sprintf("%.3f", p.value),
                                                    p.adj = sprintf("%.3f", p.adj),
                                                    sig.adj = case_when(p.adj < 0.1 & p.adj >= 0.05 ~ ".",
                                                                           p.adj < 0.05 & p.adj >= 0.01 ~ "*",
                                                                           p.adj < 0.01 & p.adj >= 0.001 ~ "**",
                                                                           p.adj < 0.001 ~ "***",
                                                                           .default = NA),
                                                    estimate = sprintf("%.2f", estimate))


test_df = read_csv("./data/kaplan_mierer/kaplan_mierer.csv")
test_df$test = factor(test_df$test, levels = c("fvc_50", "mip_60", "pco2_45", "spo2_88", "pco2_50.10", "delta_pco2_event", "delta_pco2_50_event", "nocturnal_test"),
                      labels = c("FVC < 50%", "MIP < 60cm H20", "Nocturnal CO2 >45mmHg for >5mins", "SpO2 ≤88% for ≥5mins", 
                                 "Nocturnal CO2 >50mmHg for >10mins", ">10mmHg Increase from baseline CO2 for >10mins", 
                                 ">10mmHg Increase from baseline CO2 to >50mmHg for >10mins", "Nocturnal PCO2 Test"))

event_summary = read_csv("./data/abnormal_summary/event_summary.csv") 
event_summary = event_summary %>% dplyr::select(event, abnormal, normal) %>% mutate(Notes = case_when(
  event == "event_45" ~ "PCO2 > 45 mmHg for > 5 mins",
  event == "event_50" ~ "PCO2 > 50 mmHg for > 5 mins",
  event == "event_45.10" ~ "PCO2 > 45 mmHg for > 10 mins",
  event == "event_50.10" ~ "PCO2 > 50 mmHg for > 10 mins",
  event == "event_spo2" ~ "SpO2 < 88% for > 5 mins",
  event == "delta_pco2_event" ~ "PCO2 > 10 mmHg increase from baseline for > 10 mins (cumulatively)",
  event == "delta_pco2_50_event" ~ "PCO2 > 10 mmHg increase from baseline to above 50 mmHg for > 10 mins (cumulatively)"
))
time_summary = read_csv("./data/abnormal_summary/time_summary.csv") 
time_summary = time_summary %>% dplyr::select(period, abnormal, normal) %>% mutate(Notes = case_when(
  period == "PCO2.45" ~ "PCO2 > 45 mmHg for > 5 mins",
  period == "PCO2.50" ~ "PCO2 > 50 mmHg for > 5 mins",
  period == "PCO2.45.10" ~ "PCO2 > 45 mmHg for > 10 mins",
  period == "PCO2.50.10" ~ "PCO2 > 50 mmHg for > 10 mins",
  period == "SpO2.88" ~ "SpO2 < 88% for > 5 mins",
  period == "delta_pco2_time" ~ "PCO2 > 10 mmHg increase from baseline for > 10 mins (cumulatively)",
  period == "delta_pco2_50_time" ~ "PCO2 > 10 mmHg increase from baseline to above 50 mmHg for > 10 mins (cumulatively)"
)) %>% rename("time" = "period", "abnormal" = "abnormal", "normal" = "normal") 


event_n = event_summary$Notes

function(input, output) {
  output$night_detail = renderUI({
    selectInput("night", "Select a night of interest", choices = patient_list[[input$patient]], selected = patient_list[[input$patient]][1])
  })
  output$patient_group = DT::renderDT({
    sub_df = k_means_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)) 
    group_df = sub_df %>% dplyr::select("cluster_night", "cluster_visit", "cluster_patient") %>% rename("Night.Level.Label" = "cluster_night",
                                                                                                "Visit.Level.Label" = "cluster_visit",
                                                                                                "Subject.Level.Label" = "cluster_patient") %>% pivot_longer(c(1:3), names_to = "Cluster Level", values_to = "Group")
    group_df %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                            targets = "_all")))) 
  })
  output$night_detail_wv = renderUI({
    selectInput("night_wv", "Select a night of interest", choices = patient_list[[input$patient_wv]], selected = patient_list[[input$patient_wv]][1])
  })
  output$miss_plot = renderPlot({
    sub_df = total_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)) 
    base_line = k_means_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)) %>% pull(spot_co2)
    if(length(base_line) == 0){
      base_line = NA
    }
    legend_df = data.frame(group = c("Spot CO2", "PCO2 Threshold"), yintercept = c(base_line, 50))
    ggplot() +
      geom_line(data = sub_df , aes(x = as.numeric(time), y = PCO2_DC)) +
      geom_rect(data = index_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)), mapping = aes(xmin = as.numeric(xmin), xmax = as.numeric(xmax), ymin = as.numeric(ymin), ymax = as.numeric(ymax)), fill = "red", alpha = 0.2) +
      geom_hline(data = legend_df, aes(yintercept = yintercept, color = group), linetype = "dashed") +
      labs(x = "Time", y = "PCO2") +
      guides(color = guide_legend(title = "PCO2 Levels"))
  })
  output$HZ = renderText({
    col_name = paste0("level_", input$level)
    print(paste0("Frequency Range: ", frequency_table %>% filter(subject == input$patient_wv, day == input$night_wv) %>% pull(eval(parse(text = col_name)))))
  })
  output$wave_c = renderPlot({
    sub_df = total_df %>% filter(subject == as.character(input$patient_wv), day == as.character(input$night_wv)) 
    test_night = sub_df %>% pull(PCO2_DC)
    power = floor(log2(length(test_night)))
    test_night_pad = c(test_night, rep(test_night[length(test_night)], 2^(power + 1)-length(test_night)))
    ywd = wd(family = "DaubLeAsymm",data = test_night_pad,filter.number = 7)
    par(mfrow = c(1, 2))
    C = accessC(ywd, level = as.numeric(input$level))
    D = accessD(ywd, level = as.numeric(input$level))
    if(as.numeric(input$level) >= 5){
      C = remove_pad(C)
      D = remove_pad(D)
    }
    plot.ts(C, xlab = "Coefficient Index", ylab = "Approximation Coefficients", col = "red")
    plot.ts(D, xlab = "Coefficient Index", ylab = "Detail Coefficients", col = "darkgreen")
  })
  output$raw_features = DT::renderDT({
    sub_summary = k_means_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)) 
    feature_df = sub_summary %>% dplyr::select("mean_PCO2", "sd_PCO2", "mean_delta_pct", "sd_delta_pct", "PCO2.45", "PCO2.50", "event_45", "event_50", "delta_pco2_time", "delta_pco2_event", "delta_pco2_50_time", "delta_pco2_50_event") %>% pivot_longer(c(1:12),names_to = "Raw.Features", values_to = "Statistics") %>% mutate(Statistics = round(Statistics, 3))
    feature_df %>% datatable(options = list(pageLength = 12, columnDefs = list(list(className = 'dt-center', 
                                                                   targets = "_all")))) 
  })
  output$sm = DT::renderDT({
    sub_summary = k_means_df %>% filter(subject == as.character(input$patient), day == as.character(input$night)) 
    sm_df = sub_summary %>% dplyr::select("fvc", "mip", "alsfrs_10", "alsfrs_11", "alsfrs_total", "mean_SpO2", "sd_SpO2", "event_spo2") %>% pivot_longer(c(1:8),names_to = "Standard.Measurement", values_to = "Statistics") %>% mutate(Statistics = round(Statistics, 3))
    sm_df %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                             targets = "_all")))) 
  })
  output$a_feature = DT::renderDT({
    sub_features = k_means_df %>% mutate(day = as.character(day))%>% filter(subject == as.character(input$patient_wv), day == as.character(input$night_wv)) 
    level_feature_c = wave_features[which(grepl(paste0("_c_level_",as.numeric(input$level), "$"), wave_features))]
    wv_c_df = sub_features %>% dplyr::select(level_feature_c) %>% pivot_longer(c(1:12),names_to = "Wavelet.Features", values_to = "Statistics") %>% mutate(Statistics = round(Statistics, 3))
    wv_c_df %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                               targets = "_all")))) 
  })
  output$d_feature = DT::renderDT({
    sub_features = k_means_df %>% filter(subject == as.character(input$patient_wv), day == as.character(input$night_wv)) 
    level_feature_d = wave_features[which(grepl(paste0("_d_level_",as.numeric(input$level), "$"), wave_features))]
    wv_d_df = sub_features %>% dplyr::select(level_feature_d) %>% pivot_longer(c(1:12),names_to = "Wavelet.Features", values_to = "Statistics") %>% mutate(Statistics = round(Statistics, 3))
    wv_d_df %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                targets = "_all")))) 
  })
  output$formula = DT::renderDT({
    if(input$feature_type == "raw features"){
      sig_df = lapply(1:nrow(input_df), function(i){mix_model(input_df[[i, "outcomes"]], input_df[[i, "PCO2_thre"]], mix_df)[[1]]}) %>% bind_rows()
      sig_df = sig_df[,c(1,6:7,2:5)]
      sig_df$sig = sapply(sig_df$p.value, function(x){
        case_when(x < 0.05 & x >= 0.01 ~ "*",
                  x < 0.01 & x >= 0.001 ~ "**",
                  x < 0.001 ~ "***",
                  .default = NA) 
      }, USE.NAMES = FALSE)
      datatable(sig_df %>% dplyr::select(term, result, PCO2_thre, estimate, conf.low, conf.high, p.value, sig), 
                caption = "Mixed Linear Model Summary Table - raw features",
                extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength = 13, autoWidth = TRUE,columnDefs = 
                                                         list(list(className = 'dt-center', 
                                                                   targets = "_all")), order = list(list(c(8),'desc')))) %>% formatStyle(column = c(4),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                     'sig',
                                                                     target = 'row',
                                                                     backgroundColor = styleEqual(c("*","**","***"), "pink"))
    }else{
      input_df_wv = input_df %>% dplyr::select(outcomes, lambda) %>% distinct()
      result_table = lapply(1:nrow(input_df_wv), function(i){
        mix_model(outcome = input_df_wv[[i, "outcomes"]], thre = 45, df = mix_df, type = "wavelet", lambda = input_df_wv[[i, "lambda"]], features = wavelet_features)[[1]]
      }) %>% bind_rows()
      datatable(result_table %>% dplyr::select(term, result, estimate, conf.low, conf.high, p.value, sig), 
                caption = "Mixed Linear Model Summary Table - wavelet features",
                extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength = 10, autoWidth = TRUE,columnDefs = 
                                                         list(list(className = 'dt-center', 
                                                                   targets = "_all")), order = list(list(c(7),'desc')))) %>% formatStyle(column = c(3),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                     'sig',
                                                                     target = 'row',
                                                                     backgroundColor = styleEqual(c("*","**","***"), "pink")
                                                                   ) 
      
    }
  })
  output$frequency_dis_1 = renderUI({
    if(input$feature_type == "wavelet features"){
      selectInput("level_fre", "Select the level of interest", choices = paste0("level ", seq(1,13,1)), selected = "level 13")}
  })
  #output$frequency_text_box_1 = renderUI({
  #  if(input$feature_type == "wavelet features"){
  #    textOutput("frequency_text_1")
  #  }
  #})
  output$frequency_text_box_2 = renderUI({
    if(input$feature_type == "wavelet features"){
      textOutput("frequency_text_2")
    }
  })
  output$frequency_dis_2 = renderUI({
    if(input$feature_type == "wavelet features"){
      plotOutput("frequency_plot_2")}
  })
  #output$frequency_plot_1 = renderPlot({
  #  ggplot(frequency_table_mix, aes(x = level_5)) +
  #    geom_bar() + 
  #    labs(x = "Frequency Range", y = "Count") +
  #    theme(axis.text.x = element_text(angle = 0))
  #})
  output$frequency_plot_2 = renderPlot({
    ggplot(frequency_table_mix, aes(x = eval(parse(text = gsub(" ", "_",input$level_fre))))) +
      geom_bar() + 
      labs(x = "Frequency Range", y = "Count") +
      theme(axis.text.x = element_text(angle = 0))
  })
  #output$frequency_text_1 = renderText({
  #  range = unique(frequency_table_mix$level_5)
  #  print(paste0("The frequency range of level 5 is ", paste0(str_split(range, " , ")[[1]][1], " , ", paste0(str_split(range, " , ")[[length(range)]][2]))))
  #})
  output$frequency_text_2 = renderText({
    range_13 = unique(frequency_table_mix[[gsub(" ", "_",input$level_fre)]])
    print(paste0("The frequency range of ", input$level_fre, " is ", paste0(str_split(range_13[1], " , ")[[1]][1], " , ", paste0(str_split(range_13[length(range_13)], " , ")[[1]][2]))))
  })
  output$info_type = renderUI({
    if(input$cluster_level == "Subject level"){
      DTOutput("subject_cluster")
    }else if(input$cluster_level == "Visit level"){
      selectInput("patient_v", "Select patient of interest", choices = patients, selected = patients[1])
    }else if(input$cluster_level == "Night level"){
      selectInput("notes", "Select the type of creteria", choices = event_n, selected = event_n[1])
    }
  })
  output$subject_cluster = DT::renderDT({
    subject_cluster_df %>% datatable(caption = "Subject Level Respiratory Status", options = list(columnDefs = list(list(className = 'dt-center', 
                                                                           targets = "_all")))) 
  })
  output$measure_control = renderUI({
    if (input$cluster_level == "Visit level"){
      radioButtons("measure_type", "Select the type of standard measurements", choices = c("fvc", "mip"), selected = "mip")
    }
  })
  output$v_type = renderUI({
    if(input$cluster_level == "Subject level" | input$cluster_level == "Night level"){
      plotOutput("visual_3")
    }else if(input$cluster_level == "Visit level"){
      visNetworkOutput("structure")
    }
  })
  
  output$ui_1 = renderUI({
    if(input$cluster_level == "Night level"){
      DTOutput("event_table")
    }else{
      plotOutput("visual_1")
    }
  })
  
  output$ui_2 = renderUI({
    if(input$cluster_level == "Night level"){
      DTOutput("time_table")
    }else{
      plotOutput("visual_2")
    }
  })
  
  output$visual_3 = renderPlot({
    if(input$cluster_level == "Subject level"){
      ggplot(alsfrs_df, aes(x = alsfrs, y = count, fill = subject_status)) +
        geom_bar(stat="identity") +
        labs(x = "alsfrs score", y = "count", fill = "Respiratory Status") +
        scale_fill_brewer(palette = "Pastel1") +
        facet_wrap(~type)}else if(input$cluster_level == "Night level"){
          dist_df = k_means_df %>% dplyr::select(subject, day, cluster_night, any_of(event_summary[[which(event_summary$Notes == input$notes), "event"]]), any_of(time_summary[[which(time_summary$Notes == input$notes), "time"]])) %>% 
            pivot_longer(c(4:5), names_to = "measurement", values_to = "value")
          ggplot(dist_df, aes(x = cluster_night, y = value, color = cluster_night)) +
            #geom_boxplot() +
            geom_jitter(aes(shape = cluster_night), show.legend = FALSE) +
            labs(x = "Respiratory Status", y = "Value", fill = "Respiratory Status") +
            scale_fill_brewer(palette = "Pastel1") +
            facet_wrap(~measurement, scales = "free_y")
        }
  })
  output$structure = renderVisNetwork({
    p = as.character(input$patient_v)
    nodes = nodes_f(p, k_means_df)
    edges = links_f(p, nodes, k_means_df)
    vizNetwork(nodes, edges)
  })
  output$sig_table = DT::renderDT({
    if(input$mc == "True"){
      p_select = "p.adj"
      sig_select = "sig.adj"
    }else{
      p_select = "p.value"
      sig_select = "sig"
    }
    if(input$cluster_level == "Subject level"){
      datatable(sig_result_table_subject %>% dplyr::select(outcome, term, estimate, all_of(sig_select)), 
                caption = "Cluster Effects on Standard Measurements and Raw Features",
                options = list(pageLength = 5, autoWidth = TRUE,columnDefs = 
                                                         list(list(className = 'dt-center', 
                                                                   targets = "_all")), order = list(list(c(4),'desc')))) %>% formatStyle(column = c(3),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                     sig_select,
                                                                     target = 'row',
                                                                     backgroundColor = styleEqual(c(".", "*","**","***"), "pink"))
    }else if(input$cluster_level == "Visit level"){
      datatable(sig_result_table_visit %>% dplyr::select(outcome, term, estimate, all_of(sig_select)), 
                caption = "Cluster Effects on Standard Measurements and Raw Features",
                options = list(pageLength = 5, autoWidth = TRUE,columnDefs = 
                                                         list(list(className = 'dt-center', 
                                                                   targets = "_all")), order = list(list(c(4),'desc')))) %>% formatStyle(column = c(3),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                                                     sig_select,
                                                                     target = 'row',
                                                                     backgroundColor = styleEqual(c(".", "*","**","***"), "pink")
                                                                   ) 
    }else if(input$cluster_level == "Night level"){
      datatable(sig_result_table_night %>% dplyr::select(outcome, term, estimate, all_of(sig_select)), 
                caption = "Cluster Effects on Standard Measurements and Raw Features",
                options = list(pageLength = 5, autoWidth = TRUE,columnDefs = 
                                 list(list(className = 'dt-center', 
                                           targets = "_all")), order = list(list(c(4),'desc')))) %>% formatStyle(column = c(3),color = styleInterval(c(-0.00001), c("green", "red"))) %>% formatStyle(
                                             sig_select,
                                             target = 'row',
                                             backgroundColor = styleEqual(c(".", "*","**","***"), "pink")
                                           ) 
    }
  })
  output$visual_1 = renderPlot({
    if(input$cluster_level == "Subject level"){
      ggplot(k_means_df %>% dplyr::select(subject, fvc, cluster_patient) %>% distinct() %>% 
               group_by(subject, cluster_patient) %>% summarize(fvc = mean(fvc)) , aes(x = cluster_patient, y = fvc, fill = cluster_patient)) +
        geom_boxplot() +
        geom_jitter(aes(shape = cluster_patient)) +
        labs(x = "Group", y = "FVC", fill = "Respiratory Status") +
        scale_fill_brewer(palette = "Pastel1")
    }else if(input$cluster_level == "Visit level"){
      visit_df = k_means_df %>% dplyr::select(subject, visit, fvc, mip, cluster_visit) %>% distinct()
      ggplot(visit_df , aes(x = cluster_visit, y = .data[[input$measure_type]], fill = cluster_visit)) +
        geom_boxplot() +
        geom_jitter(aes(shape = cluster_visit)) +
        labs(x = "Group", y = input$measure_type, fill = "Respiratory Status") +
        scale_fill_brewer(palette = "Pastel1")
    }
  })
  output$visual_2 = renderPlot({
    if(input$cluster_level == "Subject level"){
      ggplot(k_means_df %>% dplyr::select(subject, mip, cluster_patient) %>% distinct() %>% 
               group_by(subject, cluster_patient) %>% summarize(mip = mean(mip)), aes(x = cluster_patient, y = mip, fill = cluster_patient)) +
        geom_boxplot() +
        geom_jitter(aes(shape = cluster_patient)) +
        labs(x = "Group", y = "MIP", fill = "Respiratory Status") +
        scale_fill_brewer(palette = "Pastel1")
    }else if(input$cluster_level == "Visit level"){
      visual_df = cluster_df %>% filter(!subject %in% p_ex)
      ggplot(visual_df, aes(x = visit_time, y = .data[[input$measure_type]], color = subject)) +
        geom_line() + 
        geom_point(aes(shape = cluster_visit, size = 3)) +
        scale_size_continuous(guide = "none") +
        labs(x = "Visit Time (days)", y = input$measure_type, shape = "visit level label")
    }
  })

  # Evaluation
  #output$eval_1 = renderPlot({
  #  if(input$standard == "MIP-based"){
  #    cluster_df_long = cluster_df %>% pivot_longer(c("cluster_visit", "mip_label"), names_to = "cluster_method", values_to = "status")}else if(input$standard == "MIP&ALSFRS-based"){
  #      cluster_df_long = cluster_df %>% pivot_longer(c("cluster_visit", "standard_label"), names_to = "cluster_method", values_to = "status")
  #    }
  #  cluster_df_long = cluster_df_long %>% mutate(cluster_method = case_when(cluster_method == "cluster_visit" ~ "proposed method",
  #                                                                          cluster_method == "mip_label" ~ "MIP-based method",
  #                                                                          cluster_method == "standard_label" ~ "MIP&ALSFRS-based",
  #                                                                          .default = cluster_method))
  #  ggplot(cluster_df_long, aes(x = status, y = eval(parse(text = input$std_m)), fill = cluster_method)) +
  #    geom_boxplot() +
  #    geom_jitter(aes(shape = status)) +
  #    labs(x = "Group", y = input$std_m, fill = "Method") +
  #    scale_shape_manual(values = c(19,17)) +
  #    scale_fill_brewer(palette = "Pastel1") +
  #    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10), legend.text = element_text(size = 12))
  #})
  #
  #output$eval_4 = renderPlot({
  #  if(input$eval_patient == "All Patients"){
  #    sdf = cluster_df %>% filter(!subject %in% p_ex)
  #    ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]], color = subject)) +
  #      geom_line() + 
  #      geom_point(aes(shape = cluster_visit, size = 3)) +
  #      scale_size_continuous(guide = "none") +
  #      labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #  }else{
  #    sdf = cluster_df %>% filter(subject == input$eval_patient)
  #    ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]])) +
  #      geom_line(color = "steelblue") + 
  #      geom_point(aes(shape = cluster_visit, size = 3), color = "steelblue") +
  #      scale_size_continuous(guide = "none") +
  #      labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #  }
  #})
  #
  #output$eval_5 = renderPlot({
  #  if(input$eval_patient == "All Patients"){
  #    sdf = cluster_df %>% filter(!subject %in% p_ex)
  #    if(input$standard == "MIP-based"){
  #      ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]], color = subject)) +
  #        geom_line() + 
  #        geom_point(aes(shape = mip_label, size = 3)) +
  #        scale_size_continuous(guide = "none") +
  #        labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #    }else if (input$standard == "MIP&ALSFRS-based"){
  #      ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]], color = subject)) +
  #        geom_line() + 
  #        geom_point(aes(shape = standard_label, size = 3)) +
  #        scale_size_continuous(guide = "none") +
  #        labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #    }
  #  }else{
  #    sdf = cluster_df %>% filter(subject == input$eval_patient)
  #    if(input$standard == "MIP-based"){
  #      ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]])) +
  #        geom_line(color = "salmon") + 
  #        geom_point(aes(shape = mip_label, size = 3, color = "salmon")) +
  #        scale_size_continuous(guide = "none") +
  #        labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #    }else if (input$standard == "MIP&ALSFRS-based"){
  #      ggplot(sdf, aes(x = visit_time, y = sdf[[input$std_m]])) +
  #        geom_line(color = "salmon") + 
  #        geom_point(aes(shape = standard_label, size = 3, color = "salmon")) +
  #        scale_size_continuous(guide = "none") +
  #        labs(x = "Visit Time (days)", y = input$std_m, shape = "visit level label")
  #    }
  #  }
  #})
  #output$mip_survive = renderPlot({
  #  survfit2(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "MIP < 60cm H20")) %>% 
  #    ggsurvfit() +
  #    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  #    labs(
  #      x = "Time from date of symptom onset",
  #      y = "Free of MIP Abnormal Test"
  #    ) +
  #    theme(legend.position = "bottom") +
  #    add_confidence_interval() +
  #    add_risktable()
  #})
  #output$mip_survive_p = renderText({
  #  result_mip = survdiff(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "MIP < 60cm H20"))
  #  p_mip = result_mip$pvalue
  #  if(p_mip > 0.001){p_mip = sprintf("%.3f", p_mip)}else{p_mip = "<0.001"}
  #  print(paste0("The log-rank test p-value is ",p_mip))
  #})
  #output$agreement_table = DT::renderDT({
  #  if(input$standard == "MIP-based"){
  #    agree_table = data.frame(cbind(Measurement = c("agreement rate: estimate", "agreement rate: random_night_CI", "agreement rate: boot_strap_CI", "sensitivity", "specificity"), Agreement = c("0.72", "(0.52-0.76)", "(0.52-0.72)", sprintf("%.2f", sensitivity_mip), sprintf("%.2f", specificity_mip))))
  #  }else if (input$standard == "MIP&ALSFRS-based"){
  #    agree_table = data.frame(cbind(Measurement = c("agreement rate: estimate", "agreement rate: random_night_CI", "agreement rate: boot_strap_CI", "sensitivity", "specificity"), Agreement = c("0.68", "(0.44-0.72)", "(0.44-0.68)", sprintf("%.2f", sensitivity_standard), sprintf("%.2f", specificity_standard))))
  #  }
  #  agree_table %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
  #                                                              targets = "_all")))) 
  #})
  
  output$event_table = DT::renderDT({
    event_summary %>% dplyr::select(event, abnormal, normal) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                     targets = "_all")))) 
  })
  
  #output$event_p = renderText({
  #  event.p = t.test(event_summary$abnormal, event_summary$normal)$p.value
  #  if(event.p<0.001){print("p < 0.001")}else{print(paste0("p = ", sprintf("%.3f", event.p)))}
  #})
  
  output$time_table = DT::renderDT({
    time_summary %>% dplyr::select(time, abnormal, normal) %>% mutate(abnormal = sprintf("%.3f", abnormal),
                            normal = sprintf("%.3f", normal)) %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                      targets = "_all")))) 
  })
  
 #output$time_p = renderText({
 #  time.p = t.test(time_summary$abnormal, time_summary$normal)$p.value
 #  if(time.p<0.001){print("p < 0.001")}else{print(paste0("p = ", sprintf("%.3f", time.p)))}
 #})
  
  output$event_dis = renderPlot({
    if(input$event_name == "All"){
      ggplot(event_summary %>% pivot_longer(c("abnormal", "normal"), names_to = "status", values_to = "events"), aes(x = status, y = events, fill = status)) +
        geom_boxplot() +
        geom_jitter(aes(shape = status)) +
        scale_shape_manual(values = c(19,17)) +
        scale_fill_brewer(palette = "Pastel1") 
    }else{
      ggplot(event_summary %>% pivot_longer(c("abnormal", "normal"), names_to = "status", values_to = "events") %>% filter(event == input$event_name), aes(x = status, y = events, fill = status)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Pastel1")}
  })
  
  output$time_dis = renderPlot({
    if(input$time_name == "All"){
      ggplot(time_summary %>% pivot_longer(c("abnormal", "normal"), names_to = "status", values_to = "periods"), aes(x = status, y = periods, fill = status)) +
        geom_boxplot() +
        geom_jitter(aes(shape = status)) +
        scale_shape_manual(values = c(19,17)) +
        scale_fill_brewer(palette = "Pastel1") 
    }else{
      ggplot(time_summary %>% pivot_longer(c("abnormal", "normal"), names_to = "status", values_to = "periods") %>% filter(time == input$time_name), aes(x = status, y = periods, fill = status)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Pastel1")}
  })
  
  output$kmc = renderPlot({
    if(input$CI == "Yes"){
      survfit2(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("FVC < 50%", "MIP < 60cm H20", input$sensitive, "SpO2 ≤88% for ≥5mins"))) %>% 
        ggsurvfit(linetype_aes = TRUE) +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        labs(
          x = "Time from date of symptom onset (days)",
          y = "Free of abnormal test"
        ) +
        theme(legend.position = "bottom") +
        add_confidence_interval() +
        add_risktable()}else{
          survfit2(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("FVC < 50%", "MIP < 60cm H20", input$sensitive, "SpO2 ≤88% for ≥5mins")) %>% mutate(test = factor(test, levels = c("FVC < 50%", "MIP < 60cm H20", "SpO2 ≤88% for ≥5mins", input$sensitive)))) %>% 
            ggsurvfit(linetype_aes = TRUE) +
            scale_y_continuous(labels = scales::percent_format(scale = 100)) +
            labs(
              x = "Time from date of symptom onset (days)",
              y = "Free of abnormal test"
            ) +
            theme(legend.position = "bottom") +
            add_risktable()
        }
  })
  
  output$km_pred = DT::renderDT({
    name = c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")
    predict_result = summary(survfit(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("FVC < 50%", "MIP < 60cm H20", input$sensitive, "SpO2 ≤88% for ≥5mins"))), times = as.numeric(input$pred_time))
    predict_table = lapply(name, function(x){
      cols = predict_result[[x]]
    }) %>% bind_cols()
    colnames(predict_table) = name
    num_col = c("surv", "std.err", "lower", "upper")
    for (n in name){
      if (n %in% num_col){predict_table[[n]] = sapply(predict_table[[n]], function(x) sprintf("%.3f", x), USE.NAMES = FALSE)}else{
        predict_table[[n]] = as.character(predict_table[[n]])
      }
    }
    predict_table %>% datatable(options = list(columnDefs = list(list(className = 'dt-center', 
                                                                           targets = "_all")))) 
  })
  output$p_result1 = renderText({
    result_1 = survdiff(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("FVC < 50%", "MIP < 60cm H20")))
    p_1 = result_1$pvalue
    if(p_1 > 0.001){p_1 = sprintf("%.3f", p_1)}else{p_1 = "<0.001"}
    print(paste0("The log-rank test p-value is ",p_1))
  })
  output$p_result2 = renderText({
    result_2 = survdiff(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c(input$sensitive, "SpO2 ≤88% for ≥5mins")))
    p_2 = result_2$pvalue
    if(p_2 > 0.001){p_2 = sprintf("%.3f", p_2)}else{p_2 = "<0.001"}
    print(paste0("The log-rank test p-value is ",p_2))
  })
}


