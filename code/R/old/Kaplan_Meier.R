library(survival)
library(ggsurvfit)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
#setwd("~/Desktop/PennSIVE/Project/Overnight CO2/data/qc_data")
summary_df = read_csv("./data/cluster/hierarchical_clustering_results.csv")
summary_df$visit = sapply(summary_df$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
info_df = read_excel("~/Desktop/PennSIVE/Project/Overnight CO2/data/qc_data/MDA_DATA_V3_05.10.23.xls")
info_df = info_df %>% dplyr::select(PDF_Filename, date_onset, date_visit) %>% na.omit()
info_df$subject = sapply(info_df$PDF_Filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
info_df$visit = sapply(info_df$date_visit, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
summary_df = summary_df %>% left_join(info_df %>% dplyr::select(subject, visit, date_onset, date_visit), by = c("subject", "visit"))
# Use Onset Date as the begining date
km_df = summary_df %>% dplyr::select(subject, day, event_45, event_50.10, event_spo2, SpO2.88, delta_pco2_event, delta_pco2_50_event, fvc, mip, date_onset, date_visit, cluster_visit) %>% mutate(fvc_50 = case_when(fvc < 50 ~ 1,
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
                                                                                                                      nocturnal_test = case_when(cluster_visit == "abnormal" ~ 1,
                                                                                                                                                 .default = 0),
                                                                                                                      time_sd = date_visit - date_onset,
                                                                                                                      time_co2 = as_datetime(day) - date_onset)
sd_df = km_df %>% dplyr::select(subject, fvc, mip, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% distinct()
sd_df = sd_df %>% dplyr::select(subject, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% pivot_longer(c(2:4), names_to = "test", values_to = "event") %>% rename(time = time_sd)

pco2_df = km_df %>% dplyr::select(subject, pco2_45, spo2_88, pco2_50.10, delta_pco2_event, delta_pco2_50_event, time_co2, cluster_visit) %>% distinct()
pco2_df = pco2_df %>% pivot_longer(c(2:6), names_to = "test", values_to = "event") %>% rename(time = time_co2)

test_df =rbind(sd_df, pco2_df) 
write_csv(test_df, "./data/kaplan-meier/kaplan_meier_20230510.csv")

# survival analysis
test_df = read_csv("./data/kaplan-meier/kaplan_meier_20230510.csv")
test_df$test = factor(test_df$test, levels = c("fvc_50", "mip_60", "pco2_45", "spo2_88", "pco2_50.10", "delta_pco2_event", "delta_pco2_50_event"),
                          labels = c("FVC < 50%", "MIP < 60cm H20", "Nocturnal CO2 >45mmHg for >5mins", "SpO2 ≤88% for ≥5mins", 
                                     "Nocturnal CO2 >50mmHg for >10mins", "nocturnal CO2 increase >10 mmHg from baseline CO2 for >10 mins", 
                                     "nocturnal CO2 increase >10 mmHg from baseline to above >50 mmHg for >10 mins"))
survfit2(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("mip_60", "pco2_50.10", "spo2_88", "nocturnal_test"))) %>% 
  ggsurvfit() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset",
    y = "Free of abnormal test"
  ) +
  theme(legend.position = "bottom") +
  #add_confidence_interval() +
  add_risktable()

predict_table = summary(survfit(Surv(time, event) ~ test, data = test_df), times = 365.25)$table


# survival analysis for Model Evaluation
survfit2(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "mip_60")) %>% 
  ggsurvfit() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset",
    y = "Free of MIP Abnormal Test"
  ) +
  theme(legend.position = "bottom") +
  add_confidence_interval() +
  add_risktable()

result_1 = survdiff(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "mip_60"))
p_1 = result_1$pvalue


# Use First Visit Date as the begining date
summary_df$first_visit = sapply(summary_df$subject, function(x){
  return(summary_df %>% filter(subject == x) %>% pull(date_visit) %>% unique() %>% head(1) %>% as.Date())
}, USE.NAMES = FALSE) %>% as.Date(origin = "1970-01-01")
km_df = summary_df %>% dplyr::select(subject, day, event_45, event_50.10, event_spo2, SpO2.88, delta_pco2_event, delta_pco2_50_event, fvc, mip, first_visit, date_visit, cluster_visit) %>% mutate(fvc_50 = case_when(fvc < 50 ~ 1,
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
                                                                                                                                                                                                  nocturnal_test = case_when(cluster_visit == "abnormal" ~ 1,
                                                                                                                                                                                                                             .default = 0),
                                                                                                                                                                                                  time_sd = as.Date(date_visit) - first_visit,
                                                                                                                                                                                                  time_co2 = day - first_visit)
sd_df = km_df %>% dplyr::select(subject, fvc, mip, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% distinct()
sd_df = sd_df %>% dplyr::select(subject, fvc_50, mip_60, nocturnal_test, time_sd, cluster_visit) %>% pivot_longer(c(2:4), names_to = "test", values_to = "event") %>% rename(time = time_sd)

pco2_df = km_df %>% dplyr::select(subject, pco2_45, spo2_88, pco2_50.10, delta_pco2_event, delta_pco2_50_event, time_co2, cluster_visit) %>% distinct()
pco2_df = pco2_df %>% pivot_longer(c(2:6), names_to = "test", values_to = "event") %>% rename(time = time_co2)

test_df =rbind(sd_df, pco2_df) 
write_csv(test_df, "./data/kaplan-meier/kaplan_meier_20230510_first_visit.csv")

# survival analysis
test_df = read_csv("./data/kaplan-meier/kaplan_meier_20230510_first_visit.csv")
test_df$test = factor(test_df$test, levels = c("fvc_50", "mip_60", "pco2_45", "spo2_88", "pco2_50.10", "delta_pco2_event", "delta_pco2_50_event"),
                      labels = c("FVC < 50%", "MIP < 60cm H20", "Nocturnal CO2 >45mmHg for >5mins", "SpO2 ≤88% for ≥5mins", 
                                 "Nocturnal CO2 >50mmHg for >10mins", "nocturnal CO2 increase >10 mmHg from baseline CO2 for >10 mins", 
                                 "nocturnal CO2 increase >10 mmHg from baseline to above >50 mmHg for >10 mins"))
survfit2(Surv(time, event) ~ test, data = test_df %>% filter(test %in% c("mip_60", "pco2_50.10", "spo2_88", "nocturnal_test"))) %>% 
  ggsurvfit() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset",
    y = "Free of abnormal test"
  ) +
  theme(legend.position = "bottom") +
  #add_confidence_interval() +
  add_risktable()

predict_table = summary(survfit(Surv(time, event) ~ test, data = test_df), times = 365.25)$table


# survival analysis for Model Evaluation
survfit2(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "mip_60")) %>% 
  ggsurvfit() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset",
    y = "Free of MIP Abnormal Test"
  ) +
  theme(legend.position = "bottom") +
  add_confidence_interval() +
  add_risktable()

result_1 = survdiff(Surv(time, event) ~ cluster_visit, data = test_df %>% filter(test == "mip_60"))
p_1 = result_1$pvalue

# Subject-level
subject_l_data = function(s, t, test_df){
  event_check = test_df %>% filter(subject == s, test == t) %>% pull(event) %>% unique()
  if(1 %in% event_check){
    sub_df = test_df %>% filter(subject == s, test == t) %>% filter(event == 1) %>% head(1)
  }else{
    sub_df = test_df %>% filter(subject == s, test == t) %>% head(1)
  }
  return(sub_df)
}

k_input = expand_grid(subject = unique(test_df$subject), test = unique(test_df$test))
k_data = lapply(1:nrow(k_input), function(i){
  sub_df = subject_l_data(k_input[[i,"subject"]], k_input[[i, "test"]], test_df)
  return(sub_df)
}) %>% bind_rows()
write_csv(k_data, "./data/kaplan-meier/kaplan_meier_20230510_subject.csv")

survfit2(Surv(time, event) ~ test, data = k_data %>% filter(test %in% c("mip_60", "fvc_50", "spo2_88", "nocturnal_test"))) %>% 
  ggsurvfit() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Time from date of symptom onset",
    y = "Free of abnormal test"
  ) +
  theme(legend.position = "bottom") +
  #add_confidence_interval() +
  add_risktable()
