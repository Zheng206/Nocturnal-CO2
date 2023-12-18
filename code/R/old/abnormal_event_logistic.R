library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(readxl)
source("./code/co2_function.R")

summary_df = read_csv("./data/cluster/hierarchical_clustering_results.csv")
summary_df$first_visit = sapply(summary_df$subject, function(x){
  visit_date = summary_df %>% filter(subject == x) %>% pull(day) %>% head(1)
  return(visit_date)
}, USE.NAMES = FALSE) %>% as.Date(origin = "1970-01-01")
km_df = summary_df %>% dplyr::select(subject, day, event_45, event_50.10, event_spo2, SpO2.88, delta_pco2_event, delta_pco2_50_event, fvc, mip, cluster_visit, first_visit) %>% mutate(fvc_50 = case_when(fvc < 50 ~ 1,
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
                                                                                                                                                                          time = day - first_visit)

km_df$visit = sapply(km_df$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
info_df = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
info_df = info_df %>% dplyr::select(PDF_Filename, date_visit, visit_age, sex, height, weight) %>% na.omit()
info_df$subject = sapply(info_df$PDF_Filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
info_df$visit = sapply(info_df$date_visit, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
km_df = km_df %>% left_join(info_df %>% dplyr::select(subject, visit, visit_age, sex, height, weight), by = c("subject", "visit"))

# logistic regression
logistic_mix_gen = function(outcome, km_df){
  glm_form = paste0(outcome, " ~ cluster_visit ")
  form = paste0(outcome, " ~ cluster_visit + (1 | subject)")
  glm1 = glm(as.formula(glm_form), data = km_df, family = binomial(link = "logit"))
  model = glmer(as.formula(form), data = km_df, family = binomial(link = "logit"), start = list(fixef=coef(glm1)), control = glmerControl(nAGQ0initStep=FALSE))
  result = model %>% tidy(conf.int = TRUE) %>% filter(term %in% c("cluster_visitanormal", "time")) %>% 
    dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
    mutate(term = gsub("cluster_visit", "", term),
           sig = case_when(p.value > 0.01 & p.value < 0.05 ~ "*",
                           p.value > 0.001 & p.value <= 0.01~ "**",
                           p.value < 0.001~ "***"),
           outcome = outcome) %>% dplyr::select(outcome, term, estimate, conf.low, conf.high, p.value, sig)
  return(result)
}
outcome = c("fvc_50", "mip_60", "pco2_45", "spo2_88", "pco2_50.10", "nocturnal_test", "delta_pco2_event", "delta_pco2_50_event")

results = lapply(outcome, function(x) {
  tryCatch({
    result = logistic_mix_gen(x, km_df)
    return(result)
  }, error = function(err){
    cat("An error occurred:", conditionMessage(err), "\n")
    return(NULL)
    })}
  ) %>% bind_rows() %>% filter(!is.na(sig))





glmer(fvc_50 ~ visit_age + sex + cluster_visit + time, data = km_df, family = binomial(link = "logit"))
lmerTest::lmer(fvc_50 ~ visit_age + sex + cluster_visit + time)
glm_form = paste0(outcome,  " ~ ", paste(features, collapse = " + "))
model = glmer(as.formula(formula), data = df, family = binomial(link = "logit"), start = list(fixef=coef(glm1)), control = glmerControl(nAGQ0initStep=FALSE))