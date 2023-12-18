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
#setwd("/Users/zhengren/Desktop/PennSIVE/Project/Overnight CO2")
## Useful Functions
info_get = function(x, info){
  if(str_split(x, "/")[[1]][3] == "2 subject sample"){
    b = str_split(x, "/")[[1]][4]
    if(info == "visit"){
      c = str_split(b, " ")[[1]][length(str_split(b, " ") [[1]])]
      d = str_split(c,"[.]")[[1]][1]
      return(d)}else if (info == "subject"){
        c = str_split(b, " ")[[1]][2]
        return(c)
      }else if (info == "date"){
        c = str_split(b, " ")[[1]][length(str_split(b, " ") [[1]])]
        d = paste0(str_split(c,"[.]")[[1]][1:3], collapse = ".")
        return(d)
      }
  }else if (str_split(x, "/")[[1]][3] == "All subjects"){
    b = basename(x)
    if(str_split(b,"_")[[1]][1] != "MDA"){
      if(info == "visit"){
        c = str_split(b, "--")[[1]][2]
        d = str_sub(str_split(c,"-")[[1]][1], 5, 6)
        return(d)}else if (info == "subject"){
          c = str_split(b, "--")[[1]][1]
          return(c)
        }else if (info == "date"){
          c = str_split(b, "--")[[1]][2]
          d = str_split(c,"-")[[1]][1]
          return(d)
        }
    }else{
      if(info == "visit"){
        c = str_split(b, " ")[[1]][2]
        d = str_sub(str_split(c,"-")[[1]][1], 5, 6)
        return(d)}else if (info == "subject"){
          c = str_split(b, " ")[[1]][1]
          d = str_sub(c, 1, nchar(c)-5)
          e = str_split(d, "_")[[1]][length(str_split(d, "_")[[1]])]
          return(e)
        }else if (info == "date"){
          c = str_split(b, " ")[[1]][2]
          d = str_split(c,"-")[[1]][1]
          return(d)
        }
    }
  }
}

read_in = function(i, data){
  subj_df = read_excel(data[[i,4]])
  measurement_name = c("PCO2_DC", "SpO2", "PR")
  subj_df = subj_df %>% dplyr::select(c("Time", measurement_name))
  subj_df = replace(subj_df, subj_df == 0, NA)
  subj_df = subj_df %>% na.omit() %>% mutate(day = str_sub(convertToDateTime(subj_df[[nrow(subj_df),"Time"]], tz = "EST"),1,10), Time = convertToDateTime(Time, tz = "EST"), subject = data[[i,"subject"]], visit = data[[i,"visit"]])
  if(!nrow(subj_df) == 0){
    subj_df = subj_df %>% mutate(time = Time - subj_df[[1, "Time"]])
    subj_df = subj_df[,c("subject", "visit", "day", "Time", "time", measurement_name)]
    subj_df$time = sapply(subj_df$time, function(x) x[[1]], USE.NAMES = FALSE)
    time_df = data.frame(cbind(rep(unique(subj_df$subject),max(subj_df$time)+1), rep(unique(subj_df$visit),max(subj_df$time)+1), rep(unique(subj_df$day),max(subj_df$time)+1), seq(0,max(subj_df$time), 1)))
    colnames(time_df) = c("subject", "visit","day","time")
    time_df$time = as.numeric(time_df$time)
    subj_df = time_df %>% left_join(subj_df, by = c("subject", "visit","day","time"))
    subj_df$file = basename(data[[i,4]])
    subj_df = subj_df[c("file", "subject", "visit", "day","Time", "time", measurement_name)]
    }else{
      subj_df = subj_df %>% mutate(time = NA)
      subj_df$file = basename(data[[i,4]])
      subj_df = subj_df[,c("file", "subject", "visit", "day", "Time", "time", measurement_name)]
    }
  return(subj_df)
}

subject_com_df = function(i, input){
  com_df = df %>% filter(subject == input[[i, "subject"]] & visit == input[[i, "visit"]])
  sum_df = lapply(1:nrow(com_df), read_in, data = com_df) %>% bind_rows()
  return(sum_df)
}

date_format = function(x){
  if(is.na(x)){return(x)}else{
    b = str_split(str_split(str_split(x, " - ")[[1]][2], " ")[[1]][1],   "-")[[1]]
    b[2] = case_when(b[2] == "Jan" ~ "01",
                     b[2] == "Feb" ~ "02",
                     b[2] == "Mar" ~ "03",
                     b[2] == "Apr" ~ "04",
                     b[2] == "May" ~ "05",
                     b[2] == "Jun" ~ "06",
                     b[2] == "Jul" ~ "07",
                     b[2] == "Aug" ~ "08",
                     b[2] == "Sep" ~ "09",
                     b[2] == "Oct" ~ "10",
                     b[2] == "Nov" ~ "11",
                     b[2] == "Dec" ~ "12")
    c = paste0(c(b[3],b[2],b[1]), collapse = "-")
    return(c)}
}

time_cal = function(s, d, thre, df, type, return_type, t_length = 5){
  if(type == "PCO2"){
    sub_df = df %>% filter(subject == s, day == d, PCO2_DC > thre)
    if(nrow(sub_df) > 1){
      t = sub_df %>% dplyr::select(time) %>% pull()
      t_diff = diff(t)
      index = which(t_diff > 1)
      index_c = c(0, index, length(t_diff) + 1)
      time_int = diff(index_c)
      event = length(time_int[time_int > t_length*60])
      time_sum = sum(time_int[time_int > t_length*60])
    }else{
      time_sum = 0
      event = 0
    }
  }else if(type == "SpO2"){
    sub_df = df %>% filter(subject == s, day == d, SpO2 < thre)
    if(nrow(sub_df) > 1){
      t = sub_df %>% dplyr::select(time) %>% pull()
      t_diff = diff(t)
      index = which(t_diff > 1)
      index_c = c(0, index, length(t_diff) + 1)
      time_int = diff(index_c)
      event = length(time_int[time_int > t_length*60])
      time_sum = sum(time_int[time_int > t_length*60])
    }else{
      time_sum = 0
      event = 0
    }
  }else if(type == "delta"){
    sub_df = df %>% filter(subject == s, day == d, PCO2_DC >= (thre + 10))
    if(nrow(sub_df) > 1){
      t = sub_df %>% dplyr::select(time) %>% pull()
      t_diff = diff(t)
      index = which(t_diff > 1)
      index_c = c(0, index, length(t_diff) + 1)
      time_int = diff(index_c)
      time_sum = sum(time_int)
      if(time_sum >= 10*60){
        event = 1
      }else{event = 0}
    }else{
      time_sum = 0
      event = 0  
    }
  }else if(type == "delta_50"){
    sub_df = df %>% filter(subject == s, day == d, PCO2_DC >= (thre + 10))
    if(nrow(sub_df) > 1){
      t = sub_df %>% dplyr::select(time) %>% pull()
      t_diff = diff(t)
      index = which(t_diff > 1)
      index_end = c(index, length(t))
      index_start = c(1, index+1)
      t_end = t[index_end]
      t_start = t[index_start]
      n_int = length(t_end)
      over_50 = sapply(1:n_int, function(w){
        pco2_level = sub_df %>% filter(time >= t_start[w]&time <= t_end[w]) %>% pull(PCO2_DC)
        if(sum(pco2_level >= 50) > 0){return(1)}else{return(0)}
      }, USE.NAMES = FALSE)
      time_int = t_end - t_start + 1
      time_int = time_int[over_50 > 0]
      time_sum = sum(time_int)
      if(time_sum >= 10*60){
        event = 1
      }else{event = 0}
    }else{
      time_sum = 0
      event = 0
    }
  }
  if(return_type == "time"){
    return(time_sum)
  }else if(return_type == "event"){
    return(event)
  }
}

lambda_tunning = function(lambda, outcome, df, features){ 
  formula = paste0(outcome,  " ~ ", paste(features, collapse = " + "))
  BIC_vec = sapply(lambda, function(w){
    glm = glmmLasso(as.formula(formula),
          rnd = list(subject=~1), 
          family = gaussian(link = "identity"),
          data = df,
          lambda = w)
    bic= glm$bic
    return(as.numeric(bic))
  }, USE.NAMES = FALSE)
  best_lambda = lambda[which(BIC_vec == min(BIC_vec))][1]
  return(best_lambda)
}

mix_model = function(outcome, thre = NULL, df, type = "raw", lambda = NULL, features = NULL){
  if(type == "raw"){
    PC_time = case_when(thre == 40 ~ "PCO2.40",
                      thre == 45 ~ "PCO2.45",
                      thre == 50 ~ "PCO2.50",
                      thre == 55 ~ "PCO2.55",
                      thre == 60 ~ "PCO2.60")
    event = case_when(thre == 40 ~ "event_40",
                    thre == 45 ~ "event_45",
                    thre == 50 ~ "event_50",
                    thre == 55 ~ "event_55",
                    thre == 60 ~ "event_60")
    form = paste0(outcome, " ~ mean_delta_pct + sd_delta_pct + sd_PCO2 + delta_pco2_time + delta_pco2_event + ", PC_time, " + ", event, " + (1 | subject)")
    if (grepl("alsfrs", outcome)){
      glm_form = paste0(outcome, " ~ mean_delta_pct + sd_delta_pct + sd_PCO2 + delta_pco2_time + delta_pco2_event + ", PC_time, " + ", event)
      glm1 = glm(as.formula(glm_form), data = df, family = binomial(link = "logit"))
      model = glmer(as.formula(form), data = df, family = binomial(link = "logit"), start = list(fixef=coef(glm1)), control = glmerControl(nAGQ0initStep=FALSE))}else{
      model = lmerTest::lmer(as.formula(form), data = df)}
    result_table = model %>% tidy(conf.int = TRUE) %>% filter(term %in% c("mean_delta_pct", "sd_delta_pct", "sd_PCO2", "delta_pco2_time", "delta_pco2_event", PC_time, event)) %>% mutate(result = outcome, PCO2_thre = thre) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, result, PCO2_thre)
    result_table = result_table %>% mutate(estimate = sprintf("%.3f", estimate),
                                           conf.low = sprintf("%.3f", conf.low),
                                           conf.high = sprintf("%.3f", conf.high),
                                           p.value = sprintf("%.3f", p.value))
    }else if(type == "pca") {
    formula = paste0(outcome,  " ~ ", paste(features, collapse = " + "), " + (1 | subject)")
    if (grepl("alsfrs", outcome)){
      glm_form = paste0(outcome,  " ~ ", paste(features, collapse = " + "))
      model = glmer(as.formula(formula), data = df, family = binomial(link = "logit"), start = list(fixef=coef(glm1)), control = glmerControl(nAGQ0initStep=FALSE))
    }else{
      model = lmerTest::lmer(as.formula(formula), data = df)}
      result_table = model %>% tidy() %>% filter(term %in% features) %>% mutate(result = outcome) %>% dplyr::select(term, estimate, p.value, result)
      result_table$estimate = sprintf("%.3f",result_table$estimate)
    }else if(type == "wavelet"){
      formula = paste0(outcome,  " ~ ", paste(features, collapse = " + "))
      if (grepl("alsfrs", outcome)){
        model = glmmLasso(as.formula(formula),
                          rnd = list(subject=~1), 
                          family = binomial(link = "logit"),
                          data = df,
                          lambda = lambda,
                          switch.NR = TRUE,
                          final.re = TRUE)
      }else{
      model = glmmLasso(as.formula(formula),
          rnd = list(subject=~1), 
          family = gaussian(link = "identity"),
          data = df,
          lambda = lambda,
          switch.NR = TRUE,
          final.re = TRUE)}
      result_table = data.frame(summary(model)$coefficients)[-1,] %>% filter(!is.na(p.value)) %>% dplyr::select("Estimate", "StdErr", "z.value","p.value") %>% rownames_to_column() %>% mutate(
        conf.low = Estimate - abs(z.value*StdErr),
        conf.high = Estimate + abs(z.value*StdErr),
        sig = case_when(p.value >= 0.5 & p.value < 0.1 ~ ".",
                            p.value >= 0.01 & p.value < 0.05 ~ "*",
                            p.value >= 0.001 & p.value < 0.01~ "**",
                            p.value < 0.001~ "***"), result = outcome) %>% dplyr::select(rowname, Estimate, conf.low, conf.high, p.value, sig, result)
      colnames(result_table) = c("term", "estimate", "conf.low", "conf.high", "p.value", "sig", "result") 
      result_table = result_table[c("result", "term", "estimate", "conf.low", "conf.high", "p.value", "sig")]
      result_table = result_table %>% mutate(estimate = sprintf("%.3f", estimate),
                                             conf.low = sprintf("%.3f", conf.low),
                                             conf.high = sprintf("%.3f", conf.high),
                                             p.value = sprintf("%.3f", p.value))
    }
  return(list(result_table, model))
}

drift_correction = function(x){
  sample = read_excel(x)
  col_name = colnames(sample)
  if("PCO2_DC" %in% col_name){
    return("DC")
  }else{return("Non_DC")}
}

PR = function(x){
  sample = read_excel(x)
  col_name = colnames(sample)
  if("PR" %in% col_name){
    return("YES")
  }else{return("NO")}
}

time_length = function(x){
  sample = read_excel(x)
  if("PCO2_DC" %in% colnames(sample)){
    measurement_name = c("PCO2_DC", "SpO2", "PR")
    subj_df = sample %>% dplyr::select(c("Time", measurement_name))
    subj_df = replace(subj_df, subj_df == 0, NA)
    subj_df = subj_df %>% na.omit() 
    period = nrow(subj_df)
    if(period >= 4*60*60){return(">= 4 hours")}else{return("< 4 hours")}
  }else{return("Non_DC")}
}

delta_cal = function(s, v, d, spot_check_df, total_df){
  baseline = spot_check_df %>% filter(subject == s, visit == v) %>% dplyr::select(baseline) %>% pull()
  if(! length(baseline) == 0){
    data_stream = total_df %>% filter(subject == s, visit == v) %>% mutate(delta = PCO2_DC - baseline)
    summary = data_stream %>% filter(delta > 10) %>% group_by(day) %>% summarize(time = n()) %>% filter(time > 10*60) %>% filter(day == d)
    if (nrow(summary) == 0){
      return(0)
    }else{return(1)}}else{return(NA)}
}

rolling_sd = function(s, d, m, total_df, period){
  x = total_df %>% filter(subject == s, day == d) %>% pull(m)
  len = length(x)
  sd_list = c()
  for (t in 1:(len - period*60)){
    pco2_int = x[t:(t+period*60)]
    sd_int = sd(pco2_int)
    sd_list = c(sd_list, sd_int)
  }
  max_sd = max(sd_list)
  sd_start = which(sd_list == max_sd)
  sd_end = sd_start + period*60
  return(data.frame(cbind(s, d, max_sd, sd_start, sd_end)))
}  

missing_remove = function(measurement, t){
  na_id = which(is.na(measurement))
  if(!length(which(is.na(measurement))) == 0){ 
    n = length(which(diff(na_id) > 1)) + 1
    if (n == 1){
      na_pre = max(na_id[1] - t*60, 0)
      na_post = min(na_id[length(na_id)] + t*60, length(measurement))
      measurement[na_pre:na_post] = NA  
    }else{
      na_diff = diff(na_id)
      diff_id = which(na_diff > 1)
      time_id = sapply(diff_id, function(x) sum(c(na_id[1],na_diff[1:x])), USE.NAMES = FALSE)
      time_id_end = sapply(diff_id, function(x) sum(c(na_id[1],na_diff[1:(x-1)])), USE.NAMES = FALSE)
      time_id = c(na_id[1], time_id)
      time_id_end = c(time_id_end, na_id[length(na_id)])
      for (i in 1:length(time_id)){
        na_pre = max(time_id[i] - t*60, 0)
        na_post = min(time_id_end[i] + t*60, length(measurement))
        measurement[na_pre:na_post] = NA
      }
    }
  }
  return(measurement)
}

missing_connect = function(df, s, d){
  sub_df = df %>% filter(subject == s, day == d)
  sub_df$PCO2_DC = missing_remove(sub_df$PCO2_DC, t = 5)
  sub_df$SpO2 = missing_remove(sub_df$SpO2, t = 5)
  sub_df$PR = missing_remove(sub_df$PR, t = 5)
  sub_df = sub_df %>% na.omit() %>% mutate(time = 0:(nrow(na.omit(sub_df))-1))
  return(sub_df)
}

missing_check = function(df, s, d, m, type){
  sub_df = df %>% filter(subject == s, day == d)
  miss = sum(is.na(sub_df[[m]]))
  na_id = which(is.na(sub_df[[m]]))
  na_diff = diff(na_id)
  diff_id = which(na_diff > 1) 
  n = length(diff_id) + 1
  if (type == "status"){
    if(miss == 0){
    return ("No")
  }else{return("Yes")}
  }else if (type == "times"){
    if(miss == 0){
      return(0)
    }else{return(n)}
  } 
}

change_from_baseline = function(s,v,df,summary_df){
  sub_df = df %>% filter(subject == s, visit == v)
  baseline = summary_df %>% filter(subject == s, visit == v) %>% pull(spot_co2)
  if (length(!baseline == 0)){
    sub_df = sub_df %>% mutate(delta_pct = (PCO2_DC - baseline)*100/baseline)
  }else(sub_df = NULL)
  return(sub_df)
}

subject_summary_gen = function(df, summary_df){
  input = df %>% group_by(subject, visit) %>% summarize(count = n()) %>% ungroup() %>% dplyr::select(subject, visit)
  df = lapply(1:nrow(input), function(i){change_from_baseline(input[[i, "subject"]], input[[i, "visit"]], df, summary_df)}) %>% bind_rows()
  subject_summary = df %>% na.omit() %>% group_by(subject, visit, day) %>% summarize(mean_PCO2 = mean(PCO2_DC), sd_PCO2 = sd(PCO2_DC), mean_SpO2 = mean(SpO2), sd_SpO2 = sd(SpO2), mean_PR = mean(PR), sd_PR = sd(PR), time_length = n(), mean_delta_pct = mean(delta_pct), sd_delta_pct = sd(delta_pct)) %>% ungroup()
  subject_summary$PCO2.45 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "time", t_length = 5)}, USE.NAMES = FALSE) 
  subject_summary$PCO2.50 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "time", t_length = 5)}, USE.NAMES = FALSE)
  subject_summary$SpO2.88 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 88, total_df, type = "SpO2", return_type = "time", t_length = 5)}, USE.NAMES = FALSE)
  subject_summary$PCO2.45.10 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "time", t_length = 10)}, USE.NAMES = FALSE) 
  subject_summary$PCO2.50.10 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "time", t_length = 10)}, USE.NAMES = FALSE)
  
  subject_summary = subject_summary %>% mutate(
    PCO2.45 = PCO2.45/time_length,
    PCO2.50 = PCO2.50/time_length,
    SpO2.88 = SpO2.88/time_length,
    PCO2.45.10 = PCO2.45.10/time_length,
    PCO2.50.10 = PCO2.50.10/time_length
  )
  
  subject_summary$event_45 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "event", t_length = 5)}, USE.NAMES = FALSE)
  subject_summary$event_50 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "event", t_length = 5)}, USE.NAMES = FALSE)
  subject_summary$event_45.10 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 45, total_df, type = "PCO2", return_type = "event", t_length = 10)}, USE.NAMES = FALSE)
  subject_summary$event_50.10 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 50, total_df, type = "PCO2", return_type = "event", t_length = 10)}, USE.NAMES = FALSE)
  subject_summary$event_spo2 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 88, total_df, type = "SpO2", return_type = "event", t_length = 5)}, USE.NAMES = FALSE)
  subject_summary$event_spo2_less_5 = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], 88, total_df, type = "SpO2", return_type = "event", t_length = 0)}, USE.NAMES = FALSE)
  
  #subject_summary = subject_summary %>% left_join(summary_df %>% dplyr::select(subject,visit, fvc, mip, visit_age, height, weight, spot_co2, spot_o2, spot_hr, alsfrs_10, alsfrs_11), by = c("subject", "visit"))
  subject_summary = subject_summary %>% left_join(summary_df %>% dplyr::select(subject,visit, fvc, mip, spot_co2, alsfrs_10, alsfrs_11, alsfrs_total), by = c("subject", "visit"))
  subject_summary$delta_pco2_time = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], subject_summary[[i, "spot_co2"]], df, type = "delta", return_type = "time")}, USE.NAMES = FALSE)
  subject_summary$delta_pco2_50_time = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], subject_summary[[i, "spot_co2"]], df, type = "delta_50", return_type = "time")}, USE.NAMES = FALSE)
  subject_summary$delta_pco2_event = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], subject_summary[[i, "spot_co2"]], df, type = "delta", return_type = "event")}, USE.NAMES = FALSE)
  subject_summary$delta_pco2_50_event = sapply(1:nrow(subject_summary), function(i){time_cal(subject_summary[[i, "subject"]], subject_summary[[i, "day"]], subject_summary[[i, "spot_co2"]], df, type = "delta_50", return_type = "event")}, USE.NAMES = FALSE)
  subject_summary = subject_summary %>% mutate(
    delta_pco2_time = delta_pco2_time/time_length,
    delta_pco2_50_time = delta_pco2_50_time/time_length
  )
  
  return(subject_summary)
}

missing_inpute = function(s, d, total_df){
    hyp04_20221112 = total_df %>% filter(subject == s, day == d)
    miss_index = which(is.na(hyp04_20221112$PCO2_DC))
    miss_diff = diff(miss_index)
    miss_diff_ind = which(miss_diff  > 1)
    stop = c(0, miss_diff_ind, length(miss_index))
    int = diff(stop) 
    end = c()
    for (i in 2:length(stop)){
        end = c(end, hyp04_20221112$PCO2_DC[miss_index[stop[i]] + 1])
    }                                                      
    start = c()
    for (i in 1:(length(stop)-1)){
        start = c(start, hyp04_20221112$PCO2_DC[miss_index[stop[i] + 1] - 1])
    }

    b = (end - start)/(int+1)
    inpute = c()
    for (i in 1:length(start)){
        for (j in 1:int[i]){
            inpute = c(inpute, start[i] + j*b[i])
        }
    }
    hyp04_20221112$PCO2_DC[which(is.na(hyp04_20221112$PCO2_DC))] = inpute 
    #for (m in miss_index){
      #hyp04_20221112$PCO2_DC[m] = mean(hyp04_20221112$PCO2_DC[max(m-5*60,1):max(m-60,1)])
    #}
    return(hyp04_20221112)}

    remove_data = function(st, et, df, type = "rm_head/tail"){
      if(type == "rm_head/tail"){
        start = which(df$Time > as.POSIXct(st, tz="EST"))[1]
        end = which(df$Time > as.POSIXct(et, tz="EST"))[1]
        df = df %>% filter(row_number() >= start & row_number() <= end)
        df$time = seq(0, nrow(df) - 1)}else if(type == "rm_interval"){
        start = which(df$Time > as.POSIXct(st, tz="EST"))[1]
        end = which(df$Time > as.POSIXct(et, tz="EST"))[1]
        df$PCO2_DC[start:end] = NA
        }
      return(df)
    }

## Wavelet

family_selection = function(waveletFamily, pco2_enc){
  baseSelect = data.frame(WaveletFam = character(),Entropy=numeric(),stringsAsFactors = FALSE)
  
  for(i in waveletFamily){
    ywd = wd(family = i[1],data = pco2_enc,filter.number = i[2])
    nthresh = nlevelsWT(ywd)-1
    coefs = list()
    for (k in 0:nthresh) {
      coefs = list(list(accessD(ywd, level = k)),coefs)
    }
    coefs = list(list(accessC(ywd, level = 0)),coefs)
    coefs = unlist(coefs)
    EntropyB = Shannon.entropy(abs(coefs)/max(abs(coefs)))
    baseSelect = rbind(baseSelect,data.frame(waveletFam = paste(i[1],i[2]),Entropy = EntropyB,stringsAsFactors = FALSE))
  }
  best = baseSelect %>% arrange(Entropy) %>% head(5) %>% pull(waveletFam)
  best_1 = best[1]
  best_2 = best[2]
  best_3 = best[3]
  return(list(best_1, best_2, best_3))
}

wavelet_pre = function(df, input, i, n, m){
  test = df %>% filter(subject == input[[i, "subject"]], day == input[[i, "day"]]) 
  pco2 = test[[m]]
  p = input[[i, "power"]]
  pco2_enc = c(pco2, rep(pco2[length(pco2)], (2^(p + 1) - length(pco2))))
  best_f = family_selection(waveletFamily, pco2_enc)
  if(n == 1){
    return(best_f[[1]])
  }else if(n == 2){
    return(best_f[[2]])
  }else if(n == 3){
  return(best_f[[3]])}
}


## Feature extraction

get_features = function(coef){
  entropy = Shannon.entropy(abs(coef)/max(abs(coef)))
  statistic_features = list(
    n5 = unname(quantile(coef, 0.05)),
    n25 = unname(quantile(coef, 0.25)),
    n75 = unname(quantile(coef, 0.75)),
    n95 = unname(quantile(coef, 0.95)),
    median = median(coef),
    mean = mean(coef),
    std = sd(coef),
    var = var(coef),
    rms = mean(coef^2))
  zero_cross =  sum(diff(sign(coef))!=0)
  mean_cross = sum(diff(sign(coef - mean(coef)))!=0)
  return(list(entropy = entropy, statistic_features = statistic_features, zero_cross = zero_cross, mean_cross = mean_cross))
}

remove_pad = function(C_coef){
  c_diff = diff(C_coef)
  ind_c = which(c_diff == 0)
  if(length(ind_c) == 0){return(C_coef)}else{
    ind_diff = diff(ind_c)
    if(length(which(ind_diff > 1)) != 0){
      ind_cut_ind = which(ind_diff > 1)[length(which(ind_diff > 1))] + 1}else{ind_cut_ind = 1}
    ind_cut = ind_c[ind_cut_ind]
    C_coef_cut = C_coef[1:ind_cut]
    return(C_coef_cut)}
    }

feature_extraction = function(df, input, i){
  sub_df = df %>% filter(subject == input[[i, "subject"]], day == input[[i, "day"]])
  power = input[[i, "power"]]
  co2 = sub_df$PCO2_DC
  co2_pad = c(co2, rep(co2[length(co2)], 2^(power + 1)-length(co2)))
  ywd = wd(family = "DaubLeAsymm",data = co2_pad,filter.number = 7)
  nlevel = ywd$nlevels
  features = lapply(1:13, function(l){
    C_coef = accessC(ywd, level = l)
    D_coef = accessD(ywd, level = l)
    if (l >= 5){
      C_coef = remove_pad(C_coef)
      D_coef = remove_pad(D_coef)
    }
    c = unlist(get_features(C_coef))
    d = unlist(get_features(D_coef))
    df = data.frame(list(c = c,d = d))
    df_t = data.frame(t(df) )
    df_t = tibble::rownames_to_column(df_t, "type")
    df_t_wide = df_t %>% pivot_wider(names_from = type, values_from = colnames(df_t)[-1])
    df_t_wide$level = paste0("level ", l)   
    return(df_t_wide)
  }) %>% bind_rows()
  features_wide = features %>% pivot_wider(names_from = level, values_from = colnames(features)[-25])
  features_wide$subject = input[[i, "subject"]]
  features_wide$day = input[[i, "day"]]
  features_wide = features_wide[c(313:314, 1:312)]
  return(features_wide)
}

assign_cluster = function(s, k, k_means_df, type = "subject"){
  if(type == "subject"){
    sub = c()
    for (i in 1:k) {
      #cluster_name = paste0("cluster_", i)
      cluster =  k_means_df %>% filter(cluster == as.character(i)) %>% group_by(subject) %>% summarize(count = n())
      n = cluster %>% filter(subject == s) %>% pull(count)
      if(length(n) == 0){n = 0}
      sub = c(sub, n)
    }
    cluster_assign = which(sub == max(sub))
    if(length(cluster_assign)>1){cluster_assign = paste(cluster_assign, collapse = ",")}
    return(as.character(cluster_assign))
    }else if(type == "visit"){
      cluster = lapply(1:k, function(i){
        k_means_df %>% filter(cluster == as.character(i)) %>% group_by(subject, visit) %>% summarize(count = n()) %>% ungroup() %>% mutate(cluster = as.character(i))
      }) %>% bind_rows()
      sub_df = cluster %>% filter(subject == s)
      visit = unique(sub_df$visit)
      visit_label = sapply(visit, function(x){
        visit_df = sub_df %>% filter(visit == x) %>% group_by(cluster) %>% summarize(count = sum(count))
        count = visit_df %>% pull(count)
        max_ind = which(count == max(count))
        cluster_assign = visit_df$cluster[max_ind]
        if(length(cluster_assign) > 1){cluster_assign = paste(cluster_assign, collapse = "/")}
        return(cluster_assign)
      }, USE.NAMES = FALSE)
      visit_cluster = data.frame(cbind(rep(s, length(visit)), visit, visit_label))
      colnames(visit_cluster) = c("subject", "visit", "cluster")
      return(visit_cluster)
    }
}

# Cluster-ROC Analysis
simulation_test = function(k_means_df, bs = TRUE){
  subj = unique(k_means_df$subject)
  shuffled_df = lapply(subj, function(s){
    sub_df = k_means_df %>% filter(subject == s)
    sub_visit = unique(sub_df$visit)
    shuffled_df_sub = lapply(sub_visit, function(v){
      if(bs){
        v_df = sub_df %>% filter(visit == v) %>% slice_sample(prop = 1, replace = TRUE)
      }else{v_df = sub_df %>% filter(visit == v) %>% slice_sample(prop = 1, replace = TRUE) %>% head(1)}
    }) %>% bind_rows()
  }) %>% bind_rows()
  return(shuffled_df)
}

co2_cluster = function(k_means_df, features, weight, thre){
  # Night-level Label
  mat = as.matrix(k_means_df[features]) 
  hclust.out = hclust(dist(mat))
  k_means_df$cluster = cutree(hclust.out, k = 3)
  k_means_df$cluster = as.factor(k_means_df$cluster)
  k_means_df$cluster = sapply(k_means_df$cluster, function(x){case_when(x == "2" ~ weight, 
                                                                        x == "3" ~ 0,
                                                                        x == "1" ~ 1)}, USE.NAMES = FALSE)
  k_means_df$cluster_night = k_means_df$cluster
  
  # Visit-level Label
  visit_cluster = k_means_df %>% dplyr::select(subject, visit, day, cluster_night) %>% group_by(subject, visit) %>% summarize(cluster_visit = mean(cluster_night)) %>% 
    ungroup() %>%
    mutate(visit_pred = case_when(cluster_visit >= thre ~ "abnormal",
                                  .default = "normal"))
  #k_means_df = k_means_df %>% left_join(visit_cluster, by = c("subject", "visit")) 
  visit_level = k_means_df %>% dplyr::select(subject, visit, standard_label, mip_label) %>% distinct() %>% left_join(visit_cluster, by = c("subject", "visit")) 
  
  patient_level = k_means_df %>% dplyr::select(subject, visit, standard_label, mip_label) %>% distinct() %>% 
    mutate(standard_label = case_when(standard_label == "abnormal" ~ 1,
                                      .default = 0),
           mip_label = case_when(mip_label == "abnormal" ~ 1,
                                 .default = 0)) %>% group_by(subject) %>% 
    summarize(standard_label = case_when(mean(standard_label) >= 0.5 ~ "abnormal",
                                         .default = "normal"),  
              mip_label = case_when(mean(mip_label) >= 0.5 ~ "abnormal",
                                    .default = "normal")) %>% 
    left_join(visit_cluster %>% mutate(visit_pred = case_when(visit_pred == "abnormal" ~ 1,
                                                              .default = 0)) %>% 
                group_by(subject) %>% summarize(cluster_subject = mean(visit_pred),
                          subject_pred = case_when(cluster_subject >= thre ~ "abnormal",
                                                  .default = "normal")), by = c("subject"))
  
    roc_visit_mip = roc(visit_level$mip_label, visit_level$cluster_visit)
    roc_visit_std = roc(visit_level$standard_label, visit_level$cluster_visit)
    roc_patient_mip = roc(patient_level$mip_label, patient_level$cluster_subject)
    roc_patient_std = roc(patient_level$standard_label, patient_level$cluster_subject)
  
    return(list("visit_df" = visit_level, "patient_df" = patient_level, rocs = list("visit_mip" = roc_visit_mip, "visit_standard" = roc_visit_std, "patient_mip" = roc_patient_mip, "patient_standard" = roc_patient_std)))
}

best_thre = function(k_means_df, features){
  weight_range = seq(0.1,1,0.1)
  thre_range = seq(0.1,1,0.1)
  input = expand_grid(weight_range, thre_range)
  output = lapply(1:nrow(input), function(i){
    output_rocs = co2_cluster(k_means_df, features, input[[i,"weight_range"]], input[[i,"thre_range"]])$rocs
    output_df = data.frame(cbind(weight = input[[i,"weight_range"]], threshold = input[[i,"thre_range"]], 
                                 visit_mip_auc = output_rocs$visit_mip$auc,
                           visit_standard_auc = output_rocs$visit_standard$auc, 
                           patient_mip_auc = output_rocs$patient_mip$auc,
                           patient_standard_auc = output_rocs$patient_standard$auc,
                           mean_auc = mean(c(output_rocs$visit_mip$auc, output_rocs$visit_standard$auc, output_rocs$patient_mip$auc, output_rocs$patient_standard$auc)))) 
    return(output_df)
  }) %>% bind_rows() %>% arrange(desc(mean_auc))
  return(output)
}

# Majority Vote Method
majority_vote = function(k_means_df, rt = FALSE){
  mat = as.matrix(k_means_df[features]) 
  #mat = apply(mat, 2, scale)
  hclust.out = hclust(dist(mat))
  k_means_df$cluster = cutree(hclust.out, k = 3)
  k_means_df$cluster = as.factor(k_means_df$cluster)
  main_cluster = names(table(k_means_df$cluster) %>% sort() %>% tail(2))
  mip_mean = sapply(main_cluster, function(x) k_means_df %>% filter(cluster == x) %>% pull(mip) %>% mean(), USE.NAMES = FALSE)
  min_mip = min(mip_mean)
  name_cluster = data.frame(cbind(main_cluster, mip_mean)) %>% mutate(cluster_name = case_when(mip_mean == min_mip ~ "normal",
                                                                                               .default = "abnormal"))
  k_means_df$cluster = sapply(k_means_df$cluster, function(x){case_when(x == name_cluster[[1, "main_cluster"]] ~ name_cluster[[1, "cluster_name"]], 
                                                                        x == name_cluster[[2, "main_cluster"]] ~ name_cluster[[2, "cluster_name"]],
                                                                       .default = "abnormal")}, USE.NAMES = FALSE)
  
  k_means_df$cluster = sapply(k_means_df$cluster, function(x){case_when(x == "abnormal" ~ "1", 
                                                                        x == "normal" ~ "3")}, USE.NAMES = FALSE)
  k_means_df$cluster_night = k_means_df$cluster
  visit_cluster = lapply(unique(k_means_df$subject), function(x) assign_cluster(x, k = 3, k_means_df, type = "visit")) %>% bind_rows() %>% rename(cluster_visit = cluster)
  k_means_df = k_means_df %>% left_join(visit_cluster, by = c("subject", "visit"))
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
  
  
  k_means_df$cluster = sapply(k_means_df$cluster, function(x){
    case_when(x == "1" ~ "Group A",
              x == "2" ~ "Group B", 
              x == "3" ~ "Group C",
              x == "1/3" ~ "Group A/C",
              x == "1/2" ~ "Group A/B",
              x == "2/3" ~ "Group B/C")
  }, USE.NAMES = FALSE)
  
  # Linear Mixed Model Result
  k_means_df$visit = sapply(1:nrow(k_means_df), function(i){
    visit_enco(k_means_df[[i, "subject"]], k_means_df[[i, "visit"]], k_means_df)
  }, USE.NAMES = FALSE)
  visit_df = lapply(unique(k_means_df$subject), function(x){
    sub_df = k_means_df %>% filter(subject == x) %>% arrange(visit) 
    sub_df = sub_df %>% mutate(visit_time = day - sub_df$day[1], .after = visit)
    return(sub_df)
  }) %>% bind_rows()
  
  visit_df = visit_df %>% dplyr::select(subject, visit, visit_time, fvc, mip, cluster_visit) %>% distinct() %>% group_by(subject, visit, cluster_visit, fvc, mip) %>% summarize(visit_time = mean(visit_time))
  
  visit_df$cluster_visit = sapply(visit_df$cluster_visit, function(x){
    case_when(x == "Group A" ~ "abnormal",
              x == "Group C" ~ "normal",
              x == "Group A/C" ~ "abnormal")
  }, USE.NAMES = FALSE)
  visit_df$cluster_visit = as.factor(visit_df$cluster_visit)
  
  ## Exclude subjects only have one visit
  p_ex = visit_df %>% group_by(subject) %>% summarize(count = n()) %>% filter(count == 1) %>% pull(subject)
  
  result_table = lapply(c("mip", "fvc"), function(x) cluster_test(x, visit_df %>% filter(!subject %in% p_ex))) %>% bind_rows()
  # Ground Truth Check
  
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
  
  agree_df = k_means_df %>% dplyr::select(subject, visit, mip_label, standard_label, cluster_visit, cluster) %>% 
    mutate(cluster_visit = case_when(cluster_visit  == "Group A" ~ "abnormal",
                                     cluster_visit  == "Group C" ~ "normal",
                                     cluster_visit  == "Group A/C" ~ "abnormal")) %>% group_by(subject, visit) %>% distinct() %>% 
    mutate(mip_agreed = case_when(mip_label == cluster_visit ~ "agreed",
                                  .default = "not agreed"),
           standard_agreed = case_when(
             standard_label == cluster_visit ~ "agreed",
             .default = "not agreed"))
  mip_agree = nrow(agree_df %>% filter(mip_agreed == "agreed"))/nrow(agree_df)
  standard_agree = nrow(agree_df %>% filter(standard_agreed == "agreed"))/nrow(agree_df)
  if(rt){
    return(k_means_df)
  }else{
    return(list("mixed_model_result" = result_table, "mip_agree_rate" = mip_agree, "standard_agree_rate" = standard_agree))
  }
}


# Network Visualization

id_node = function(nodes, node.name){
  nodes %>% 
    filter(variable == node.name) %>% 
    pull(id)
}

nodes_f = function(s, k_means_df){
  df = k_means_df %>% arrange(subject, day) %>% dplyr::select(subject, visit, day, cluster_night, cluster_visit, cluster_patient) %>% distinct() %>% filter(subject == s) 
  visit_uni = setNames(seq(1, length(unique(df$visit)), 1), 
                       unique(df$visit))
  df$visit = dplyr::recode(df$visit, !!!visit_uni)
  nodes_night = df %>% mutate(variable = paste0(subject, "_", day)) %>% dplyr::select(variable, cluster_night) %>% rename("group" = "cluster_night")
  nodes_visit = df %>% mutate(variable = paste0(subject, "_", visit)) %>% dplyr::select(variable, cluster_visit) %>% rename("group" = "cluster_visit") %>% distinct()
  nodes_subject = df %>% mutate(variable = subject) %>% dplyr::select(variable, cluster_patient) %>% rename("group" = "cluster_patient") %>% distinct()
  nodes_df = rbind(nodes_subject, nodes_visit, nodes_night) %>% mutate(id = seq(1,nrow(.), 1), .before  = 'variable')
  return(nodes_df)}

visit_enco = function(s, v, k_means_df){
  df = k_means_df %>% arrange(subject, day) %>% dplyr::select(subject, visit, day, cluster_night, cluster_visit, cluster_patient) %>% distinct() %>% filter(subject == s) 
  visit_uni = setNames(seq(1, length(unique(df$visit)), 1), 
                       unique(df$visit))
  visit_re = visit_uni[[v]]
  return(visit_re)
}

links_f = function(s, nodes, k_means_df){
  df = k_means_df %>% arrange(subject, day) %>% dplyr::select(subject, visit, day, cluster_night, cluster_visit, cluster_patient) %>% distinct() %>% filter(subject == s) 
  visit_uni = setNames(seq(1, length(unique(df$visit)), 1), 
                       unique(df$visit))
  df$visit = dplyr::recode(df$visit, !!!visit_uni)
  df = df %>% mutate(night_level = paste0(subject, "_", day), visit_level = paste0(subject, "_", visit))
  link_night = df %>% dplyr::select(night_level, visit_level) %>% rename(from = visit_level, to = night_level) %>% mutate(from = purrr::map_int(from, id_node, nodes = nodes), to = purrr::map_int(to, id_node, nodes = nodes))
  link_visit = df %>% dplyr::select(visit_level, subject) %>% rename("from" = "subject", "to" = "visit_level") %>% distinct() %>% mutate(from = purrr::map_int(from, id_node, nodes = nodes), to = purrr::map_int(to, id_node, nodes = nodes))
  links_df = rbind(link_visit, link_night) %>% dplyr::select(from, to)
  return(links_df)
}

vizNetwork = function(vis.nodes, vis.edges){
  colrs = setNames(c("salmon", "cyan"), 
                   c("Abnormal", "Normal"))
  # adapt to variable 
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$variable # Text on click
  vis.nodes$label  <- vis.nodes$variable # Node label
  vis.nodes$borderWidth <- 2
  vis.nodes$color.background <- dplyr::recode(vis.nodes$group, !!!colrs)
  vis.nodes$color.border <- "black"
    vis.nodes$color.highlight.background <- "orange"
      vis.nodes$color.highlight.border <- "darkred"
        vis.edges$color <- "white"
          
        visNetwork(vis.nodes, vis.edges, width = "100%") %>% 
          visEdges(arrows = "from") %>% 
          visHierarchicalLayout() %>% visGroups(groupname = "Abnormal", color = 'salmon', shape = "dot", color.border = 'black') %>% visGroups(groupname = "Normal", color = 'cyan', shape = "dot", color.border = 'black') %>% visNodes(font = list(color = "white")) %>% 
          visOptions(highlightNearest = TRUE) %>% visInteraction(navigationButtons = TRUE)
}

cluster_test = function(outcome, df, label = "cluster_visit", type = "visit"){
  if(outcome %in% c("fvc", "mip")){
    if(type == "visit"){
      form = paste0(outcome, " ~ ", label, " + visit_time + (1 | subject)")
      model = lmerTest::lmer(as.formula(form), data = df)
      result_table = model %>% tidy() %>% filter(effect == "fixed", term != "(Intercept)") %>% mutate(term = gsub(label, "", term), sig = case_when(p.value< 0.1 & p.value >= 0.05 ~ ".",
                                                                                                                                                    p.value < 0.05 & p.value >= 0.01 ~ "*",
                                                                                                                                                    p.value < 0.01 & p.value >= 0.001 ~ "**",
                                                                                                                                                    p.value < 0.001 ~ "***",
                                                                                                                                                    .default = NA)) %>% dplyr::select(term, estimate, p.value, sig) %>% mutate(outcome = outcome, .before = "term", estimate = round(estimate, 3))
      }else if(type == "subject"){
        form = paste0(outcome, " ~ ", label)
        model = lm(as.formula(form), data = df)
        result_table = model %>% tidy() %>% filter(grepl(paste0(label,"*"), term)) %>% mutate(term = gsub(label, "", term), sig = case_when(p.value< 0.1 & p.value >= 0.05 ~ ".",
                                                                                                                                            p.value < 0.05 & p.value >= 0.01 ~ "*",
                                                                                                                                            p.value < 0.01 & p.value >= 0.001 ~ "**",
                                                                                                                                            p.value < 0.001 ~ "***",
                                                                                                                                            .default = NA)) %>% dplyr::select(term, estimate, p.value, sig) %>% mutate(outcome = outcome, .before = "term", estimate = round(estimate, 3))
      }else if(type == "night"){
        form = paste0(outcome, " ~ ", label, " + (1 | subject)")
        model = lmerTest::lmer(as.formula(form), data = df)
        result_table = model %>% tidy() %>% filter(effect == "fixed", term != "(Intercept)") %>% mutate(term = gsub(label, "", term), sig = case_when(p.value< 0.1 & p.value >= 0.05 ~ ".",
                                                                                                                                                      p.value < 0.05 & p.value >= 0.01 ~ "*",
                                                                                                                                                      p.value < 0.01 & p.value >= 0.001 ~ "**",
                                                                                                                                                      p.value < 0.001 ~ "***",
                                                                                                                                                      .default = NA)) %>% dplyr::select(term, estimate, p.value, sig) %>% mutate(outcome = outcome, .before = "term", estimate = round(estimate, 3))
      }
  }else{
    if(type == "subject"){
      form = paste0(outcome, " ~ ", label)
      model = lm(as.formula(form), data = df)
      result_table = model %>% tidy() %>% filter(grepl(paste0(label,"*"), term)) %>% mutate(term = gsub(label, "", term), sig = case_when(p.value< 0.1 & p.value >= 0.05 ~ ".",
                                                                                                                                          p.value < 0.05 & p.value >= 0.01 ~ "*",
                                                                                                                                          p.value < 0.01 & p.value >= 0.001 ~ "**",
                                                                                                                                          p.value < 0.001 ~ "***",
                                                                                                                                          .default = NA)) %>% dplyr::select(term, estimate, p.value, sig) %>% mutate(outcome = outcome, .before = "term", estimate = round(estimate, 3))
    }else{
      form = paste0(outcome, " ~ ", label, " + (1 | subject)")
      model = lmerTest::lmer(as.formula(form), data = df)
      result_table = model %>% tidy() %>% filter(effect == "fixed", term != "(Intercept)") %>% mutate(term = gsub(label, "", term), sig = case_when(p.value< 0.1 & p.value >= 0.05 ~ ".",
                                                                                                                                                    p.value < 0.05 & p.value >= 0.01 ~ "*",
                                                                                                                                                    p.value < 0.01 & p.value >= 0.001 ~ "**",
                                                                                                                                                    p.value < 0.001 ~ "***",
                                                                                                                                                    .default = NA)) %>% dplyr::select(term, estimate, p.value, sig) %>% mutate(outcome = outcome, .before = "term", estimate = round(estimate, 3))
    }
  }
  return(result_table)
}

frequency_conversion = function(total_df, s, d, level){
  co2 = total_df %>% filter(subject == s, day == d) %>% pull(PCO2_DC)
  power = floor(log2(length(co2)))
  point1 = 1/2^(power + 1 - level)
  point2 = 1/2^(power - level)
  return(paste0("[",point1, " , ", point2, "]"))
}

# Sample Info Summary
sample_info = function(subject_summary){
  subject_summary$event_spo2_less_5 = (subject_summary$event_spo2_less_5 > 0) - (subject_summary$event_spo2 > 0)
  n_used = nrow(subject_summary)
  event_45_5 = nrow(subject_summary %>% filter(event_45 > 0))
  event_45_10 = nrow(subject_summary %>% filter(event_45.10 > 0))
  event_50_10 = nrow(subject_summary %>% filter(event_50.10 > 0))
  delta_10_50 = nrow(subject_summary %>% filter(delta_pco2_50_event > 0))
  fvc_above_50 = nrow(subject_summary %>% filter(fvc > 50))
  mip_above_60 = nrow(subject_summary %>% filter(mip < -60))
  SpO2_less_5min = nrow(subject_summary %>% filter(event_spo2_less_5 > 0))
  norm_com = nrow(subject_summary %>% filter(fvc > 50 & mip < -60 & event_spo2_less_5 > 0))
  name = c("Total Records", "Records (with DC & t >= 4h)", "Records Used", "Records (CO2 > 45 for > 5 mins)", "Records (CO2 > 45 for > 10 mins)",
           "Records (CO2 > 50 for > 10 mins)", "Records (>10 Increase from baseline to CO2 > 50 for > 10 mins)", "Records (fvc > 50%)",
           "Records (mip < -60)", "Records (SpO2 < 88% for < 5 mins)", "Records (fvc > 50%, mip < -60, SpO2 < 88% for < 5 mins)")
  count = c(N, n_dc_4, n_used, event_45_5, event_45_10, event_50_10, delta_10_50, fvc_above_50, mip_above_60, SpO2_less_5min, norm_com)
  sample_size_summary = data.frame(cbind(name, count))
  return(sample_size_summary)
  #write_csv(sample_size_summary, "./data/data_management/data_info/records_info.csv")
}

