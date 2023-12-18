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
library(parallel)

source("./code/co2_function.R")

total_df = read_csv("./data/data_management/total_df/overnight_data_all_subjects_20231129.csv")
input = total_df %>% group_by(file, subject, visit, day) %>% summarize(count = n()) %>% dplyr::select(file, subject, visit, day) %>% ungroup()

input[["PCO2_status"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "PCO2_DC", "status"), USE.NAMES = FALSE)
input[["SpO2_status"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "SpO2", "status"), USE.NAMES = FALSE)
input[["PR_status"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "PR", "status"), USE.NAMES = FALSE)
input[["PCO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "PCO2_DC", "times"), USE.NAMES = FALSE)
input[["SpO2_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "SpO2", "times"), USE.NAMES = FALSE)
input[["PR_miss_n"]] = sapply(1:nrow(input), function(i) missing_check(total_df, input[[i, "subject"]], input[[i, "day"]], "PR", "times"), USE.NAMES = FALSE)

ggplot(total_df %>% filter(subject == input[[i, "subject"]], day == input[[i, "day"]]), aes(x = time, y = PCO2_DC)) +
    geom_line()

write_csv(input, "./data/data_management/missing/missing_summary_20230620.csv")
 

