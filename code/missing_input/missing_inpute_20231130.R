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
library(tibbletime)
setwd("/home/zhengren/Desktop/Project/overnight_co2")
source("/home/zhengren/Desktop/Project/overnight_co2/code/co2_function.R")

total_df = read_csv("./data/data_management/raw_data/overnight_data_all_subjects_20231129.csv")
total_df$day = as.character(total_df$day)
total_df$Time = format(total_df$Time, tz = "EST")
total_df$Time = as.POSIXct(total_df$Time, tz="EST")
#miss_info = read_excel("./data/data_management/Missing_data_quality_check_updated.xls")

# HYP01 2023-08-10
hyp01_20230810 = total_df %>% filter(subject == "HYP01", day == "2023-08-10")
hyp01_20230810_c = hyp01_20230810 %>% filter(!is.na(PCO2_DC))
hyp01_20230810 = missing_inpute("HYP01", "2023-08-10", total_df)
plot(hyp01_20230810$PCO2_DC, type = 'l')
miss_df_c = hyp01_20230810_c
miss_df = hyp01_20230810
index_df = missing_inpute("HYP01", "2023-08-10", total_df, type = "index")

# HYP01 2023-08-12

hyp01_20230812 = total_df %>% filter(subject == "HYP01", day == "2023-08-12")
## direct connection
hyp01_20230812_c = hyp01_20230812 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20230812_c)
## missing inputation
hyp01_20230812 = missing_inpute("HYP01", "2023-08-12", total_df)
plot(hyp01_20230812$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20230812)
## missing index
hyp01_20230812_ind = missing_inpute("HYP01", "2023-08-12", total_df, type = "index")
index_df = rbind(index_df, hyp01_20230812_ind)

# HYP02 2023-08-30

hyp02_20230830 = total_df %>% filter(subject == "HYP02", day == "2023-08-30")
hyp02_20230830 = remove_data("2023-08-30 01:01:00", "2023-08-30 01:20:00", hyp02_20230830, type = "rm_interval")
hyp02_20230830 = remove_data("2023-08-29 23:01:00", "2023-08-30 03:36:30", hyp02_20230830)
hyp02_20230830_c = hyp02_20230830 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230830_c)
hyp02_20230830_ind = missing_inpute("HYP02", "2023-08-30", hyp02_20230830, type = "index")
index_df = rbind(index_df, hyp02_20230830_ind)
hyp02_20230830 = missing_inpute("HYP02", "2023-08-30", hyp02_20230830)
plot(hyp02_20230830$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230830)

# HYP02 2023-08-31

hyp02_20230831 = total_df %>% filter(subject == "HYP02", day == "2023-08-31")
hyp02_20230831_c = hyp02_20230831 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230831_c)
hyp02_20230831_ind = missing_inpute("HYP02", "2023-08-31", hyp02_20230831, type = "index")
index_df = rbind(index_df, hyp02_20230830_ind)
hyp02_20230831 = missing_inpute("HYP02", "2023-08-31", hyp02_20230831)
plot(hyp02_20230831$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230831)

# HYP04 2023-09-19

hyp04_20230919 = total_df %>% filter(subject == "HYP04", day == "2023-09-19")
hyp04_20230919 = remove_data("2023-09-18 23:10:00", "2023-09-19 08:21:00", hyp04_20230919)
hyp04_20230919_c = hyp04_20230919 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230919_c)
hyp04_20230919_ind = missing_inpute("HYP04", "2023-09-19", hyp04_20230919, type = "index")
index_df = rbind(index_df, hyp04_20230919_ind)
hyp04_20230919 = missing_inpute("HYP04", "2023-09-19", hyp04_20230919)
plot(hyp04_20230919$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230919)

# HYP04 2023-09-20

hyp04_20230920 = total_df %>% filter(subject == "HYP04", day == "2023-09-20")
hyp04_20230920 = remove_data("2023-09-20 00:00:00", "2023-09-20 07:40:00", hyp04_20230920)
hyp04_20230920 = remove_data("2023-09-20 02:55:00", "2023-09-20 03:05:00", hyp04_20230920, type = "rm_interval")
hyp04_20230920_c = hyp04_20230920 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230920_c)
hyp04_20230920_ind = missing_inpute("HYP04", "2023-09-20", hyp04_20230920, type = "index")
index_df = rbind(index_df, hyp04_20230920_ind)
hyp04_20230920 = missing_inpute("HYP04", "2023-09-20", hyp04_20230920)
plot(hyp04_20230920$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230920)

# HYP04 2023-09-21

hyp04_20230921 = total_df %>% filter(subject == "HYP04", day == "2023-09-21")
hyp04_20230921 = remove_data("2023-09-20 23:45:00", "2023-09-21 06:38:00", hyp04_20230921)
hyp04_20230921_c = hyp04_20230921 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230921_c)
hyp04_20230921_ind = missing_inpute("HYP04", "2023-09-21", hyp04_20230921, type = "index")
index_df = rbind(index_df, hyp04_20230921_ind)
hyp04_20230921 = missing_inpute("HYP04", "2023-09-21", hyp04_20230921)
plot(hyp04_20230921$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230921)

# HYP07 2023-07-13

hyp07_20230713 = total_df %>% filter(subject == "HYP07", day == "2023-07-13")
hyp07_20230713 = remove_data("2023-07-12 23:10:00", "2023-07-13 04:50:00", hyp07_20230713)
hyp07_20230713_c = hyp07_20230713 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20230713_c)
hyp07_20230713_ind = missing_inpute("HYP07", "2023-07-13", hyp07_20230713, type = "index")
index_df = rbind(index_df, hyp07_20230713_ind)
hyp07_20230713 = missing_inpute("HYP07", "2023-07-13", hyp07_20230713)
plot(hyp07_20230713$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20230713)

# HYP07 2023-07-14

hyp07_20230714 = total_df %>% filter(subject == "HYP07", day == "2023-07-14")
hyp07_20230714 = remove_data("2023-07-13 23:25:00", "2023-07-14 04:40:00", hyp07_20230714)
hyp07_20230714 = remove_data("2023-07-14 00:18:30", "2023-07-14 00:25:00", hyp07_20230714, type = "rm_interval")
hyp07_20230714_c = hyp07_20230714 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20230714_c)
hyp07_20230714_ind = missing_inpute("HYP07", "2023-07-14", hyp07_20230714, type = "index")
index_df = rbind(index_df, hyp07_20230714_ind)
hyp07_20230714 = missing_inpute("HYP07", "2023-07-14", hyp07_20230714)
plot(hyp07_20230714$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20230714)

# HYP07 2023-07-15

hyp07_20230715 = total_df %>% filter(subject == "HYP07", day == "2023-07-15")
hyp07_20230715 = remove_data("2023-07-14 23:25:00", "2023-07-15 06:36:39", hyp07_20230715)
hyp07_20230715_c = hyp07_20230715 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20230715_c)
hyp07_20230715_ind = missing_inpute("HYP07", "2023-07-15", hyp07_20230715, type = "index")
index_df = rbind(index_df, hyp07_20230715_ind)
hyp07_20230715 = missing_inpute("HYP07", "2023-07-15", hyp07_20230715)
plot(hyp07_20230715$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20230715)

# HYP08 2023-07-20

hyp08_20230720 = total_df %>% filter(subject == "HYP08", day == "2023-07-20")
hyp08_20230720_c = hyp08_20230720 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20230720_c)
hyp08_20230720_ind = missing_inpute("HYP08", "2023-07-20", hyp08_20230720, type = "index")
index_df = rbind(index_df, hyp08_20230720_ind)
hyp08_20230720 = missing_inpute("HYP08", "2023-07-20", hyp08_20230720)
plot(hyp08_20230720$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20230720)

# HYP08 2023-10-18

hyp08_20231018 = total_df %>% filter(subject == "HYP08", day == "2023-10-18")
hyp08_20231018_c = hyp08_20231018 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20231018_c)
hyp08_20231018_ind = missing_inpute("HYP08", "2023-10-18", hyp08_20231018, type = "index")
index_df = rbind(index_df, hyp08_20231018_ind)
hyp08_20231018 = missing_inpute("HYP08", "2023-10-18", hyp08_20231018)
plot(hyp08_20231018$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20231018)

# HYP09 2023-11-04

hyp09_20231104 = total_df %>% filter(subject == "HYP09", day == "2023-11-04")
hyp09_20231104 = remove_data("2023-11-04 01:00:00", "2023-11-04 06:32:39", hyp09_20231104)
hyp09_20231104_c = hyp09_20231104 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20231104_c)
hyp09_20231104_ind = missing_inpute("HYP09", "2023-11-04", hyp09_20231104, type = "index")
index_df = rbind(index_df, hyp09_20231104_ind)
hyp09_20231104 = missing_inpute("HYP09", "2023-11-04", hyp09_20231104)
plot(hyp09_20231104$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20231104)

# HYP09 2023-11-05

hyp09_20231105 = total_df %>% filter(subject == "HYP09", day == "2023-11-05")
hyp09_20231105 = remove_data("2023-11-04 23:00:00", "2023-11-05 02:36:37", hyp09_20231105)
hyp09_20231105_c = hyp09_20231105 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20231105_c)
hyp09_20231105_ind = missing_inpute("HYP09", "2023-11-05", hyp09_20231105, type = "index")
index_df = rbind(index_df, hyp09_20231105_ind)
hyp09_20231105 = missing_inpute("HYP09", "2023-11-05", hyp09_20231105)
plot(hyp09_20231105$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20231105)

# HYP12 2023-08-28

hyp12_20230828 = total_df %>% filter(subject == "HYP12", day == "2023-08-28")
hyp12_20230828 = remove_data("2023-08-28 01:00:00", "2023-08-28 11:55:52", hyp12_20230828)
hyp12_20230828_c = hyp12_20230828 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp12_20230828_c)
hyp12_20230828_ind = missing_inpute("HYP12", "2023-08-28", hyp12_20230828, type = "index")
index_df = rbind(index_df, hyp12_20230828_ind)
hyp12_20230828 = missing_inpute("HYP12", "2023-08-28", hyp12_20230828)
plot(hyp12_20230828$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp12_20230828)

# HYP12 2023-08-29

hyp12_20230829 = total_df %>% filter(subject == "HYP12", day == "2023-08-29")
hyp12_20230829 = remove_data("2023-08-29 01:30:00", "2023-08-29 10:39:08", hyp12_20230829)
hyp12_20230829_c = hyp12_20230829 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp12_20230829_c)
hyp12_20230829_ind = missing_inpute("HYP12", "2023-08-29", hyp12_20230829, type = "index")
index_df = rbind(index_df, hyp12_20230829_ind)
hyp12_20230829 = missing_inpute("HYP12", "2023-08-29", hyp12_20230829)
plot(hyp12_20230829$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp12_20230829)

# HYP13 2023-06-08

hyp13_20230608 = total_df %>% filter(subject == "HYP13", day == "2023-06-08")
hyp13_20230608_c = hyp13_20230608 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230608_c)
hyp13_20230608_ind = missing_inpute("HYP13", "2023-06-08", hyp13_20230608, type = "index")
index_df = rbind(index_df, hyp13_20230608_ind)
hyp13_20230608 = missing_inpute("HYP13", "2023-06-08", hyp13_20230608)
plot(hyp13_20230608$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp13_20230608)


# HYP13 2023-06-09

hyp13_20230609 = total_df %>% filter(subject == "HYP13", day == "2023-06-09")
hyp13_20230609_c = hyp13_20230609 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230609_c)
hyp13_20230609_ind = missing_inpute("HYP13", "2023-06-09", hyp13_20230609, type = "index")
index_df = rbind(index_df, hyp13_20230609_ind)
hyp13_20230609 = missing_inpute("HYP13", "2023-06-09", hyp13_20230609)
plot(hyp13_20230609$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp13_20230609)

# HYP13 2023-06-10

hyp13_20230610 = total_df %>% filter(subject == "HYP13", day == "2023-06-10")
hyp13_20230610_c = hyp13_20230610 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230610_c)
hyp13_20230610_ind = missing_inpute("HYP13", "2023-06-10", hyp13_20230610, type = "index")
index_df = rbind(index_df, hyp13_20230610_ind)
hyp13_20230610 = missing_inpute("HYP13", "2023-06-10", hyp13_20230610)
plot(hyp13_20230610$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp13_20230610)

# UPENN03 2023-05-02

upenn03_20230502 = total_df %>% filter(subject == "UPENN03", day == "2023-05-02")
upenn03_20230502_c = upenn03_20230502 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230502_c)
upenn03_20230502_ind = missing_inpute("UPENN03", "2023-05-02", upenn03_20230502, type = "index")
index_df = rbind(index_df, upenn03_20230502_ind)
upenn03_20230502 = missing_inpute("UPENN03", "2023-05-02", upenn03_20230502)
plot(upenn03_20230502$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230502)

# UPENN03 2023-05-03

upenn03_20230503 = total_df %>% filter(subject == "UPENN03", day == "2023-05-03")
upenn03_20230503 = remove_data("2023-05-02 22:00:00", "2023-05-03 00:30:00", upenn03_20230503, type = "rm_interval")
upenn03_20230503_c = upenn03_20230503 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230503_c)
upenn03_20230503_ind = missing_inpute("UPENN03", "2023-05-03", upenn03_20230503, type = "index")
index_df = rbind(index_df, upenn03_20230503_ind)
upenn03_20230503 = missing_inpute("UPENN03", "2023-05-03", upenn03_20230503)
plot(upenn03_20230503$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230503)

# UPENN03 2023-05-04

upenn03_20230504 = total_df %>% filter(subject == "UPENN03", day == "2023-05-04")
upenn03_20230504_c = upenn03_20230504 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230504_c)
upenn03_20230504_ind = missing_inpute("UPENN03", "2023-05-04", upenn03_20230504, type = "index")
index_df = rbind(index_df, upenn03_20230504_ind)
upenn03_20230504 = missing_inpute("UPENN03", "2023-05-04", upenn03_20230504)
plot(upenn03_20230504$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230504)

# UPENN03 2023-07-21

upenn03_20230721 = total_df %>% filter(subject == "UPENN03", day == "2023-07-21")
upenn03_20230721 = remove_data("2023-07-20 23:15:00", "2023-07-20 23:25:00", upenn03_20230721, type = "rm_interval")
upenn03_20230721_c = upenn03_20230721 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230721_c)
upenn03_20230721_ind = missing_inpute("UPENN03", "2023-07-21", upenn03_20230721, type = "index")
index_df = rbind(index_df, upenn03_20230721_ind)
upenn03_20230721 = missing_inpute("UPENN03", "2023-07-21", upenn03_20230721)
plot(upenn03_20230721$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230721)

# UPENN03 2023-07-22

upenn03_20230722 = total_df %>% filter(subject == "UPENN03", day == "2023-07-22")
upenn03_20230722 = remove_data("2023-07-21 21:20:00", "2023-07-22 07:56:35", upenn03_20230722)
upenn03_20230722_c = upenn03_20230722 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230722_c)
upenn03_20230722_ind = missing_inpute("UPENN03", "2023-07-22", upenn03_20230722, type = "index")
index_df = rbind(index_df, upenn03_20230722_ind)
upenn03_20230722 = missing_inpute("UPENN03", "2023-07-22", upenn03_20230722)
plot(upenn03_20230722$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230722)

# UPENN03 2023-07-23

upenn03_20230723 = total_df %>% filter(subject == "UPENN03", day == "2023-07-23")
upenn03_20230723 = remove_data("2023-07-22 20:40:24", "2023-07-23 02:58:00", upenn03_20230723)
upenn03_20230723_c = upenn03_20230723 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn03_20230723_c)
upenn03_20230723_ind = missing_inpute("UPENN03", "2023-07-23", upenn03_20230723, type = "index")
index_df = rbind(index_df, upenn03_20230723_ind)
upenn03_20230723 = missing_inpute("UPENN03", "2023-07-23", upenn03_20230723)
plot(upenn03_20230723$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn03_20230723)

# UPENN01 2023-08-03

upenn01_20230803 = total_df %>% filter(subject == "UPENN01", day == "2023-08-03")
upenn01_20230803_c = upenn01_20230803 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20230803_c)
upenn01_20230803_ind = missing_inpute("UPENN01", "2023-08-03", upenn01_20230803, type = "index")
index_df = rbind(index_df, upenn01_20230803_ind)
upenn01_20230803 = missing_inpute("UPENN01", "2023-08-03", upenn01_20230803)
plot(upenn01_20230803$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20230803)

# UPENN01 2023-08-03

upenn01_20230804 = total_df %>% filter(subject == "UPENN01", day == "2023-08-04")
upenn01_20230804_c = upenn01_20230804 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20230804_c)
upenn01_20230804_ind = missing_inpute("UPENN01", "2023-08-04", upenn01_20230804, type = "index")
index_df = rbind(index_df, upenn01_20230804_ind)
upenn01_20230804 = missing_inpute("UPENN01", "2023-08-04", upenn01_20230804)
plot(upenn01_20230804$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20230804)








#write_csv(miss_df, "./data/qc_data/inpute_missing_data.csv")
write_csv(index_df, "./data/qc_data/input_data/inpute_missing_data_index_20231206.csv")
write_csv(miss_df, "./data/qc_data/input_data/inpute_missing_data_20231206.csv") 























