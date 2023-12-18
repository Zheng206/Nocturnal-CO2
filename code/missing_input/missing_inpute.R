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

total_df = read_csv("./data/data_management/overnight_data_all_subjects_new.csv")
total_df$day = as.character(total_df$day)
total_df$Time = format(total_df$Time, tz = "EST")
total_df$Time = as.POSIXct(total_df$Time, tz="EST")
miss_info = read_excel("./data/data_management/Missing_data_quality_check_updated.xls")

# HYP04 2022-11-12
hyp04_20221112 = total_df %>% filter(subject == "HYP04", day == "2022-11-12")
hyp04_20221112_c = hyp04_20221112 %>% filter(!is.na(PCO2_DC))
hyp04_20221112 = missing_inpute("HYP04", "2022-11-12", total_df)
plot(hyp04_20221112$PCO2_DC, type = 'l')
miss_df_c = hyp04_20221112_c
miss_df = hyp04_20221112
index_df = missing_inpute("HYP04", "2022-11-12", total_df, type = "index")

# HYP04 2023-03-05
hyp04_20230305 = total_df %>% filter(subject == "HYP04", day == "2023-03-05")
hyp04_20230305 = remove_data("2023-03-04 23:30:00", "2023-03-05 06:25:00", hyp04_20230305)
hyp04_20230305_c = hyp04_20230305 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230305_c)
hyp04_20230305_ind = missing_inpute("HYP04", "2023-03-05", hyp04_20230305, type = "index")
index_df = rbind(index_df, hyp04_20230305_ind)
hyp04_20230305 = missing_inpute("HYP04", "2023-03-05", hyp04_20230305)
plot(hyp04_20230305$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230305)


# HYP04 2022-11-11
hyp04_20221111 = total_df %>% filter(subject == "HYP04", day == "2022-11-11")
hyp04_20221111 = remove_data("2022-11-11 02:00:00", "2022-11-11 08:45:00", hyp04_20221111)
hyp04_20221111_c = hyp04_20221111 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20221111_c)
hyp04_20221111_ind = missing_inpute("HYP04", "2022-11-11", hyp04_20221111, type = "index")
index_df = rbind(index_df, hyp04_20221111_ind)
hyp04_20221111 = missing_inpute("HYP04", "2022-11-11", hyp04_20221111)
plot(hyp04_20221111$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20221111)


# HYP04 2022-11-13
hyp04_20221113 = total_df %>% filter(subject == "HYP04", day == "2022-11-13")
hyp04_20221113_c = hyp04_20221113 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20221113_c)
hyp04_20221113 = missing_inpute("HYP04", "2022-11-13", total_df)
plot(hyp04_20221113$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20221113)
index_df = rbind(index_df, missing_inpute("HYP04", "2022-11-13", total_df, type = "index"))

# HYP04 2023-03-03
hyp04_20230303 = total_df %>% filter(subject == "HYP04", day == "2023-03-03")
hyp04_20230303 = remove_data("2023-03-03 01:10:00", "2023-03-03 07:16:00", hyp04_20230303)
hyp04_20230303 = remove_data("2023-03-03 01:35:00", "2023-03-03 02:15:00", hyp04_20230303, type = "rm_interval")
hyp04_20230303_c = hyp04_20230303 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230303_c)
hyp04_20230303_ind = missing_inpute("HYP04", "2023-03-03", hyp04_20230303, type = "index")
index_df = rbind(index_df, hyp04_20230303_ind)
hyp04_20230303 = missing_inpute("HYP04", "2023-03-03", hyp04_20230303)
plot(hyp04_20230303$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230303)


# HYP04 2023-03-04
hyp04_20230304 = total_df %>% filter(subject == "HYP04", day == "2023-03-04")
hyp04_20230304 = remove_data("2023-03-04 01:30:00", "2023-03-04 06:29:00", hyp04_20230304)
hyp04_20230304_c = hyp04_20230304 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20230304_c)
hyp04_20230304_ind = missing_inpute("HYP04", "2023-03-04", hyp04_20230304, type = "index")
index_df = rbind(index_df, hyp04_20230304_ind)
hyp04_20230304 = missing_inpute("HYP04", "2023-03-04", hyp04_20230304)
plot(hyp04_20230304$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20230304)



# HYP04 2022-07-23
hyp04_20220723 = total_df %>% filter(subject == "HYP04", day == "2022-07-23")
hyp04_20220723 = remove_data("2022-07-23 04:40:00", "2022-07-23 05:10:00", hyp04_20220723, type = "rm_interval")
hyp04_20220723 = remove_data("2022-07-23 01:12:05", "2022-07-23 06:50:00", hyp04_20220723)
hyp04_20220723_c = hyp04_20220723 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20220723_c)
hyp04_20220723_ind  = missing_inpute("HYP04", "2022-07-23", hyp04_20220723, type = "index")
index_df = rbind(index_df, hyp04_20220723_ind)
hyp04_20220723  = missing_inpute("HYP04", "2022-07-23", hyp04_20220723 )
plot(hyp04_20220723$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20220723)


# HYP04 2022-07-22
hyp04_20220722 = total_df %>% filter(subject == "HYP04", day == "2022-07-22")
hyp04_20220722 = remove_data("2022-07-22 00:55:00", "2022-07-22 08:33:00", hyp04_20220722)
hyp04_20220722_c = hyp04_20220722 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20220722_c)
hyp04_20220722_ind  = missing_inpute("HYP04", "2022-07-22", hyp04_20220722, type = "index")
index_df = rbind(index_df, hyp04_20220722_ind)
hyp04_20220722  = missing_inpute("HYP04", "2022-07-22", hyp04_20220722)
plot(hyp04_20220722$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20220722)


# UPENN01 2022-09-20
upenn01_20220920 = total_df %>% filter(subject == "UPENN01", day == "2022-09-20")
upenn01_20220920 = remove_data("2022-09-20 01:20:00", "2022-09-20 02:15:00", upenn01_20220920, type = "rm_interval")
upenn01_20220920_c = upenn01_20220920 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20220920_c)
upenn01_20220920_ind = missing_inpute("UPENN01", "2022-09-20", upenn01_20220920, type = "index")
index_df = rbind(index_df, upenn01_20220920_ind)
upenn01_20220920 = missing_inpute("UPENN01", "2022-09-20", upenn01_20220920)
plot(upenn01_20220920$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20220920)


# HYP04 2022-07-21
hyp04_20220721 = total_df %>% filter(subject == "HYP04", day == "2022-07-21")
hyp04_20220721 = remove_data("2022-07-21 00:10:00", "2022-07-21 08:12:13", hyp04_20220721)
hyp04_20220721_c = hyp04_20220721 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp04_20220721_c)
hyp04_20220721_ind = missing_inpute("HYP04", "2022-07-21", hyp04_20220721, type = "index")
index_df = rbind(index_df, hyp04_20220721_ind)
hyp04_20220721 = missing_inpute("HYP04", "2022-07-21", hyp04_20220721)
plot(hyp04_20220721$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp04_20220721)


# UPENN01 2022-09-19
upenn01_20220919 = total_df %>% filter(subject == "UPENN01", day == "2022-09-19")
upenn01_20220919 = remove_data("2022-09-18 21:26:00", "2022-09-19 06:30:00", upenn01_20220919)
upenn01_20220919_c = upenn01_20220919 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20220919_c)
upenn01_20220919_ind = missing_inpute("UPENN01", "2022-09-19", upenn01_20220919, type = "index")
index_df = rbind(index_df, upenn01_20220919_ind)
upenn01_20220919 = missing_inpute("UPENN01", "2022-09-19", upenn01_20220919)
plot(upenn01_20220919$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20220919)


# UPENN01 2022-12-18
upenn01_20221218 = total_df %>% filter(subject == "UPENN01", day == "2022-12-18")
upenn01_20221218 = remove_data("2022-12-17 23:10:00", "2022-12-18 00:00:00", upenn01_20221218, type = "rm_interval")
upenn01_20221218_c = upenn01_20221218 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20221218_c)
upenn01_20221218_ind = missing_inpute("UPENN01", "2022-12-18", upenn01_20221218, type = "index")
index_df = rbind(index_df, upenn01_20221218_ind)
upenn01_20221218 = missing_inpute("UPENN01", "2022-12-18", upenn01_20221218)
plot(upenn01_20221218$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20221218)


# HYP11 2022-09-22
hyp11_20220922 = total_df %>% filter(subject == "HYP11", day == "2022-09-22")
hyp11_20220922_c = hyp11_20220922 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp11_20220922_c)
hyp11_20220922_ind = missing_inpute("HYP11", "2022-09-22", hyp11_20220922, type = "index")
index_df = rbind(index_df, hyp11_20220922_ind)
hyp11_20220922 = missing_inpute("HYP11", "2022-09-22", hyp11_20220922)
plot(hyp11_20220922$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp11_20220922)


# UPENN01 2022-12-16
upenn01_20221216 = total_df %>% filter(subject == "UPENN01", day == "2022-12-16")
upenn01_20221216 = remove_data("2022-12-15 22:43:00", "2022-12-16 05:00:00", upenn01_20221216)
upenn01_20221216_c = upenn01_20221216 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20221216_c)
upenn01_20221216_ind = missing_inpute("UPENN01", "2022-12-16", upenn01_20221216, type = "index")
index_df = rbind(index_df, upenn01_20221216_ind)
upenn01_20221216 = missing_inpute("UPENN01", "2022-12-16", upenn01_20221216)
plot(upenn01_20221216$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20221216)


# UPENN01 2023-03-18
upenn01_20230318 = total_df %>% filter(subject == "UPENN01", day == "2023-03-18")
upenn01_20230318 = remove_data("2023-03-17 23:38:00", "2023-03-18 06:20:00", upenn01_20230318)
upenn01_20230318_c = upenn01_20230318 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20230318_c)
upenn01_20230318_ind = missing_inpute("UPENN01", "2023-03-18", upenn01_20230318, type = "index")
index_df = rbind(index_df, upenn01_20230318_ind)
upenn01_20230318 = missing_inpute("UPENN01", "2023-03-18", upenn01_20230318)
plot(upenn01_20230318$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20230318)


# UPENN01 2023-03-20
upenn01_20230320 = total_df %>% filter(subject == "UPENN01", day == "2023-03-20")
upenn01_20230320 = remove_data("2023-03-20 03:27:00", "2023-03-20 04:10:00", upenn01_20230320, type = "rm_interval")
upenn01_20230320_c = upenn01_20230320 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20230320_c)
upenn01_20230320_ind = missing_inpute("UPENN01", "2023-03-20", upenn01_20230320, type = "index")
index_df = rbind(index_df, upenn01_20230320_ind)
upenn01_20230320 = missing_inpute("UPENN01", "2023-03-20", upenn01_20230320)
plot(upenn01_20230320$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20230320)


# HYP01 2022-09-09
hyp01_20220909 = total_df %>% filter(subject == "HYP01", day == "2022-09-09")
hyp01_20220909_c = hyp01_20220909 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20220909_c)
hyp01_20220909_ind = missing_inpute("HYP01", "2022-09-09", hyp01_20220909, type = "index")
index_df = rbind(index_df, hyp01_20220909_ind)
hyp01_20220909 = missing_inpute("HYP01", "2022-09-09", hyp01_20220909)
plot(hyp01_20220909$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20220909)


# UPENN02 2023-03-13
upenn02_20230313 = total_df %>% filter(subject == "UPENN02", day == "2023-03-13")
upenn02_20230313 = remove_data("2023-03-13 03:40:00", "2023-03-13 03:48:00", upenn02_20230313, type = "rm_interval")
upenn02_20230313 = remove_data("2023-03-13 05:25:00", "2023-03-13 05:30:00", upenn02_20230313, type = "rm_interval")
upenn02_20230313_c = upenn02_20230313 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn02_20230313_c)
upenn02_20230313_ind = missing_inpute("UPENN02", "2023-03-13", upenn02_20230313, type = "index")
index_df = rbind(index_df, upenn02_20230313_ind)
upenn02_20230313 = missing_inpute("UPENN02", "2023-03-13", upenn02_20230313)
plot(upenn02_20230313$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn02_20230313)


# HYP07 2022-08-13
hyp07_20220813 = total_df %>% filter(subject == "HYP07", day == "2022-08-13")
hyp07_20220813 = remove_data("2022-08-13 01:00:00", "2022-08-13 07:11:00", hyp07_20220813)
hyp07_20220813_c = hyp07_20220813 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20220813_c)
hyp07_20220813_ind = missing_inpute("HYP07", "2022-08-13", hyp07_20220813, type = "index")
index_df = rbind(index_df, hyp07_20220813_ind)
hyp07_20220813 = missing_inpute("HYP07", "2022-08-13", hyp07_20220813)
plot(hyp07_20220813$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20220813)


# UPENN02 2023-03-14
upenn02_20230314 = total_df %>% filter(subject == "UPENN02", day == "2023-03-14")
upenn02_20230314 = remove_data("2023-03-13 23:00:00", "2023-03-14 08:50:00", upenn02_20230314)
upenn02_20230314 = remove_data("2023-03-13 23:17:00", "2023-03-13 23:30:00", upenn02_20230314, type = "rm_interval")
upenn02_20230314 = remove_data("2023-03-13 23:35:00", "2023-03-13 23:40:00", upenn02_20230314, type = "rm_interval")
upenn02_20230314 = remove_data("2023-03-14 04:10:00", "2023-03-14 04:20:00", upenn02_20230314, type = "rm_interval")
upenn02_20230314_c = upenn02_20230314 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn02_20230314_c)
upenn02_20230314_ind = missing_inpute("UPENN02", "2023-03-14", upenn02_20230314, type = "index")
index_df = rbind(index_df, upenn02_20230314_ind)
upenn02_20230314 = missing_inpute("UPENN02", "2023-03-14", upenn02_20230314)
plot(upenn02_20230314$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn02_20230314)


# HYP08 2022-08-11
hyp08_20220811 = total_df %>% filter(subject == "HYP08", day == "2022-08-11")
hyp08_20220811 = remove_data("2022-08-10 22:50:00", "2022-08-11 04:55:00", hyp08_20220811)
hyp08_20220811_c = hyp08_20220811 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20220811_c)
hyp08_20220811_ind = missing_inpute("HYP08", "2022-08-11", hyp08_20220811, type = "index")
index_df = rbind(index_df, hyp08_20220811_ind)
hyp08_20220811 = missing_inpute("HYP08", "2022-08-11", hyp08_20220811)
plot(hyp08_20220811$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20220811)


# HYP01 2022-09-10
hyp01_20220910 = total_df %>% filter(subject == "HYP01", day == "2022-09-10")
hyp01_20220910_c = hyp01_20220910 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20220910_c)
hyp01_20220910_ind = missing_inpute("HYP01", "2022-09-10", hyp01_20220910, type = "index")
index_df = rbind(index_df, hyp01_20220910_ind)
hyp01_20220910 = missing_inpute("HYP01", "2022-09-10", hyp01_20220910)
plot(hyp01_20220910$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20220910)


# HYP02 2022-10-28
hyp02_20221028 = total_df %>% filter(subject == "HYP02", day == "2022-10-28")
hyp02_20221028 = remove_data("2022-10-27 23:10:00", "2022-10-28 06:08:00", hyp02_20221028)
hyp02_20221028_c = hyp02_20221028 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20221028_c)
hyp02_20221028_ind = missing_inpute("HYP02", "2022-10-28", hyp02_20221028, type = "index")
index_df = rbind(index_df, hyp02_20221028_ind)
hyp02_20221028 = missing_inpute("HYP02", "2022-10-28", hyp02_20221028)
plot(hyp02_20221028$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20221028)


# UPENN02 2023-03-11
upenn02_20230311 = total_df %>% filter(subject == "UPENN02", day == "2023-03-11")
upenn02_20230311 = remove_data("2023-03-11 00:57:00", "2023-03-11 09:33:00", upenn02_20230311)
upenn02_20230311_c = upenn02_20230311 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn02_20230311_c)
upenn02_20230311_ind = missing_inpute("UPENN02", "2023-03-11", upenn02_20230311, type = "index")
index_df = rbind(index_df, upenn02_20230311_ind)
upenn02_20230311 = missing_inpute("UPENN02", "2023-03-11", upenn02_20230311)
plot(upenn02_20230311$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn02_20230311)


# HYP03 2022-10-23
hyp03_20221023 = total_df %>% filter(subject == "HYP03", day == "2022-10-23")
hyp03_20221023 = remove_data("2022-10-23 04:45:00", "2022-10-23 13:13:00", hyp03_20221023)
hyp03_20221023_c = hyp03_20221023 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp03_20221023_c)
hyp03_20221023_ind = missing_inpute("HYP03", "2022-10-23", hyp03_20221023, type = "index")
index_df = rbind(index_df, hyp03_20221023_ind)
hyp03_20221023 = missing_inpute("HYP03", "2022-10-23", hyp03_20221023)
plot(hyp03_20221023$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp03_20221023)


# HYP08 2022-08-12
hyp08_20220812 = total_df %>% filter(subject == "HYP08", day == "2022-08-12")
hyp08_20220812 = remove_data("2022-08-11 21:40:00", "2022-08-12 03:50:00", hyp08_20220812)
hyp08_20220812_c = hyp08_20220812 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20220812_c)
hyp08_20220812_ind = missing_inpute("HYP08", "2022-08-12", hyp08_20220812, type = "index")
index_df = rbind(index_df, hyp08_20220812_ind)
hyp08_20220812 = missing_inpute("HYP08", "2022-08-12", hyp08_20220812)
plot(hyp08_20220812$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20220812)


# UPENN01 2023-03-19
upenn01_20230319 = total_df %>% filter(subject == "UPENN01", day == "2023-03-19")
upenn01_20230319_c = upenn01_20230319 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn01_20230319_c)
upenn01_20230319_ind = missing_inpute("UPENN01", "2023-03-19", upenn01_20230319, type = "index")
index_df = rbind(index_df, upenn01_20230319_ind)
upenn01_20230319 = missing_inpute("UPENN01", "2023-03-19", upenn01_20230319)
plot(upenn01_20230319$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn01_20230319)


# HYP12 2022-10-20
hyp12_20221020 = total_df %>% filter(subject == "HYP12", day == "2022-10-20")
hyp12_20221020 = remove_data("2022-10-19 22:30:00", "2022-10-20 07:12:00", hyp12_20221020)
hyp12_20221020_c = hyp12_20221020 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp12_20221020_c)
hyp12_20221020_ind = missing_inpute("HYP12", "2022-10-20", hyp12_20221020, type = "index")
index_df = rbind(index_df, hyp12_20221020_ind)
hyp12_20221020 = missing_inpute("HYP12", "2022-10-20", hyp12_20221020)
plot(hyp12_20221020$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp12_20221020)


# HYP02 2023-02-09
hyp02_20230209 = total_df %>% filter(subject == "HYP02", day == "2023-02-09")
hyp02_20230209 = remove_data("2023-02-09 03:10:00", "2023-02-09 03:20:00", hyp02_20230209, type = "rm_interval")
hyp02_20230209_c = hyp02_20230209 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230209_c)
hyp02_20230209_ind = missing_inpute("HYP02", "2023-02-09", hyp02_20230209, type = "index")
index_df = rbind(index_df, hyp02_20230209_ind )
hyp02_20230209 = missing_inpute("HYP02", "2023-02-09", hyp02_20230209)
plot(hyp02_20230209$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230209)


# HYP07 2022-12-07
hyp07_20221207 = total_df %>% filter(subject == "HYP07", day == "2022-12-07")
hyp07_20221207_c = hyp07_20221207 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20221207_c)
hyp07_20221207_ind = missing_inpute("HYP07", "2022-12-07", hyp07_20221207, type = "index")
index_df = rbind(index_df, hyp07_20221207_ind)
hyp07_20221207 = missing_inpute("HYP07", "2022-12-07", hyp07_20221207)
plot(hyp07_20221207$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20221207)


# UPENN04 2023-02-25
upenn04_20230225 = total_df %>% filter(subject == "UPENN04", day == "2023-02-25")
upenn04_20230225_c = upenn04_20230225 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn04_20230225_c)
upenn04_20230225_ind = missing_inpute("UPENN04", "2023-02-25", upenn04_20230225, type = "index")
index_df = rbind(index_df, upenn04_20230225_ind)
upenn04_20230225 = missing_inpute("UPENN04", "2023-02-25", upenn04_20230225)
plot(upenn04_20230225$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn04_20230225)


# HYP12 2022-10-19
hyp12_20221019 = total_df %>% filter(subject == "HYP12", day == "2022-10-19")
hyp12_20221019 = remove_data("2022-10-18 23:10:00", "2022-10-19 09:54:00", hyp12_20221019)
hyp12_20221019_c = hyp12_20221019 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp12_20221019_c)
hyp12_20221019_ind = missing_inpute("HYP12", "2022-10-19", hyp12_20221019, type = "index")
index_df = rbind(index_df, hyp12_20221019_ind)
hyp12_20221019 = missing_inpute("HYP12", "2022-10-19", hyp12_20221019)
plot(hyp12_20221019$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp12_20221019)


# HYP02 2023-02-10
hyp02_20230210 = total_df %>% filter(subject == "HYP02", day == "2023-02-10")
hyp02_20230210 = remove_data("2023-02-10 02:00:00", "2023-02-10 02:10:00", hyp02_20230210, type = "rm_interval")
hyp02_20230210_c = hyp02_20230210 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230210_c)
hyp02_20230210_ind = missing_inpute("HYP02", "2023-02-10", hyp02_20230210, type = "index")
index_df = rbind(index_df, hyp02_20230210_ind)
hyp02_20230210 = missing_inpute("HYP02", "2023-02-10", hyp02_20230210)
plot(hyp02_20230210$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230210)

# HYP07 2022-12-08
hyp07_20221208 = total_df %>% filter(subject == "HYP07", day == "2022-12-08")
hyp07_20221208 = remove_data("2022-12-07 23:30:00", "2022-12-08 06:45:00", hyp07_20221208)
hyp07_20221208_c = hyp07_20221208 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20221208_c)
hyp07_20221208_ind = missing_inpute("HYP07", "2022-12-08", hyp07_20221208, type = "index")
index_df = rbind(index_df, hyp07_20221208_ind)
hyp07_20221208 = missing_inpute("HYP07", "2022-12-08", hyp07_20221208)
plot(hyp07_20221208$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20221208)


# HYP09 2022-08-19
hyp09_20220819 = total_df %>% filter(subject == "HYP09", day == "2022-08-19")
hyp09_20220819_c = hyp09_20220819 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20220819_c)
hyp09_20220819_ind = missing_inpute("HYP09", "2022-08-19", hyp09_20220819, type = "index")
index_df = rbind(index_df, hyp09_20220819_ind)
hyp09_20220819 = missing_inpute("HYP09", "2022-08-19", hyp09_20220819)
plot(hyp09_20220819$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20220819)


# HYP02 2023-02-11
hyp02_20230211 = total_df %>% filter(subject == "HYP02", day == "2023-02-11")
hyp02_20230211 = remove_data("2023-02-10 23:04:00", "2023-02-11 02:10:00", hyp02_20230211)
hyp02_20230211_c = hyp02_20230211 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230211_c)
hyp02_20230211_ind = missing_inpute("HYP02", "2023-02-11", hyp02_20230211, type = "index")
index_df = rbind(index_df, hyp02_20230211_ind)
hyp02_20230211 = missing_inpute("HYP02", "2023-02-11", hyp02_20230211)
plot(hyp02_20230211$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230211)


# HYP01 2023-01-07
hyp01_20230107 = total_df %>% filter(subject == "HYP01", day == "2023-01-07")
hyp01_20230107_c = hyp01_20230107 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20230107_c)
hyp01_20230107_ind = missing_inpute("HYP01", "2023-01-07", hyp01_20230107, type = "index")
index_df = rbind(index_df, hyp01_20230107_ind)
hyp01_20230107 = missing_inpute("HYP01", "2023-01-07", hyp01_20230107)
plot(hyp01_20230107$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20230107)


# HYP06 2022-08-03
hyp06_20220803 = total_df %>% filter(subject == "HYP06", day == "2022-08-03")
hyp06_20220803_c = hyp06_20220803 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp06_20220803_c)
hyp06_20220803_ind = missing_inpute("HYP06", "2022-08-03", hyp06_20220803, type = "index")
index_df = rbind(index_df, hyp06_20220803_ind)
hyp06_20220803 = missing_inpute("HYP06", "2022-08-03", hyp06_20220803)
plot(hyp06_20220803$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp06_20220803)


# HYP11 2022-09-21
hyp11_20220921 = total_df %>% filter(subject == "HYP11", day == "2022-09-21")
hyp11_20220921 = remove_data("2022-09-20 22:20:00", "2022-09-21 06:09:00", hyp11_20220921)
hyp11_20220921_c = hyp11_20220921 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp11_20220921_c)
hyp11_20220921_ind = missing_inpute("HYP11", "2022-09-21", hyp11_20220921, type = "index")
index_df = rbind(index_df, hyp11_20220921_ind)
hyp11_20220921 = missing_inpute("HYP11", "2022-09-21", hyp11_20220921)
plot(hyp11_20220921$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp11_20220921)


# HYP12 2022-10-18
hyp12_20221018 = total_df %>% filter(subject == "HYP12", day == "2022-10-18")
hyp12_20221018 = remove_data("2022-10-18 00:40:00", "2022-10-18 04:50:00", hyp12_20221018)
hyp12_20221018_c = hyp12_20221018 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp12_20221018_c)
hyp12_20221018_ind = missing_inpute("HYP12", "2022-10-18", hyp12_20221018, type = "index")
index_df = rbind(index_df, hyp12_20221018_ind)
hyp12_20221018 = missing_inpute("HYP12", "2022-10-18", hyp12_20221018)
plot(hyp12_20221018$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp12_20221018)


# HYP09 2022-08-17
hyp09_20220817 = total_df %>% filter(subject == "HYP09", day == "2022-08-17")
hyp09_20220817 = remove_data("2022-08-17 05:40:00", "2022-08-17 06:00:00", hyp09_20220817, type = "rm_interval")
hyp09_20220817_c = hyp09_20220817 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20220817_c)
hyp09_20220817_ind = missing_inpute("HYP09", "2022-08-17", hyp09_20220817, type = "index")
index_df = rbind(index_df, hyp09_20220817_ind)
hyp09_20220817 = missing_inpute("HYP09", "2022-08-17", hyp09_20220817)
plot(hyp09_20220817$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20220817)


# HYP02 2023-02-12
hyp02_20230212 = total_df %>% filter(subject == "HYP02", day == "2023-02-12")
hyp02_20230212 = remove_data("2023-02-12 00:36:00", "2023-02-12 05:00:00", hyp02_20230212)
hyp02_20230212_c = hyp02_20230212 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp02_20230212_c)
hyp02_20230212_ind = missing_inpute("HYP02", "2023-02-12", hyp02_20230212, type = "index")
index_df = rbind(index_df, hyp02_20230212_ind)
hyp02_20230212 = missing_inpute("HYP02", "2023-02-12", hyp02_20230212)
plot(hyp02_20230212$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp02_20230212)


# HYP07 2022-08-12
hyp07_20220812 = total_df %>% filter(subject == "HYP07", day == "2022-08-12")
hyp07_20220812_c = hyp07_20220812 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20220812_c)
hyp07_20220812_ind = missing_inpute("HYP07", "2022-08-12", hyp07_20220812, type = "index")
index_df = rbind(index_df, hyp07_20220812_ind)
hyp07_20220812 = missing_inpute("HYP07", "2022-08-12", hyp07_20220812)
plot(hyp07_20220812$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20220812)


# HYP03 2022-10-25
hyp03_20221025 = total_df %>% filter(subject == "HYP03", day == "2022-10-25")
hyp03_20221025 = remove_data("2022-10-25 04:40:00", "2022-10-25 12:12:00", hyp03_20221025)
hyp03_20221025_c = hyp03_20221025 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp03_20221025_c)
hyp03_20221025_ind = missing_inpute("HYP03", "2022-10-25", hyp03_20221025, type = "index")
index_df = rbind(index_df, hyp03_20221025_ind)
hyp03_20221025 = missing_inpute("HYP03", "2022-10-25", hyp03_20221025)
plot(hyp03_20221025$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp03_20221025)


# HYP08 2022-08-13
hyp08_20220813 = total_df %>% filter(subject == "HYP08", day == "2022-08-13")
hyp08_20220813 = remove_data("2022-08-12 22:00:00", "2022-08-13 05:20:00", hyp08_20220813)
hyp08_20220813_c = hyp08_20220813 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20220813_c)
hyp08_20220813_ind = missing_inpute("HYP08", "2022-08-13", hyp08_20220813, type = "index")
index_df = rbind(index_df, hyp08_20220813_ind)
hyp08_20220813 = missing_inpute("HYP08", "2022-08-13", hyp08_20220813)
hyp08_20220813_ind = missing_inpute("HYP08", "2022-08-13", hyp08_20220813, type = "index")
plot(hyp08_20220813$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20220813)


# HYP06 2022-08-04
hyp06_20220804 = total_df %>% filter(subject == "HYP06", day == "2022-08-04")
hyp06_20220804_c = hyp06_20220804 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp06_20220804_c)
hyp06_20220804_ind = missing_inpute("HYP06", "2022-08-04", hyp06_20220804, type = "index")
index_df = rbind(index_df, hyp06_20220804_ind)
hyp06_20220804 = missing_inpute("HYP06", "2022-08-04", hyp06_20220804)
plot(hyp06_20220804$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp06_20220804)


# HYP09 2022-08-18
hyp09_20220818 = total_df %>% filter(subject == "HYP09", day == "2022-08-18")
hyp09_20220818_c = hyp09_20220818 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20220818_c)
hyp09_20220818_ind = missing_inpute("HYP09", "2022-08-18", hyp09_20220818, type = "index")
index_df = rbind(index_df, hyp09_20220818_ind)
hyp09_20220818 = missing_inpute("HYP09", "2022-08-18", hyp09_20220818)
plot(hyp09_20220818$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20220818)


# HYP13 2023-03-03
hyp13_20230303 = total_df %>% filter(subject == "HYP13", day == "2023-03-03")
hyp13_20230303 = remove_data("2023-03-02 23:50:00", "2023-03-03 08:10:00", hyp13_20230303)
hyp13_20230303_c = hyp13_20230303 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230303_c)
hyp13_20230303_ind = missing_inpute("HYP13", "2023-03-03", hyp13_20230303, type = "index")
index_df = rbind(index_df, hyp13_20230303_ind)
hyp13_20230303 = missing_inpute("HYP13", "2023-03-03", hyp13_20230303)
plot(hyp13_20230303$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp13_20230303)


# HYP08 2022-12-09
hyp08_20221209 = total_df %>% filter(subject == "HYP08", day == "2022-12-09")
hyp08_20221209 = remove_data("2022-12-09 01:00:00", "2022-12-09 01:30:00", hyp08_20221209, type = "rm_interval")
hyp08_20221209_c = hyp08_20221209 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20221209_c)
hyp08_20221209_ind = missing_inpute("HYP08", "2022-12-09", hyp08_20221209, type = "index")
index_df = rbind(index_df, hyp08_20221209_ind)
hyp08_20221209 = missing_inpute("HYP08", "2022-12-09", hyp08_20221209)
plot(hyp08_20221209$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20221209)


# HYP10 2022-12-15
hyp10_20221215 = total_df %>% filter(subject == "HYP10", day == "2022-12-15")
hyp10_20221215 = remove_data("2022-12-15 01:03:00", "2022-12-15 09:30:00", hyp10_20221215)
hyp10_20221215 = remove_data("2022-12-15 08:40:00", "2022-12-15 08:50:00", hyp10_20221215, type = "rm_interval")
hyp10_20221215_c = hyp10_20221215 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp10_20221215_c)
hyp10_20221215_ind = missing_inpute("HYP10", "2022-12-15", hyp10_20221215, type = "index")
index_df = rbind(index_df, hyp10_20221215_ind)
hyp10_20221215 = missing_inpute("HYP10", "2022-12-15", hyp10_20221215)
plot(hyp10_20221215$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp10_20221215)


# HYP07 2022-08-11
hyp07_20220811 = total_df %>% filter(subject == "HYP07", day == "2022-08-11")
hyp07_20220811 = remove_data("2022-08-10 23:09:00", "2022-08-11 05:48:00", hyp07_20220811)
hyp07_20220811 = remove_data("2022-08-11 05:20:00", "2022-08-11 05:25:00", hyp07_20220811, type = "rm_interval")
hyp07_20220811_c = hyp07_20220811 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp07_20220811_c)
hyp07_20220811_ind = missing_inpute("HYP07", "2022-08-11", hyp07_20220811, type = "index")
index_df = rbind(index_df, hyp07_20220811_ind)
hyp07_20220811 = missing_inpute("HYP07", "2022-08-11", hyp07_20220811)
plot(hyp07_20220811$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp07_20220811)


# HYP08 2023-03-30
hyp08_20230330 = total_df %>% filter(subject == "HYP08", day == "2023-03-30")
hyp08_20230330 = remove_data("2023-03-29 23:25:00", "2023-03-30 06:57:00", hyp08_20230330)
hyp08_20230330 = remove_data("2023-03-30 02:10:00", "2023-03-30 02:48:00", hyp08_20230330, type = "rm_interval")
hyp08_20230330_c = hyp08_20230330 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20230330_c)
hyp08_20230330_ind = missing_inpute("HYP08", "2023-03-30", hyp08_20230330, type = "index")
index_df = rbind(index_df, hyp08_20230330_ind)
hyp08_20230330 = missing_inpute("HYP08", "2023-03-30", hyp08_20230330)
plot(hyp08_20230330$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20230330)


# HYP08 2023-04-01
hyp08_20230401 = total_df %>% filter(subject == "HYP08", day == "2023-04-01")
hyp08_20230401 = remove_data("2023-03-31 21:45:00", "2023-04-01 07:02:00", hyp08_20230401)
hyp08_20230401_c = hyp08_20230401 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20230401_c)
hyp08_20230401_ind = missing_inpute("HYP08", "2023-04-01", hyp08_20230401, type = "index")
index_df = rbind(index_df, hyp08_20230401_ind)
hyp08_20230401 = missing_inpute("HYP08", "2023-04-01", hyp08_20230401)
plot(hyp08_20230401$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20230401)


# HYP13 2023-03-04
hyp13_20230304 = total_df %>% filter(subject == "HYP13", day == "2023-03-04")
hyp13_20230304 = remove_data("2023-03-03 23:40:00", "2023-03-04 08:35:00", hyp13_20230304)
hyp13_20230304_c = hyp13_20230304 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230304_c)
hyp13_20230304_ind = missing_inpute("HYP13", "2023-03-04", hyp13_20230304, type = "index")
index_df = rbind(index_df, hyp13_20230304_ind)
hyp13_20230304 = missing_inpute("HYP13", "2023-03-04", hyp13_20230304)
plot(hyp13_20230304$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp13_20230304)


# UPENN04 2023-02-23
upenn04_20230223 = total_df %>% filter(subject == "UPENN04", day == "2023-02-23")
upenn04_20230223 = remove_data("2023-02-22 23:08:00", "2023-02-23 07:02:00", upenn04_20230223)
upenn04_20230223 = remove_data("2023-02-23 01:40:00", "2023-02-23 02:00:00", upenn04_20230223, type = "rm_interval")
upenn04_20230223 = remove_data("2023-02-23 04:50:00", "2023-02-23 05:05:00", upenn04_20230223, type = "rm_interval")
upenn04_20230223_c = upenn04_20230223 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn04_20230223_c)
upenn04_20230223_ind = missing_inpute("UPENN04", "2023-02-23", upenn04_20230223, type = "index")
index_df = rbind(index_df, upenn04_20230223_ind)
upenn04_20230223 = missing_inpute("UPENN04", "2023-02-23", upenn04_20230223)
plot(upenn04_20230223$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn04_20230223)


# UPENN04 2023-02-24
upenn04_20230224 = total_df %>% filter(subject == "UPENN04", day == "2023-02-24")
upenn04_20230224 = remove_data("2023-02-23 22:40:00", "2023-02-24 04:42:00", upenn04_20230224)
upenn04_20230224 = remove_data("2023-02-24 00:40:00", "2023-02-24 00:45:00", upenn04_20230224, type = "rm_interval")
upenn04_20230224_c = upenn04_20230224 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, upenn04_20230224_c)
upenn04_20230224_ind = missing_inpute("UPENN04", "2023-02-24", upenn04_20230224, type = "index")
index_df = rbind(index_df, upenn04_20230224_ind)
upenn04_20230224 = missing_inpute("UPENN04", "2023-02-24", upenn04_20230224)
plot(upenn04_20230224$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, upenn04_20230224)


# HYP01 2022-05-28
hyp01_20220528 = total_df %>% filter(subject == "HYP01", day == "2022-05-28")
hyp01_20220528_c = hyp01_20220528 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20220528_c)
hyp01_20220528_ind = missing_inpute("HYP01", "2022-05-28", hyp01_20220528, type = "index")
index_df = rbind(index_df, hyp01_20220528_ind)
hyp01_20220528 = missing_inpute("HYP01", "2022-05-28", hyp01_20220528)
plot(hyp01_20220528$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20220528)


# HYP08 2022-12-08
hyp08_20221208 = total_df %>% filter(subject == "HYP08", day == "2022-12-08")
hyp08_20221208_c = hyp08_20221208 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20221208_c)
hyp08_20221208_ind = missing_inpute("HYP08", "2022-12-08", hyp08_20221208, type = "index")
index_df = rbind(index_df, hyp08_20221208_ind)
hyp08_20221208 = missing_inpute("HYP08", "2022-12-08", hyp08_20221208)
plot(hyp08_20221208$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20221208)



# HYP13 2023-03-02
hyp13_20230302 = total_df %>% filter(subject == "HYP13", day == "2023-03-02")
hyp13_20230302_c = hyp13_20230302 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp13_20230302_c)
hyp13_20230302_ind = missing_inpute("HYP13", "2023-03-02", hyp13_20230302, type = "index")
index_df = rbind(index_df, hyp13_20230302_ind)
hyp13_20230302= missing_inpute("HYP13", "2023-03-02", hyp13_20230302)
plot(hyp13_20230302$PCO2_DC, type = 'l') 
miss_df = rbind(miss_df, hyp13_20230302)


# HYP08 2023-03-31
hyp08_20230331 = total_df %>% filter(subject == "HYP08", day == "2023-03-31")
hyp08_20230331_c = hyp08_20230331 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20230331_c)
hyp08_20230331_ind = missing_inpute("HYP08", "2023-03-31", hyp08_20230331, type = "index")
index_df = rbind(index_df, hyp08_20230331_ind)
hyp08_20230331 = missing_inpute("HYP08", "2023-03-31", hyp08_20230331)
plot(hyp08_20230331$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20230331)


# HYP08 2022-12-10
hyp08_20221210 = total_df %>% filter(subject == "HYP08", day == "2022-12-10")
hyp08_20221210 = remove_data("2022-12-09 21:25:00", "2022-12-10 06:50:00", hyp08_20221210)
hyp08_20221210_c = hyp08_20221210 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp08_20221210_c)
hyp08_20221210_ind = missing_inpute("HYP08", "2022-12-10", hyp08_20221210, type = "index")
index_df = rbind(index_df, hyp08_20221210_ind)
hyp08_20221210 = missing_inpute("HYP08", "2022-12-10", hyp08_20221210)
plot(hyp08_20221210$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp08_20221210)


# HYP10 2022-08-25
hyp10_20220825 = total_df %>% filter(subject == "HYP10", day == "2022-08-25")
hyp10_20220825 = remove_data("2022-08-25 03:20:00", "2022-08-25 09:40:00", hyp10_20220825)
hyp10_20220825_c = hyp10_20220825 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp10_20220825_c)
hyp10_20220825_ind = missing_inpute("HYP10", "2022-08-25", hyp10_20220825, type = "index")
index_df = rbind(index_df, hyp10_20220825_ind)
hyp10_20220825 = missing_inpute("HYP10", "2022-08-25", hyp10_20220825)
plot(hyp10_20220825$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp10_20220825)


# HYP10 2022-08-27
hyp10_20220827 = total_df %>% filter(subject == "HYP10", day == "2022-08-27")
hyp10_20220827_c = hyp10_20220827 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp10_20220827_c)
hyp10_20220827_ind = missing_inpute("HYP10", "2022-08-27", hyp10_20220827, type = "index")
index_df = rbind(index_df, hyp10_20220827_ind)
hyp10_20220827 = missing_inpute("HYP10", "2022-08-27", hyp10_20220827)
plot(hyp10_20220827$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp10_20220827)


# HYP01 2022-05-26
hyp01_20220526 = total_df %>% filter(subject == "HYP01", day == "2022-05-26")
hyp01_20220526 = remove_data("2022-05-25 22:31:00", "2022-05-25 22:34:00", hyp01_20220526, type = "rm_interval")
hyp01_20220526_c = hyp01_20220526 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp01_20220526_c)
hyp01_20220526_ind = missing_inpute("HYP01", "2022-05-26", hyp01_20220526, type = "index")
index_df = rbind(index_df, hyp01_20220526_ind)
hyp01_20220526 = missing_inpute("HYP01", "2022-05-26", hyp01_20220526)
plot(hyp01_20220526$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp01_20220526)


# HYP06 2022-08-02
hyp06_20220802 = total_df %>% filter(subject == "HYP06", day == "2022-08-02")
hyp06_20220802 = remove_data("2022-08-02 00:25:00", "2022-08-02 09:07:00", hyp06_20220802)
hyp06_20220802_c = hyp06_20220802 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp06_20220802_c)
hyp06_20220802_ind = missing_inpute("HYP06", "2022-08-02", hyp06_20220802, type = "index")
index_df = rbind(index_df, hyp06_20220802_ind)
hyp06_20220802 = missing_inpute("HYP06", "2022-08-02", hyp06_20220802)
plot(hyp06_20220802$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp06_20220802)


# HYP09 2022-12-21
hyp09_20221221 = total_df %>% filter(subject == "HYP09", day == "2022-12-21")
hyp09_20221221 = remove_data("2022-12-21 03:30:00", "2022-12-21 03:40:00", hyp09_20221221, type = "rm_interval")
hyp09_20221221_c = hyp09_20221221 %>% filter(!is.na(PCO2_DC))
miss_df_c = rbind(miss_df_c, hyp09_20221221_c)
hyp09_20221221_ind = missing_inpute("HYP09", "2022-12-21", hyp09_20221221, type = "index")
index_df = rbind(index_df, hyp09_20221221_ind)
hyp09_20221221 = missing_inpute("HYP09", "2022-12-21", hyp09_20221221)
plot(hyp09_20221221$PCO2_DC, type = 'l')
miss_df = rbind(miss_df, hyp09_20221221)


#write_csv(miss_df, "./data/qc_data/inpute_missing_data.csv")
#write_csv(index_df, "./data/qc_data/inpute_missing_data_index.csv")
write_csv(miss_df, "./data/qc_data/inpute_missing_data_c.csv") ## Without linear interpolation























