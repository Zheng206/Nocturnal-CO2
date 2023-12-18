library(tidyverse)
library(tidyr)
library(readxl)

# Identical files check

## Data Stream Files
all_files = list.files("./data/All subjects", recursive = TRUE, full.names = TRUE)
old_files = all_files[grepl("20230510", all_files)]
old_files_names = sapply(old_files, function(x) basename(x), USE.NAMES = FALSE)
new_files = all_files[which(grepl("20230620", all_files))]
new_files_names = sapply(new_files, function(x) basename(x), USE.NAMES = FALSE)
same_files = intersect(old_files_names, new_files_names)

path_1 = paste0("./data/All subjects/20230620/", same_files)
path_2 = paste0("./data/All subjects/20230510/", same_files)
input = data.frame(cbind(path_1, path_2))

identical_test = function(p1, p2){
  file1 = read_excel(p1)
  file2 = read_excel(p2)
  dif = identical(file1, file2)
  if(dif == FALSE){return("different")}else{return("same")}
}

input$identical = sapply(1:nrow(input), function(i) identical_test(input[[i, "path_1"]], input[[i, "path_2"]]), USE.NAMES = FALSE)

## MDA Files
total_df = read_csv("./data/qc_data/total_subject.csv")
MDA.5.10 = read_excel("./data/MDA_DATA/MDA_DATA_V3_05.10.23.xls")
MDA.8.04 = read_excel("./data/MDA_DATA/MDA_DATA_V3_08.04.23.xls")
MDA.5.10 = MDA.5.10 %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "date_onset") %>% filter(grepl("HYP|MDA", PDF_Filename))
MDA.8.04 = MDA.8.04 %>% dplyr::select("PDF_Filename", "date_visit", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "date_onset") %>% filter(grepl("HYP|MDA", PDF_Filename))
colnames(MDA.5.10) = c("filename", "period", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "date_onset")
colnames(MDA.8.04) = c("filename", "period", "fvc", "mip", "visit_age", "height", "weight", "spot_co2",	"spot_o2",	"spot_hr", "alsfrs_10", "alsfrs_11", "date_onset")
MDA.5.10$subject = sapply(MDA.5.10$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
MDA.5.10$day  = as.character(MDA.5.10$period)
MDA.8.04$subject = sapply(MDA.8.04$filename, function(x) {if(grepl("HYP", x)){str_split(x,"--")[[1]][1]}else{
  item = str_split(x,"_")[[1]]
  upenn = item[which(grepl("UPENN", item))]
  upenn = str_sub(upenn, start = 1, end = 7)
  return(upenn)
}}, USE.NAMES = FALSE)
MDA.8.04$day  = as.character(MDA.8.04$period)
MDA.5.10 = MDA.5.10 %>% dplyr::select(subject, day, fvc, mip, spot_co2, alsfrs_10, alsfrs_11) %>% na.omit()
MDA.5.10$visit = sapply(MDA.5.10$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
MDA.8.04 = MDA.8.04 %>% dplyr::select(subject, day, fvc, mip, spot_co2, alsfrs_10, alsfrs_11) %>% na.omit()
MDA.8.04$visit = sapply(MDA.8.04$day, function(x) str_split(x, "-")[[1]][2], USE.NAMES = FALSE)
a = MDA.8.04 %>% left_join(MDA.5.10 %>% dplyr::select(subject, fvc, mip, spot_co2, alsfrs_10, alsfrs_11, visit), by =  c("subject", "visit"), suffix = c("8", "5")) %>% na.omit()
cols = c("fvc", "mip", "spot_co2", "alsfrs_10", "alsfrs_11")
mda_identical = function(x, df){
  v1 = paste0(x,"5")
  v2 = paste0(x,"8")
  sub_df = df[c(v1, v2)]
  dif = sum(sub_df[[1]]!=sub_df[[2]])
  if(dif > 0){return("different")}else{return("same")}
}
mda_identical = sapply(cols, function(x) mda_identical(x, a), USE.NAMES = FALSE)

