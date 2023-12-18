library(tidyverse)
file_05 = list.files("./data/All subjects/20230510", recursive = TRUE, full.names = TRUE)
file_11 = list.files("./data/All subjects/20231127", recursive = TRUE, full.names = TRUE)
basename(file_05) %in% basename(file_11)

version_identical = function(old, new_list){
  a = read_csv(old)
  new = new_list[which(basename(new_list) == basename(old))]
  b = read_csv(new)
  return(all.equal(a,b))
}

check_result = sapply(1:length(file_05), function(i) version_identical(file_05[i], file_11), USE.NAMES = FALSE)

new_files = file_11[which(!basename(file_11) %in% basename(file_05))]
