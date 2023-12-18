.libPaths(c("/misc/appl/R-4.1/lib64/R/library","/home/zhengren/Desktop/cluster_set_up/r_packages"))
install.packages("Rwave")
library(Rwave)
library(wavethresh)
library(parallel)
library(dplyr)
library(stringr)
library(tidyverse)
source("~/Desktop/Nocturnal_CO2/code/co2_function.R")
# Wavelet on Non-missing data
#source("/home/zhengren/Desktop/Project/overnight_co2/code/wavelet.R")
non_missing_df = read_csv("~/Desktop/Nocturnal_CO2/data/qc_data/total_subject.csv")
#non_missing_df$day = as.character(non_missing_df$day)
#non_missing_input = non_missing_df %>% group_by(subject, day) %>% summarize(count = n()) %>% ungroup()
#non_missing_input$power = sapply(non_missing_input$count, function(x) floor(log2(x)), USE.NAMES = FALSE)
#non_missing_input = non_missing_input %>% filter(power > 10)

test_night = non_missing_df %>% filter(subject == "HYP01", day == "2022-05-26") %>% pull(PCO2_DC)
power = floor(log2(length(test_night)))
test_night = test_night[1:2^(power - 1)]
orig_wave = data.frame(time = seq(1,length(test_night)), PCO2 = test_night)

#index_df = data.frame(period = c("period 1", "period 2"), xmin = c(0,2^8), xmax = c(2^8, 2^9), ymin = c(min(orig_wave$PCO2), min(orig_wave$PCO2)), ymax = c(max(orig_wave$PCO2), max(orig_wave$PCO2)))
ggplot(data = orig_wave, aes(x = time, y = PCO2)) +
  geom_line() +
  geom_vline(xintercept = c(2^0, 2^8, 2^9), linetype = "dashed", color = "red", size = 0.1) +
  labs(x = "time (s)")


ywd = wd(family = "DaubLeAsymm",data = test_night,filter.number = 7)
plot(ywd, scaling = "by.level")

C_6 = accessC(ywd, level = 6)
approx_wave = data.frame(time = seq(1,length(C_6)), PCO2 = C_6)
plot.ts(C_6)
D_5 = accessD(ywd, level = 5)
diff_wave = data.frame(time = seq(1,length(D_5)), PCO2 = D_5)
plot.ts(D_5)

ggplot(data = approx_wave, aes(x = time, y = PCO2)) +
  geom_line(color = "red") +
  labs(x = "period", y = "overall PCO2 level")

ggplot(data = diff_wave, aes(x = time, y = PCO2)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = mean(diff_wave$PCO2), linetype = "dashed", color = "steelblue") +
  labs(x = "period", y = "overall PCO2 level difference") 

ggplot(data = diff_wave, aes(x = PCO2)) +
  geom_density() +
  geom_vline(xintercept = quantile(diff_wave$PCO2)[2:4], linetype = "dashed", color = "red")


plot.ts(accessC(ywd, level = 10))
plot.ts(accessD(ywd, level = 10))
plot.ts(accessC(ywd, level = 9))
plot.ts(accessD(ywd, level = 9))
plot.ts(accessC(ywd, level = 8))
plot.ts(accessD(ywd, level = 8))
plot.ts(accessC(ywd, level = 7))
plot.ts(accessD(ywd, level = 7))
plot.ts(accessC(ywd, level = 6))
plot.ts(accessD(ywd, level = 6))
plot.ts(accessC(ywd, level = 5))
plot.ts(accessD(ywd, level = 5))
plot.ts(accessC(ywd, level = 4))
plot.ts(accessD(ywd, level = 4))
plot.ts(accessC(ywd, level = 3))
plot.ts(accessD(ywd, level = 3))
plot.ts(accessC(ywd, level = 2))
plot.ts(accessD(ywd, level = 2))
plot.ts(accessC(ywd, level = 1))
plot.ts(accessD(ywd, level = 1))

# Provide visual illustration
sub_df = total_df %>% filter(subject == "HYP01", day == "2022-05-26") 
test_night = sub_df %>% pull(PCO2_DC)
power = floor(log2(length(test_night)))
test_night_pad = c(test_night, rep(test_night[length(test_night)], 2^(power + 1)-length(test_night)))
ywd = wd(family = "DaubLeAsymm",data = test_night_pad,filter.number = 7)
par(mfrow = c(1,1))
C = accessC(ywd, level = 9)
D = accessD(ywd, level = 9)
C = remove_pad(C)
D = remove_pad(D)
plot(test_night, type = "l", xlab = "Time (seconds)", ylab = "Original Data Streams", col = "black")
plot.ts(C, xlab = "Coefficient Index", ylab = "Approximation Coefficients", col = "red")
plot.ts(D, xlab = "Coefficient Index", ylab = "Detail Coefficients", col = "darkgreen")



# thresholding
ywd_thre = threshold(ywd, policy="universal", by.level = TRUE) 
plot(ywd_thre, scaling = "by.level")
png("wr.png")
plot(wr(ywd_thre, start.level = 10)[1:length(test_night)], type = 'l')
dev.off()
plot(ywd_thre, scaling = "by.level")
plot(wr(ywd_thre)[1:length(test_night)], type = 'l')

# reconstruct certain level
re_level = ywd
for (i in c(1:12, 14)){
  accessC(re_level, level = i) = 0
  accessD(re_level, level = i) = 0
}


# wavelet packet
zwp = wp(family = "DaubLeAsymm",data = test_night_pad,filter.number = 7)
plot(zwp, color.force = TRUE)
getpacket(zwp, level = 11, index = 3)

for (i in 8:(power - 2)){
    ywd$D[which(ywd$D == accessD(ywd, level = 15))] = 0 
}

png("wr.png")
plot(wr(ywd)[1:length(test_night)], type = 'l')
dev.off()

plot(accessC(ywd, level = 6), type = 'l')








test = upenn01_20220920$PCO2_DC
power = floor(log2(length(test)))
test_night_pad = c(test, rep(test[length(test)], 2^(power + 1)-length(test)))
#draw.default(filter.number = 7, family = "DaubLeAsymm")
ywd = wd(family = "DaubLeAsymm",data = test_night_pad,filter.number = 7)
plot(ywd, scaling = "by.level")

ywd_thre = threshold(ywd, policy="universal", by.level = TRUE) 
plot(wr(ywd_thre, start.level = 10)[1:length(test)], type = 'l')

for (i in 8:power){
    ywd$D[which(ywd$D == accessD(ywd, level = i))] = 0 
}

plot(wr(ywd)[1:length(test)], type = 'l')
