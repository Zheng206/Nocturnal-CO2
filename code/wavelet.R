library(wavethresh)

replace_missing = function(v){
  miss_id = which(is.na(v))
  if(length(miss_id) == 0){
    return(v)
  }else if(length(miss_id) == 1){
    if(miss_id == 1){v[miss_id] = v[miss_id + 1]}else if(miss_id == length(miss_id)){
      v[miss_id] = v[miss_id - 1]
    }else{
    v[miss_id] = mean(v[miss_id - 1], v[miss_id + 1])}
  }else{
  diff_id = diff(miss_id)
  if(length(which(!diff_id == 1)) == 0){
    if (miss_id[1] == 1){v[1:miss_id[length(miss_id)]] = v[miss_id[length(miss_id)] + 1]}else if(miss_id[length(miss_id)] == length(v)){
      v[miss_id[1]:length(v)] = v[miss_id[1] - 1]
    }else{
    for (id in miss_id){
      v[id] = mean(c(v[id - 1], v[miss_id[length(miss_id)] + 1]))
    }
    }
  }else{
  max_int = max(c(which(!diff_id == 1)[1], diff(which(!diff_id == 1)), length(diff_id) + 1 - which(!diff_id == 1)[length(which(!diff_id == 1))]))
  for (id in miss_id){
   v[id] = mean(na.omit(v[max(1, id - max_int/2): min(id + max_int/2, length(v))]))
  }
  }
  return(v)}
}


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

wavelet_pre = function(s, d, p, n, m){
  test = total_df %>% filter(subject == s, day == d) 
  pco2 = test[[m]]
  pco2 = replace_missing(pco2)
  pco2_enc = c(pco2, rep(pco2[length(pco2)], (2^(p + 1) - length(pco2))))
  best_f = family_selection(waveletFamily, pco2_enc)
  if(n == 1){
    return(best_f[[1]])
  }else if(n == 2){
    return(best_f[[2]])
  }else if(n == 3){
  return(best_f[[3]])}
}

roll_sd = function(x){
  len = length(x)
  sd_list = c()
  for (t in 1:(len - 30*60)){
    pco2_int = x[t:(t+30*60)]
    sd_int = sd(pco2_int)
    sd_list = c(sd_list, sd_int)
  }
  return(which(sd_list == max(sd_list)))
}

reconstruct_wave = function(thre, s, d, p, m, plot = FALSE, total_df, ...){
  test = total_df %>% filter(subject == s, day == d) 
  pco2 = test[[m]]
  pco2 = replace_missing(pco2)
  pco2_enc = c(pco2, rep(pco2[length(pco2)], (2^(p + 1) - length(pco2))))
  ywd = wd(pco2_enc, ...)
  ywd$D[ywd$D < thre] = 0
  if (plot == TRUE){
    plot(pco2, type = "l", xlab = "Seconds", ylab = "Measurement", title = "Before Transform", ylim = c(min(pco2), max(pco2)))
    recon = wr(ywd)[1:length(pco2)]
    max_st = roll_sd(recon)
    max_end = max_st + 30*60
    plot(recon, type = 'l', xlab = "Seconds", ylab = "Measurement", title = "After Transform", ylim = c(min(pco2), max(pco2)))
    abline(v = max_st, col = 'red', lty = 2)
    abline(v = max_end, col = 'red', lty = 2)
  }else{
    reconstructed_wv = wr(ywd)[1:length(pco2)]
    subject = rep(s, length(reconstructed_wv))
    day = rep(d, length(reconstructed_wv))
    time = seq(0, length(reconstructed_wv) - 1, 1)
    reconstructed_wv_df = data.frame(cbind(subject, day, time, reconstructed_wv))
    reconstructed_wv_df = tibble(reconstructed_wv_df)
    reconstructed_wv_df = reconstructed_wv_df %>% mutate (time = as.numeric(time), reconstructed_wv = as.numeric(reconstructed_wv))
    colnames(reconstructed_wv_df) = c("subject", "day", "time", paste0(m, "_recon"))
    return(reconstructed_wv_df)}
}

recon_wave = function(df, v, f, ...){
  reconstructed_pco2 = mclapply(1:nrow(df), function(i){
 reconstruct_wave(10, df[[i, "subject"]], df[[i,"day"]], df[[i,"power"]], "PCO2_DC", filter.number = v, family = f, ...)
}, mc.cores = 10) %>% bind_rows()
reconstructed_spo2 = mclapply(1:nrow(df), function(i){
 reconstruct_wave(30, df[[i, "subject"]], df[[i,"day"]], df[[i,"power"]], "SpO2", filter.number = v, family = f, ...)
}, mc.cores = 10) %>% bind_rows()
reconstructed_pr = mclapply(1:nrow(df), function(i){
 reconstruct_wave(40, df[[i, "subject"]], df[[i,"day"]], df[[i,"power"]], "PR", filter.number = v, family = f, ...)
}, mc.cores = 10) %>% bind_rows()
  total_df = total_df %>% left_join(reconstructed_pco2, by = c("subject", "day", "time")) %>% 
 left_join(reconstructed_spo2, by = c("subject", "day", "time")) %>% left_join(reconstructed_pr, by = c("subject", "day", "time"))
  write_csv(total_df, paste0("/home/zhengren/Desktop/Project/overnight_co2/data/overnight_data_all_subjects_recon_", f, v, ".csv"))
}

