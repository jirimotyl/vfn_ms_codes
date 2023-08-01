library(tidyverse)

data_prefinal <- read_csv("redcap_export.csv", 
                          na = c("NA", "NASK", "INV", "NAVU", "ASKU"), 
                          col_types= cols(birthday = col_date("%Y-%m-%d"), psycho_date = col_date("%Y-%m-%d")),
                          guess_max=12345)

#tidy data
##tag branch of data (SCG = arm_1, ASA/SET = arm_2, CIS/FEMS = arm_4)
data_prefinal$redcap_branch <- str_sub(data_prefinal$redcap_event_name, -5)

##filling demogrpahics info to all records
data_prefinal_01a <- filter(data_prefinal, redcap_branch !=  "arm_5")
data_prefinal_02a <- filter(data_prefinal, redcap_branch == "arm_5" & (is.na(redcap_repeat_instance) == T))
data_prefinal_02a <- select(data_prefinal_02a, record_id, surname, firstname, birthday, sex, later_hand)
data_prefinal_02b <- filter(data_prefinal, redcap_branch == "arm_5" & (is.na(redcap_repeat_instance) != T))
data_prefinal_02b <- select(data_prefinal_02b, -surname, -firstname, -birthday, -sex, -later_hand)

data_final_02 <- left_join(data_prefinal_02b, data_prefinal_02a, by=c("record_id"), multiple = "all", keep = F)

data_prefinal_01 <- 
  tibble(record_id = data_prefinal_01a$record_id, 
         redcap_branch = data_prefinal_01a$redcap_branch,
         redcap_repeat_instrument = data_prefinal_01a$redcap_repeat_instrument, 
         redcap_repeat_instance = data_prefinal_01a$redcap_repeat_instance, 
         firstname = data_prefinal_01a$firstname,
         surname = data_prefinal_01a$surname,
         birthday = data_prefinal_01a$birthday, 
         sex = data_prefinal_01a$sex,
         later_hand = data_prefinal_01a$later_hand,
         basic_demographic_data_complete = data_prefinal_01a$basic_demographic_data_complete,
  ) %>%
  filter(is.na(data_prefinal_01a$basic_demographic_data_complete)!=T) %>%
  left_join(data_prefinal_01a, by=c("record_id", "redcap_branch"), keep = F)

data_final_01 <- select(data_prefinal_01, 
                        -redcap_repeat_instrument.y,
                        -redcap_repeat_instance.y,
                        -firstname.y,
                        -surname.y,
                        -birthday.y, 
                        -sex.y,
                        -later_hand.y,
                        -basic_demographic_data_complete.y)
data_final_01 <- rename(data_final_01, 
                        redcap_repeat_instrument = redcap_repeat_instrument.x,
                        redcap_repeat_instance = redcap_repeat_instance.x,
                        firstname = firstname.x,
                        surname = surname.x,
                        birthday = birthday.x, 
                        sex = sex.x,
                        later_hand = later_hand.x,
                        basic_demographic_data_complete = basic_demographic_data_complete.x)


data_final <- rbind(data_final_01, data_final_02)

##necessary recoding
data_final$id_participant <- data_final$record_id
data_final$age <- as.numeric(NA)
data_final <- rename(data_final, cowat_k_total = cowat_k_total_calc,
                     cowat_p_total = cowat_p_total_calc,
                     cowat_s_total = cowat_s_total_calc,
                     cowat_n_total = cowat_n_total_calc,
                     dkst_cs_hs = dkefs_freesort_cstotal,
                     dkst_ds_hs = dkefs_freesort_descr_total)
data_final$sex <- ifelse(data_final$sex == "F", "female",ifelse(data_final$sex == "M","male", NA))
data_final$later_hand <- ifelse(data_final$later_hand == 1 | data_final$later_hand == 2, "right",
                                ifelse(data_final$later_hand == 3 | data_final$later_hand == 4,"left",
                                       ifelse(data_final$later_hand == 5 | data_final$later_hand == 6,"other",NA
                                       )))
remove(data_final_01)
remove(data_final_02)
remove(data_prefinal)
remove(data_prefinal_01)
remove(data_prefinal_01a)
remove(data_prefinal_02a)
remove(data_prefinal_02b)

##merge all data into one variable (calculated and non-calculated fields)
for (i in 1:nrow(data_final)) {
  if(is.na(data_final$pasat_total[i])==T & is.na(data_final$pasat_total_calculated[i])!=T) {
    data_final$pasat_total[i] <- data_final$pasat_total_calculated[i]
  }
  if(is.na(data_final$sdmt90_total[i])==T & is.na(data_final$sdmt90_total_calculated[i])!=T) {
    data_final$sdmt90_total[i] <- data_final$sdmt90_total_calculated[i]
  }
  if(is.na(data_final$cvlt_total[i])==T & is.na(data_final$cvlt_total_calculated[i])!=T) {
    data_final$cvlt_total[i] <- data_final$cvlt_total_calculated[i]
  }
  if(is.na(data_final$bvmt_total[i])==T & is.na(data_final$bvmt_total_calculated[i])!=T) {
    data_final$bvmt_total[i] <- data_final$bvmt_total_calculated[i]
  }
  if(is.na(data_final$ravlt_total[i])==T & is.na(data_final$ravlt_total_calc[i])!=T) {
    data_final$ravlt_total[i] <- data_final$ravlt_total_calc[i]
  }
  if(is.na(data_final$cowat_kps_total[i])==T & is.na(data_final$cowat_kps_total_calc[i])!=T) {
    data_final$cowat_kps_total[i] <- data_final$cowat_kps_total_calc[i]
  }
  if(is.na(data_final$cowat_nkp_total[i])==T & is.na(data_final$cowat_nkp_total_calc[i])!=T) {
    data_final$cowat_nkp_total[i] <- data_final$cowat_nkp_total_calc[i]
  }
  if(is.na(data_final$cowat_animal_total[i])==T & is.na(data_final$cowat_animal_total_calc[i])!=T) {
    data_final$cowat_animal_total[i] <- data_final$cowat_animal_total_calc[i]
  }
  if(is.na(data_final$cowat_vege_total[i])==T & is.na(data_final$cowat_vege_total_calc[i])!=T) {
    data_final$cowat_vege_total[i] <- data_final$cowat_vege_total_calc[i]
  }
  if(is.na(data_final$fss_total[i])==T & is.na(data_final$fss_total_calculated[i])!=T) {
    data_final$fss_total[i] <- data_final$fss_total_calculated[i]
  }
  if(is.na(data_final$fss_total_re[i])==T & is.na(data_final$fss_total_re_calculated[i])!=T) {
    data_final$fss_total_re[i] <- data_final$fss_total_re_calculated[i]
  }
  if(is.na(data_final$fss_total_krupp[i])==T & is.na(data_final$fss_total_krupp_calc[i])!=T) {
    data_final$fss_total_krupp[i] <- data_final$fss_total_krupp_calc[i]
  }
  if(is.na(data_final$fss_total_v02[i])==T & is.na(data_final$fss_total_v02_calc[i])!=T) {
    data_final$fss_total_v02[i] <- data_final$fss_total_v02_calc[i]
  }
  if(is.na(data_final$bdi_total[i])==T & is.na(data_final$bdi_total_calculated[i])!=T) {
    data_final$bdi_total[i] <- data_final$bdi_total_calculated[i]
  }
  if(is.na(data_final$hads_depres_total[i])==T & is.na(data_final$hads_depres_total_calc[i])!=T) {
    data_final$hads_depres_total[i] <- data_final$hads_depres_total_calc[i]
  }
  if(is.na(data_final$hads_anxiet_total[i])==T & is.na(data_final$hads_anxiet_total_calc[i])!=T) {
    data_final$hads_anxiet_total[i] <- data_final$hads_anxiet_total_calc[i]
  }
  if(is.na(data_final$msnq_total[i])==T & is.na(data_final$msnq_total_calculated[i])!=T) {
    data_final$msnq_total[i] <- data_final$msnq_total_calculated[i]
  }
  if(is.na(data_final$stai_x1_total[i])==T & is.na(data_final$stai_x1_total_calculated[i])!=T) {
    data_final$stai_x1_total[i] <- data_final$stai_x1_total_calculated[i]
  }
  if(is.na(data_final$stai_x2_total[i])==T & is.na(data_final$stai_x2_total_calculated[i])!=T) {
    data_final$stai_x2_total[i] <- data_final$stai_x2_total_calculated[i]
  }
  if(is.na(data_final$dymus_total[i])==T & is.na(data_final$dymus_total_calculated[i])!=T) {
    data_final$dymus_total[i] <- data_final$dymus_total_calculated[i]
  }
}


##merge FSS versions in one variable
fss_errors_01 <- filter(data_final, is.na(fss_total)==F & is.na(fss_total_re)==F)
fss_errors_02 <- filter(data_final, is.na(fss_total)==F & is.na(fss_total_v02)==F)
fss_errors_03 <- filter(data_final, is.na(fss_total)==F & is.na(fss_total_krupp)==F)
fss_errors_04 <- filter(data_final, is.na(fss_total_re)==F & is.na(fss_total_v02)==F)
fss_errors_05 <- filter(data_final, is.na(fss_total_re)==F & is.na(fss_total_krupp)==F)
fss_errors_06 <- filter(data_final, is.na(fss_total_v02)==F & is.na(fss_total_krupp)==F)

fss_error_ids <- c(fss_errors_01$record_id, fss_errors_02$record_id, fss_errors_03$record_id, fss_errors_04$record_id, fss_errors_05$record_id, fss_errors_06$record_id)

fss_merge_ready <- filter(data_final, !record_id %in% c(fss_error_ids))

vars <- c("fss_total", "fss_total_re", "fss_total_v02", "fss_total_krupp")

fss_final <- unite(fss_merge_ready, col = fss_total_merged, all_of(vars), remove = F, na.rm = T)
data_final$fss_total_merged <- as.numeric(fss_final$fss_total_merged)
data_final$fss_version <- ifelse(is.na(data_final$fss_total) != T, 1,
                                 ifelse(is.na(data_final$fss_total_re) != T, 2,
                                        ifelse(is.na(data_final$fss_total_v02) != T, 3,
                                               ifelse(is.na(data_final$fss_total_krupp) != T, 4, NA
                                               ))))


remove(fss_errors_01)
remove(fss_errors_02)
remove(fss_errors_03)
remove(fss_errors_04)
remove(fss_errors_05)
remove(fss_errors_06)
remove(fss_final)
remove(fss_merge_ready)
remove(vars)
remove(fss_error_ids)



#Count cases for Leavitt
##Neuroperformance measures
###SDMT (and other BICAMS measures if available)
###Timed 25-foot walk test
###Nine-hole peg test
##PROs
###Depression
###Anxiety
###Fatigue

##All data
data_selected <- filter(data_final,
       !is.na(sdmt90_total) &
         !is.na(twentyfive_foot_01) &
         !is.na(twentyfive_foot_02) &
         !is.na(ninehp_dom_01) &
         !is.na(ninehp_dom_02) &
         !is.na(ninehp_nedom_01) &
         !is.na(ninehp_nedom_02) &    
         !is.na(fss_total_merged) &
         (!is.na(bdi_total) | !is.na(hads_depres_total)) &
         (!is.na(stai_x1_total) | !is.na(hads_anxiet_total)) 
       )

length(unique(data_selected$record_id))


##SCG only
data_selected_scg <- filter(data_final,
                        !is.na(sdmt90_total) &
                          !is.na(twentyfive_foot_01) &
                          !is.na(twentyfive_foot_02) &
                          !is.na(ninehp_dom_01) &
                          !is.na(ninehp_dom_02) &
                          !is.na(ninehp_nedom_01) &
                          !is.na(ninehp_nedom_02) &    
                          !is.na(fss_total_merged) &
                          (!is.na(bdi_total) | !is.na(hads_depres_total)) &
                          (!is.na(stai_x1_total) | !is.na(hads_anxiet_total)) &
                          redcap_branch == "arm_1"
                          )

length(unique(data_selected_scg$record_id))

