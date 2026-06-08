
PIS_FA_disp <- read_csv("/conf/FolicAcid/data/extracts/PIS_FA_bnf_cohort_201023_disp.csv") %>% clean_names()

PIS_FA_prescr <- read_csv("/conf/FolicAcid/data/extracts/PIS_FA_bnf_cohort_201023_prescr_250925.csv")%>% clean_names()

newPIS_FA_disp <- read_csv("/conf/FolicAcid/data/extracts/PIS_FA_bnf_cohort_newPIS_disp.csv") %>% clean_names()
newPIS_FA_prescr <- read_csv("/conf/FolicAcid/data/extracts/PIS_FA_bnf_cohort_newPIS_presc.csv")%>% clean_names()

new_PIS_all <- left_join(newPIS_FA_prescr, newPIS_FA_disp, by=("claim_form_scan_reference_no"))
PIS_FA_all <- left_join(PIS_FA_prescr, PIS_FA_disp, by = c( "pr_form_scan_reference_no"="di_form_scan_reference_no"))

new_PIS_all <-new_PIS_all  %>% rename(dispensed_quantity=  claim_di_number_of_dispensed_items, 
                                      form_scan_reference_no= claim_form_scan_reference_no, 
                                      pr_dcvp_electronic_flag = claim_dcvp_electronic_flag.x, 
                                      di_dcvp_electronic_flag= claim_dcvp_electronic_flag.y,
                                      number_of_prescribed_items=claim_pr_number_of_prescribed_items)

  rm(PIS_FA_disp, PIS_FA_prescr, newPIS_FA_disp, newPIS_FA_prescr)
gc()
##count scan nos
scan_counts <- PIS_FA_all %>% group_by(pr_form_scan_reference_no) %>% count() %>% filter(n>1)
duplicates <- PIS_FA_all %>% filter(pr_form_scan_reference_no %in% scan_counts$pr_form_scan_reference_no)

##DVCP electronic flag =N means default date
PIS_FA_all <- PIS_FA_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                    di_dcvp_electronic_flag=="U" ~1, T~0), 
                                    disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard dispensed too long after prescribed. 
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>%
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                                                  T~disp_date))%>%
  ##additional rule to deal with unflagged default dates
    mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                   T~date_to_use))
           
new_PIS_all <- new_PIS_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                    di_dcvp_electronic_flag=="U" ~1, T~0), 
                                    disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>% 
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard dispensed too long after prescribed. 
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                 T~disp_date)) %>%
  ##additional rule to deal with unflagged default dates
  mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                 T~date_to_use))


PIS_hdFA_all <- PIS_FA_all %>% filter(prescribed_strength !=400 &  prescribed_strength !=80)
newPIS_hdFA_all <- new_PIS_all %>% filter(prescribed_strength !=400 & prescribed_strength !=80)

#date in different formats - sort it
PIS_hdFA_all <- PIS_hdFA_all%>% mutate(date_string = substr(date_to_use,1,10)) %>%
  mutate(date_to_use =  as.Date(date_string,  "%Y/%m/%d" ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(hdFA_PIS = 1) %>%
   filter(date_to_use <= as.Date("2023-04-20"))
newPIS_hdFA_all <- new_PIS_all %>% 
  mutate(date_to_use =  as.Date(date_to_use,  "%Y/%m/%d" ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(hdFA_PIS = 1) 
PIS_hdFA_all <- bind_rows(PIS_hdFA_all, newPIS_hdFA_all)
#save version with all vars
saveRDS(PIS_hdFA_all,paste0(folder_data_path, "processed_extracts/PIS_hdFA_processed.rds") )

PIS_hdFA_all <- PIS_hdFA_all%>%
  mutate(hdFA_PIS = 1) %>%
  select(mother_upi, date_to_use, hdFA_PIS)


#saveRDS(PIS_hdFA_all,paste0(folder_data_path, "processed_extracts/PIS_hdFA_processed.rds") )

slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
names(slipbd1)
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)


###join pregs and hdfa and just keep ones with a flag#
pregs_hdfa <- left_join(slipbd1, PIS_hdFA_all) %>% filter(hdFA_PIS==1)

###now determine whether the hdfa prescription is relevant to this pregnancy.
## from 12 weeks prior to conception date to 12weeks gestation (ie 10wks after conception)
##also flag any in pregnancy including after 12 wks
pregs_hdfa <- pregs_hdfa %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception) + (7*10)) %>%
  mutate(hdFA_PIS_to_conception = case_when(date_to_use >= start_window & date_to_use <  as.Date(est_date_conception)  ~1 , T~0), 
         hdFA_PIS_to_12wks = case_when(date_to_use >= as.Date(est_date_conception) & date_to_use < end_window ~1 , T~0), 
         hdFA_PIS_in_preg = case_when(date_to_use >= start_window & date_to_use <= date_end_pregnancy ~1 , T~0), 
         hdFA_PIS_after_12wks=case_when(date_to_use >= end_window & date_to_use <= date_end_pregnancy ~1 , T~0)  ) 
table(pregs_hdfa$hdFA_PIS_in_preg, pregs_hdfa$hdFA_PIS_to_12wks)

##filter to remove prescriptiosn not relewvant to pregnancy
pregs_hdfa <- pregs_hdfa %>% filter(hdFA_PIS_in_preg==1)

##metrics wanted for cohort
#first relevant date for hdFA in pregnancy
#last prescription in pregnancy
##N prescriptions <12 weeks
#N prescriptions total in pregnancy

hdfa_summary <- pregs_hdfa %>%
  group_by(pregnancy_id, mother_upi) %>%
  mutate(n_hdfa_12 = sum(hdFA_PIS_to_12wks),n_hdfa_any = sum(hdFA_PIS_in_preg )) %>% 
  summarise(est_date_conception = first(est_date_conception),
            first_prescr = min_(date_to_use), 
            last_prescr = max_(date_to_use),
            hdFA_PIS_to_conception  = max_(hdFA_PIS_to_conception ),
            hdFA_PIS_to_12wks = max_(hdFA_PIS_to_12wks), 
            hdFA_PIS_in_preg = max_(hdFA_PIS_in_preg),
            hdFA_PIS_after_12wks = max_(hdFA_PIS_after_12wks),
            n_hdfa_12 = max(n_hdfa_12), 
            n_hdfa_any = max(n_hdfa_any)) %>% ungroup() 

hdfa_summary <- hdfa_summary  %>% 
  mutate(wks_first_presc = floor((as.Date(first_prescr) -as.Date(est_date_conception))/7)+2)


saveRDS(hdfa_summary, paste0(folder_data_path,"processed_extracts/PIS_hdfa_flags.rds"))


wk_distrib <- hdfa_summary %>% group_by(wks_first_presc) %>% count()
wk_distrib <- wk_distrib %>% mutate(wks_first_presc = as.numeric(wks_first_presc))
ggplot( wk_distrib, aes(x=wks_first_presc, y=n)) + geom_line()

wk_distrib_time <- hdfa_summary %>%
  mutate(time_period = case_when(year(est_date_conception) <=2014 ~ "2010-2014", 
                                 year(est_date_conception) >2014 & year(est_date_conception)<2019 ~ "2015-2018",
                                 year(est_date_conception) >=2019 ~ "2019-2023" )) %>%
  group_by(wks_first_presc, time_period) %>% count() %>% mutate(wks_first_presc = as.numeric(wks_first_presc))

wk_distrib <- wk_distrib %>% mutate(wks_first_presc = as.numeric(wks_first_presc))

ggplot( wk_distrib_time, aes(x=wks_first_presc, y=n)) + geom_line(aes(colour=time_period))

####methotrexate####
mtx_old_presc <- read_csv("/conf/FolicAcid/data/extracts/mtx_cohort_201023_presc.csv")%>% clean_names()
mtx_old_disp <- read_csv("/conf/FolicAcid/data/extracts/mtx_cohort_201023_disp.csv")%>% clean_names()
mtx_newPIS_presc <- read_csv("/conf/FolicAcid/data/extracts/mtx_bnf_cohort_newPIS_presc.csv")%>% clean_names()
mtx_newPIS_disp <- read_csv("/conf/FolicAcid/data/extracts/mtx_cohort_newPIS_disp.csv") %>% clean_names()

new_mtx_all <- left_join(mtx_newPIS_presc, mtx_newPIS_disp, by=("claim_form_scan_reference_no"))
old_mtx_all <- left_join(mtx_old_presc, mtx_old_disp , by = c( "pr_form_scan_reference_no"="di_form_scan_reference_no"))

new_mtx_all <-new_mtx_all  %>% rename(dispensed_quantity=  claim_di_number_of_dispensed_items, 
                                      form_scan_reference_no= claim_form_scan_reference_no, 
                                      pr_dcvp_electronic_flag = claim_dcvp_electronic_flag.x, 
                                      di_dcvp_electronic_flag= claim_dcvp_electronic_flag.y,
                                      number_of_prescribed_items=claim_pr_number_of_prescribed_items)

##DVCP electronic flag =N means default date
old_mtx_all <- old_mtx_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                    di_dcvp_electronic_flag=="U" ~1, T~0), 
                                    disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard dispensed too long after prescribed. 
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>%
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                 T~disp_date))%>%
  ##additional rule to deal with unflagged default dates
  mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                 T~date_to_use))

new_mtx_all <- new_mtx_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                      di_dcvp_electronic_flag=="U" ~1, T~0), 
                                      disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>% 
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard dispensed too long after prescribed. 
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                 T~disp_date)) %>%
  ##additional rule to deal with unflagged default dates
  mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                 T~date_to_use))


#renaming
old_mtx_all <- old_mtx_all%>%
  mutate(date_to_use =  as.Date(date_to_use ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(methotrexate_pis = 1) %>%
  select(mother_upi, date_to_use, methotrexate_pis) %>% filter(date_to_use <= as.Date("2023-04-20"))
new_mtx_all <- new_mtx_all %>% 
  mutate(date_to_use =  as.Date(date_to_use ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(methotrexate_pis = 1) %>%
  select(mother_upi, date_to_use, methotrexate_pis)

PIS_mtx_all <- rbind(old_mtx_all, new_mtx_all)


slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)


###join pregs and mtx and just keep ones with a flag
pregs_mtx <- left_join(slipbd1, PIS_mtx_all) %>% filter(methotrexate_pis==1)

###now determine whether the mtx prescription is relevant to this pregnancy.
## from 12 weeks prior to conception date to 12weeks gestation (ie 10wks after conception)
##also flag any in pregnancy including after 12 wks
pregs_mtx  <- pregs_mtx  %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception)) %>%
  mutate(start_window2 = as.Date(est_date_conception) , 
         end_window2 = as.Date(est_date_conception)+ (7*10)) %>%
  mutate(mtx_PIS_conception = case_when(date_to_use >= start_window & date_to_use <= end_window ~1 , T~0), 
         mtx_PIS_to_12_wks = case_when(date_to_use >= start_window2 & date_to_use <= end_window2 ~1 , T~0), 
         mtx_PIS_after_12wks=case_when(date_to_use >=end_window2 & date_to_use <= date_end_pregnancy ~1 , T~0)  )%>%
  mutate(mtx_anytime = case_when(mtx_PIS_conception==1| mtx_PIS_to_12_wks==1 | mtx_PIS_after_12wks==1 ~1 , T~0))
table(pregs_mtx$mtx_anytime)
##filter to remove prescriptiosn not relewvant to pregnancy
pregs_mtx <- pregs_mtx %>% filter(mtx_anytime==1) %>%
  group_by(pregnancy_id)%>%
  summarise(mtx_PIS_conception = max(mtx_PIS_conception), 
  mtx_PIS_to_12_wks = max(mtx_PIS_to_12_wks), 
  mtx_PIS_after_12wks=max(mtx_PIS_after_12wks), 
  mtx_anytime = max(mtx_anytime) ) %>% ungroup() 

saveRDS(pregs_mtx, paste0(folder_data_path, "processed_extracts/methotrextate_PIS.rds"))

rm(mtx_newPIS_disp, mtx_newPIS_presc, mtx_old_disp, mtx_old_presc)

###ASMs####
asm_oldPIS_presc <- read_csv("/conf/FolicAcid/data/extracts/asm_cohort_201023_presc.csv")%>% clean_names()
asm_oldPIS_disp <- read_csv("/conf/FolicAcid/data/extracts/asm_cohort_201023_disp.csv")%>% clean_names()
asm_newPIS_presc <- read_csv("/conf/FolicAcid/data/extracts/asm_cohort_newPIS_presc.csv")%>% clean_names()
asm_newPIS_disp <- read_csv("/conf/FolicAcid/data/extracts/asm_cohort_newPIS_disp.csv") %>% clean_names()


new_asm_all <- left_join(asm_newPIS_presc, asm_newPIS_disp, by=("claim_form_scan_reference_no"))
old_asm_all <- left_join(asm_oldPIS_presc, asm_oldPIS_disp , by = c( "pr_form_scan_reference_no"="di_form_scan_reference_no"))

new_asm_all <-new_asm_all  %>% rename(dispensed_quantity=  claim_di_number_of_dispensed_items, 
                                      form_scan_reference_no= claim_form_scan_reference_no, 
                                      pr_dcvp_electronic_flag = claim_dcvp_electronic_flag.x, 
                                      di_dcvp_electronic_flag= claim_dcvp_electronic_flag.y,
                                      number_of_prescribed_items=claim_pr_number_of_prescribed_items)

##DVCP electronic flag =N means default date
old_asm_all <- old_asm_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                      di_dcvp_electronic_flag=="U" ~1, T~0), 
                                      disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard ispensed too long after prescribed. 
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>%
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                 T~disp_date))%>%
  ##additional rule to deal with unflagged default dates
  mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                 T~date_to_use))

new_asm_all <- new_asm_all %>% mutate(default_disp_date = case_when(di_dcvp_electronic_flag=="N"|
                                                                      di_dcvp_electronic_flag=="U" ~1, T~0), 
                                      disp_date_missing = case_when(is.na(disp_date) ~ 1, T~0)) %>%
  mutate(default_prescr_date = case_when(pr_dcvp_electronic_flag=="N" |pr_dcvp_electronic_flag=="U" ~1, T~0)) %>% 
  mutate(flag_big_gap = case_when(as.Date(disp_date) - as.Date(presc_date) >365 ~ 1, T~0)) %>% 
  filter(flag_big_gap==0)%>% # discard ispensed too long after prescribed. 
  mutate(date_to_use = case_when(default_disp_date==1| is.na(disp_date) ~ presc_date, 
                                 T~disp_date)) %>%
  ##additional rule to deal with unflagged default dates
  mutate(date_to_use = case_when(day(disp_date)==30 | day(disp_date)==31 ~ presc_date,
                                 T~date_to_use))

#table(PIS_FA_all$dispensed_strength, PIS_FA_all$prescribed_strength_uo_m, useNA="always")

#renaming
old_asm_all <- old_asm_all %>%
  mutate(date_to_use =  as.Date(date_to_use ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(asm_pis = 1) %>%
  select(mother_upi, date_to_use, asm_pis) %>% filter(date_to_use <= as.Date("2023-04-20"))
new_asm_all <- new_asm_all %>% 
  mutate(date_to_use =  as.Date(date_to_use ))%>%
  rename(mother_upi = pat_upi_c.x) %>% mutate(asm_pis = 1) %>%
  select(mother_upi, date_to_use, asm_pis)

PIS_asm_all <- rbind(old_asm_all, new_asm_all)

##join pregs

slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)


###join pregs and hdfa and just keep ones with a flag
pregs_asm <- left_join(slipbd1, PIS_asm_all) %>% filter(asm_pis==1)
table(pregs_asm$asm_pis)

###now determine whether the asm prescription is relevant to this pregnancy.
## from 12 weeks prior to conception date to 12weeks gestation (ie 10wks after conception)
##also flag any in pregnancy including after 12 wks
pregs_asm  <- pregs_asm  %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception)) %>%
  mutate(start_window2 = as.Date(est_date_conception) , 
         end_window2 = as.Date(est_date_conception)+ (7*10)) %>%
  mutate(asm_PIS_conception = case_when(date_to_use >= start_window & date_to_use <= end_window ~1 , T~0), 
         asm_PIS_to_12_wks = case_when(date_to_use >= start_window2 & date_to_use <= end_window2 ~1 , T~0), 
         asm_PIS_after_12wks=case_when(date_to_use >=end_window2 & date_to_use <= date_end_pregnancy ~1 , T~0))%>%
  mutate(asm_anytime = 
           case_when(asm_PIS_conception==1| asm_PIS_to_12_wks==1 | asm_PIS_after_12wks==1 ~1 , T~0))%>%
  filter(asm_anytime==1)


pregs_asm <- pregs_asm %>% 
  group_by(pregnancy_id)%>%
  summarise(asm_PIS_conception = max(asm_PIS_conception), 
            asm_PIS_to_12_wks = max(asm_PIS_to_12_wks), 
            asm_PIS_after_12wks=max(asm_PIS_after_12wks), 
            asm_anytime = max(asm_anytime) ) %>% ungroup() 

saveRDS(pregs_asm, paste0(folder_data_path, "processed_extracts/asm_PIS.rds"))

###
