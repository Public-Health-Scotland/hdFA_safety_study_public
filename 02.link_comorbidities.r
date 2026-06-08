##02.Link sources####
###
source("00.setup.r")
##load data##
slipbd1 <- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))

smr01_comorb_flags<-readRDS(paste0(folder_data_path,"extracts/smr01_comorbs.rds"))%>%ungroup()
smr01_indicator_flags <- readRDS(paste0(folder_data_path,"extracts/smr01_indicators.rds"))%>%ungroup()

smr02_indicator_flags<- readRDS(paste0(folder_data_path,"extracts/smr02_indicators.rds")) %>%ungroup()
smr02_comorb_flags<- readRDS(paste0(folder_data_path,"extracts/smr02_comorbs.rds"))%>%ungroup()
smr02_delivery_indicator_flags <- readRDS(paste0(folder_data_path,"extracts/smr02_indicators_del.rds"))%>%ungroup()

preg_details <-  read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet")) %>% select(pregnancy_id, mother_upi)

##first join all the indicator flags
ind <- left_join(smr02_indicator_flags, smr02_delivery_indicator_flags, by=c("pregnancy_id", "upi_number"))
ind <- left_join(ind, smr01_indicator_flags, by=c("pregnancy_id", "upi_number"))

ind <- ind %>% mutate(flag_ntd = pmax(ntd.x, ntd.y, na.rm=T),
                      flag_coeliac = pmax(coeliac.x, coeliac.y, coeliac, na.rm=T),
                      flag_sickle_cell = pmax(sickle_cell, sickle_cell.x, sickle_cell.y,  na.rm=T),
                      flag_thalassaemia = pmax(thalassaemia, thalassaemia.x, thalassaemia.y,  na.rm=T),
                      flag_preexist_diabetes =
                        pmax(preexist_diabetes, preexist_diabetes.x, preexist_diabetes.y,  na.rm=T)) %>%
  select(upi_number, pregnancy_id, flag_ntd, flag_coeliac, flag_sickle_cell, flag_thalassaemia, flag_preexist_diabetes) %>%
  rename(ind_ntd = flag_ntd, ind_coeliac = flag_coeliac, ind_sickle_cell= flag_sickle_cell,
         ind_thalassaemia = flag_thalassaemia, ind_preexist_diabetes=flag_preexist_diabetes)

##and the comorbidity flags 
comorb <- left_join(smr01_comorb_flags, smr02_comorb_flags, by=c("pregnancy_id", "upi_number"))

comorb <- comorb %>% mutate(vte = pmax(vte.x, vte.y , na.rm=T),
                            hypertension = pmax(hypertension.x, hypertension.y, na.rm=T),
                            demyelinating_NMD = pmax(demyelinating_NMD.x, demyelinating_NMD.y, na.rm=T),
                            haematological = pmax(haematological.x, haematological.y, na.rm=T),
                            thyroid = pmax(thyroid.x, thyroid.y, na.rm=T),
                            asthma = pmax(asthma.x, asthma.y, na.rm=T),
                            GI_dis = pmax(GI_dis.x, GI_dis.y, na.rm=T),
                            liver = pmax(liver.x, liver.y, na.rm=T),
                            Imm_mediated_joint_ct = pmax(Imm_mediated_joint_ct.x, Imm_mediated_joint_ct.y, na.rm=T),
                            kidney_dis = pmax(kidney_dis.x, kidney_dis.y, na.rm=T),
                            dis_pelvic_genital_tract = pmax(dis_pelvic_genital_tract.x, dis_pelvic_genital_tract.y, na.rm=T), 
                            skin = pmax(skin.x, skin.y, na.rm=T),) %>%
  select(upi_number, pregnancy_id, vte, hypertension, demyelinating_NMD, haematological, thyroid, asthma, GI_dis,
         liver, Imm_mediated_joint_ct, kidney_dis, dis_pelvic_genital_tract, skin)
         
##Link SMR flags
df <- left_join(preg_details, ind)
df<- left_join(df, comorb)

##link SMR06 (mothers' history)
data_smr06_mothers <- read_parquet(paste0(folder_data_path, "extracts/temp_smr06_mother.parquet"))

cancer_mothers <- data_smr06_mothers %>% select(upi_number, incidence_date, age_in_years)
preg_dates <- slipbd1 %>% select(pregnancy_id, mother_upi, est_date_conception)

cancer_df <- left_join(preg_dates, cancer_mothers , by = c("mother_upi" = "upi_number"))
cancer_df <- cancer_df %>% 
  mutate(cancer_history = case_when(incidence_date < est_date_conception~1 , T~0)) %>%
  select(pregnancy_id, cancer_history) %>%
  group_by(pregnancy_id) %>%
  summarise(cancer_history = max_(cancer_history)) %>% ungroup()


df <- left_join(df, cancer_df)
#
saveRDS(df, paste0(folder_data_path, "processed_extracts/all_comorbs.rds") )

