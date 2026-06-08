#pooled regression sensitivity analysis1####
##ASM indication####
folder_data_path <- "/conf/FolicAcid/data/"

#Data prep####
df<- readRDS(paste0(folder_data_path, "working_data/Sens_ASM_dataset_clean_wts.rds"))
df <- df %>%
  mutate(follow_days = case_when(as.numeric(follow_days)==0 ~1, T~as.numeric(follow_days))) %>%
  mutate(
    follow_years = follow_days / 365.25,
    n_int = ceiling(follow_years)  #1-year intervals
  ) %>%
  mutate(treated = hdfa_preg)
##combine last two years
df <- df %>% mutate(n_int = case_when(n_int==14 ~13, T~n_int))

df <-df %>% select(pregnancy_id,
                   treated,n_int,follow_years,
                   cancer_outcome,hdfa_preg, year_conception, maternal_age_conception,
                   maternal_simd , mat_ethnicity_broad_groups, 
                   ind_ntd , ind_preexist_diabetes , asm_flag,
                   mtx_flag ,obese, ind_coeliac, ind_sickle_cell, 
                   ind_thalassaemia , cancer_history ,
                   any_comorb, maternal_smoking, stabil_weights)


df_long <-  df  %>%
    uncount(n_int, .id = "time_interval", .remove = FALSE) %>%
  mutate(
    tstart = time_interval - 1L,
    event = as.integer(time_interval == n_int & cancer_outcome == 1)
  ) %>%
  mutate(time_interval = time_interval-1)
saveRDS(df_long, paste0(folder_data_path, "working_data/sensitivity_ASM_long_data.rds"))
