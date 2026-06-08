#06b.propensity score calculation - sensitivity analyses.
library(cobalt)
library(WeightIt)
library(adjustedCurves)
source("00.setup.r")

df<- readRDS(paste0(folder_data_path, "working_data/main_dataset_clean_wts.rds"))

###restrict to those with an indication we could find for hdfa####
df_ind <- df %>% mutate(any_indication = 
                          case_when(obese==1 | ind_ntd==1|asm_flag==1| mtx_flag==1 | 
                                      ind_preexist_diabetes==1|ind_sickle_cell==1|
                                      ind_thalassaemia==1| ind_coeliac==1 ~1,
                                    T~0)) %>%
  filter(any_indication==1) %>% 
  mutate(maternal_age_conception = as.numeric(maternal_age_conception)) 


#
W.out1 <- WeightIt::weightit(hdfa_preg ~
                               year_conception + maternal_age_conception +
                               maternal_simd + mat_ethnicity_broad_groups + 
                               ind_ntd + ind_preexist_diabetes + asm_flag +
                               mtx_flag +obese + ind_coeliac + ind_sickle_cell + 
                               ind_thalassaemia +
                               cancer_history +
                               any_comorb + maternal_smoking, data = df_ind,
                             estimand = "ATE",  # Find the ATE
                             method = "ps", , 
                             stabilize=TRUE)  # Build weights with propensity scores

summary(W.out1)

print(bal.tab(W.out1, stats = c("m", "v"),
              thresholds = c(m = .1)), binary="std")

love.plot(W.out1, thresholds = c(m = .1), var.order = "alphabetical", , binary="std")

df_ind$stabil_weights <- W.out1$weights
df_ind$weights <- NULL

saveRDS(df_ind, paste0(folder_data_path, "working_data/Sens_Any_dataset_clean_wts.rds"))

###ASM sensitivty analysis####
df_asm <- df %>% 
  filter(asm_flag==1) %>% 
  mutate(maternal_age_conception = as.numeric(maternal_age_conception)) 

W.out.a <- WeightIt::weightit(hdfa_preg ~ year_conception + maternal_age_conception +
                                maternal_simd + mat_ethnicity_broad_groups + 
                                ind_ntd + ind_preexist_diabetes +
                                mtx_flag +obese + ind_coeliac + ind_sickle_cell + 
                                ind_thalassaemia +
                                cancer_history +
                                any_comorb + maternal_smoking, data = df_asm,
                              estimand = "ATE",  # Find the ATE
                              method = "ps", 
                              stabilze=TRUE)  # Build weights with propensity scores

df_asm$stabil_weights <- W.out.a$weights
df_asm$weights <- NULL
saveRDS(df_asm, paste0(folder_data_path, "working_data/Sens_ASM_dataset_clean_wts.rds"))



print(bal.tab(W.out.a, stats = c("m", "v"),
              thresholds = c(m = .05)), binary="std")

love.plot(W.out.a, thresholds = c(m = .05), var.order = "alphabetical")



