#06.propensity score calculation.

library(cobalt)
library(WeightIt)
source("00.setup.r")

df<- readRDS(paste0(folder_data_path, "working_data/main_dataset_clean.rds"))
df <- df %>% mutate(hdfa_preg = case_when(hdfa_preg=="exposed_hdfa" ~1, 
                                          hdfa_preg=="unexposed_hdfa"~0))

dfshortnames <- df %>% 
  mutate(mat_age = as.numeric(maternal_age_conception)) %>% 
  rename(yr_con = year_conception, 
         SIMD = maternal_simd, 
         m_ethnicity = mat_ethnicity_broad_groups, 
         ind_diabetes = ind_preexist_diabetes, 
         smoking = maternal_smoking)

##stabilised weights
W.out.stab <- WeightIt::weightit(hdfa_preg ~
                              year_conception + as.numeric(maternal_age_conception)+
                                year_conception*maternal_simd + mat_ethnicity_broad_groups + 
                              ind_ntd + ind_preexist_diabetes + asm_flag +
                              mtx_flag +year_conception*obese + ind_coeliac + ind_sickle_cell + 
                              ind_thalassaemia +
                              cancer_history +
                              any_comorb +maternal_smoking, data = df,
                            estimand = "ATE",  # Find the ATE
                            method = "ps", 
                            stabilize=TRUE)  # Build weights with propensity scores
summary(W.out.stab)
#check balance
bts <- bal.tab(W.out.stab, stats = c("m", "v"),
              thresholds = c(m = .1), binary= "std")
bts
bal.plot(W.out.stab, var.name = "maternal_simd",
         which = "both",
         type = "histogram")
bal.plot(W.out.stab, var.name = "year_conception",
         which = "both",
         type = "histogram")
bal.plot(W.out.stab, var.name = "mat_ethnicity_broad_groups",
         which = "both",
         type = "histogram")

love.plot(W.out.stab, thresholds = c(m = .1), var.order = "unadjusted", binary= "std")
#plot(W.out)

##add weights to data
df$stabil_weights <- W.out.stab$weights
saveRDS(df, paste0(folder_data_path, "working_data/main_dataset_clean_wts.rds"))
