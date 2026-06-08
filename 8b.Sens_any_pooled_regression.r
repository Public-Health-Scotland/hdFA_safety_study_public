##pooled regression
## need  > 20k memory to run both weighted and uinweighted model full dataset
##works with 30k , possibly dont need quite that much
source("00.setup.r")
library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)

df_long <- readRDS(paste0(folder_data_path, "working_data/sensitivity_any_long_data.rds"))

# The variable death is only '1' at end-of-followup
df_long<-df_long %>%
  group_by(pregnancy_id)%>%
  mutate(
    cancer_overall = max(event)
  ) %>% ungroup()

arrow::write_parquet(df_long,  paste0(folder_data_path, "working_data/sensitivity_any_long_data.parquet"))

# Create 'baseline' - data collected at visit 0
baseline <-df_long %>%
  dplyr::filter(as.numeric(time_interval) == 0)

## Check events per interval

# Weighted models different forms for time -----------------------------
## Form of time,
##try linear
fit_wts <- glm(event ~ 
                        time_interval +  treated,
                      weights = stabil_weights, data = df_long,  family=binomial())
summary(fit_wts)
#AIC: 3578.7
saveRDS(fit_wts, paste0(folder_data_path, "outputs/sens_any_model_lineartime.rds"))

##factor time
#fit_wts_f <- glm(event ~ 
#                 as.factor(time_interval) +  treated,
#               weights =  stabil_weights, data = df_long,  family=binomial())
#summary(fit_wts_f)

#saveRDS(fit_wts_f, paste0(folder_data_path, "outputs/sens_any_model_factortime.rds"))

##splines####
#saveRDS(fit_wts_f, paste0(folder_data_path, "outputs/sens_any_model_factortime.rds"))
fit_wts_spline <- glm(event ~ 
                        time_interval + rcs(time_interval,4)+ treated,
                      weights = stabil_weights,  data = df_long,  family=binomial())#

summary(fit_wts_spline)
#IC: 3577.5
